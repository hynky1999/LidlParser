module Parser where

import           Control.Exception              ( AssertionFailed
                                                    ( AssertionFailed
                                                    )
                                                , SomeException
                                                , catch
                                                , evaluate
                                                )
import           Data.Char                      ( isAlphaNum
                                                , isDigit
                                                , isSpace
                                                )
import           Prelude                 hiding ( minimum )
import           State
import           System.IO                      ( hSetEncoding
                                                , stdout
                                                , utf8
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )

import           Expressions


data ParseError = ParseError
    { errorExpected :: String
    , errorFound    :: String
    }
    deriving Eq

-- | Nastavíme 'ParseError'u nějaké rozumné vypisování :)
instance Show ParseError where
    show err =
        "expected: " <> errorExpected err <> ", but found: " <> errorFound err

newtype Parser a = Parser {runParser :: State String (Either ParseError a)}


parseEof :: Parser ()
parseEof = Parser $ do
    input <- get
    case input of
        []      -> return $ Right ()
        (c : _) -> return $ Left $ ParseError "end of file" [c]

parseAny :: Parser Char
parseAny = Parser $ do -- pracujeme v 'State' monádě, vnitřek má typ 'State String (Either ParseError Char)'
    input <- get
    case input of
        []       -> return $ Left expectedCharError
        (c : cs) -> do
            set cs
            return $ Right c


expectedCharError :: ParseError
expectedCharError = ParseError "any character" "end of file"



mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f (Parser p) = Parser $ do -- do-notace je pro 'State String' monádu
    result <- p
    case result of
        Left  err -> return $ Left err
        Right a   -> return $ Right $ f a


instance Functor Parser where
    fmap = mapParser

returnParser :: a -> Parser a
returnParser x = Parser $ do
    return $ Right x

andThenParser :: Parser a -> (a -> Parser b) -> Parser b
andThenParser (Parser p) f = Parser $ do
    result <- p
    case result of
        Right x   -> runParser (f x)
        Left  err -> return $ Left err


instance Monad Parser where
    return = returnParser
    (>>=)  = andThenParser

instance Applicative Parser where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x



(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ do
    backup  <- get
    result1 <- p1
    case result1 of
        Left err -> do
            set backup
            x <- p2
            return x
        Right x -> return $ Right x




-- | Pomocná funkce, která načte tři písmenka.
parseThreeLetters :: Parser String
parseThreeLetters = do
    x <- parseAny
    y <- parseAny
    z <- parseAny
    return [x, y, z]

parseError :: String -> String -> Parser a
parseError expected found = Parser $ return $ Left $ ParseError expected found


satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description p = do -- do-notace pro 'Parser Char'
    c <- parseAny
    if p c then return c else parseError description [c]


parse :: Parser a -> String -> Either ParseError a
parse p s = snd $ runState (runParser go) s
  where
    go = do -- do-notace pro 'Parser a'
        result <- p
        parseEof
        return result

char :: Char -> Parser Char
char c = satisfy [c] (== c)

space :: Parser Char
space = satisfy "space" isSpace

digit :: Parser Char
digit = satisfy "digit" isDigit

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do -- tady je do-notace pro 'Parser [a]'
    first <- p
    rest  <- many p
    return (first : rest)

string :: String -> Parser String
string s = mapM char s


number :: Parser Int
number = read <$> removeSucceddingSpaces (many1 (satisfy "parseDigit" isDigit))

optional :: Parser a -> Parser (Maybe a)
optional x = (Just <$> x) <|> return Nothing


parseLeftAssoc p op = do
    x <- p
    process x
  where
    process x = do
        maybef <- optional op -- parses or returns Nothing on fail
        case maybef of
            Nothing -> return x
            Just f  -> do
                y <- p
                process (f x y)

spaces :: Parser String
spaces = many space

symbol :: String -> Parser String
symbol s = do
    result <- string s
    _      <- spaces
    return result

between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
    _      <- left
    result <- p
    _      <- right
    return result

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")


parseNull :: Parser ()
parseNull = do
    _ <- symbol "null"
    return ()

parseQuotedString :: Parser String
parseQuotedString = between (symbol "\"")
                            (symbol "\"")
                            (many parseNonquoteChar)
    where parseNonquoteChar = satisfy "parseNonquoteChar" (\c -> c /= '"')

sepBy, sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy p s = sepBy1 p s <|> return []
sepBy1 p s = do
    first <- p
    rest  <- many (s >> p) -- stručněji jako 'many (s >> p)'
    return (first : rest)

parseListOf :: Parser a -> Parser [a]
parseListOf p = brackets $ p `sepBy` symbol ","

choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch where noMatch = parseError desc "no match"



------------------------------------------
parseUnaryOp
    :: (Expression -> a) -> Parser String -> Parser Expression -> Parser a
parseUnaryOp trans parserChar parserExp = trans <$> (parserChar >> parserExp)


parseRelOp :: Parser Expression
parseRelOp =
    parseLeftAssoc parseAddOp (symbol "==" >> return (RelExpression Eq))


parseAddOp :: Parser Expression
parseAddOp = parseLeftAssoc
    parseMulOp
    (choice
        "additive / substract"
        [ symbol "+" >> return (NumExpression Add)
        , symbol "-" >> return (NumExpression Sub)
        ]
    )

parseMulOp :: Parser Expression
parseMulOp = parseLeftAssoc
    parseFactor
    (choice
        "multiple / divide"
        [ symbol "*" >> return (NumExpression Mul)
        , symbol "/" >> return (NumExpression Div)
        ]
    )


removeSucceddingSpaces :: Parser a -> Parser a
removeSucceddingSpaces p = do
    x <- p
    _ <- spaces
    return x


parseId = removeSucceddingSpaces (many1 $ satisfy "parseId" isAlphaNum)
parseVar = VarExpression <$> parseId

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseExprList = parens $ parseExpression `sepBy` symbol ","
parseFunctionCall = FunctionCall <$> parseId <*> parseExprList


parseFactor :: Parser Expression
parseFactor = choice
    "functor"
    [ FunctionCallExpression <$> parseFunctionCall -- f(x,y)
    , ValueExpression <$> IntValue <$> number -- 1234
    , parens parseExpression --(expression)
    , parseSigned parseFactor -- +x, -x
    , parseVar -- x
    ]


parseSigned :: Parser Expression -> Parser Expression
parseSigned exp = choice
    "signedExpression"
    [ parseUnaryOp (UnaryExpression Neg) (symbol "-") exp
    , parseUnaryOp (UnaryExpression Pos) (symbol "+") exp
    ]

parseExpression :: Parser Expression
parseExpression = parseRelOp

---------------------------------
-- Statements
parseStatement :: Parser Statement
parseStatement = choice
    "parseStatment"
    [ Assign <$> parseId <*> (symbol ":=" >> parseExpression)
    , FunctionCallStmt <$> parseFunctionCall
    , parseIfStatement
    , parseWhileStatement
    ]


parseCompoundStatement :: Parser Block
parseCompoundStatement = do
    _     <- symbol "begin"
    stmts <- many $ parseInfront parseStatement (symbol ";")
    _     <- symbol "end"
    return stmts


parseSimpleOrCompoundStatement :: Parser Block
parseSimpleOrCompoundStatement =
    parseCompoundStatement <|> ((: []) <$> parseStatement)


parseIfStatement :: Parser Statement
parseIfStatement = do
    _        <- symbol "if"
    cond     <- parseExpression
    _        <- symbol "then"
    thenStmt <- parseSimpleOrCompoundStatement
    _        <- symbol "else"
    elseStmt <- optional parseSimpleOrCompoundStatement
    case elseStmt of
        Nothing -> return $ If cond thenStmt []
        Just st -> return $ If cond thenStmt st

parseWhileStatement :: Parser Statement
parseWhileStatement = do
    _    <- symbol "while"
    cond <- parseExpression
    _    <- symbol "do"
    While cond <$> parseSimpleOrCompoundStatement



parseType :: Parser Type
parseType = choice
    "parseType"
    [symbol "integer" >> return IntType, symbol "boolean" >> return BoolType]

parseIdListWithType :: Parser [(Id, Type)]
parseIdListWithType = do
    ids   <- parseId `sepBy1` symbol ","
    _     <- symbol ":"
    type' <- parseType
    return $ map (\id -> (id, type')) ids

parseVarDeclaration :: Parser [Statement]
parseVarDeclaration = do
    _ <- symbol "var"
    typeDefsAsStatement <$> parseIdListWithType


typeDefsAsStatement :: [(Id, Type)] -> [Statement]
typeDefsAsStatement = map (uncurry Define)

parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = do
    _    <- symbol "function"
    name <- parseId
    args <-
        concat <$> parens (parseIdListWithType `sepBy` symbol ";") <|> return []
    _       <- symbol ":"
    retType <- parseType
    body    <- parseBlock
    return $ FunctionDef name (Func args body name retType)




parseBlock :: Parser Block
parseBlock = do
    vars <- concat <$> many (parseInfront parseVarDeclaration (symbol ";"))
    fcs  <- many $ parseInfront parseFunctionDeclaration (symbol ";")
    body <- parseCompoundStatement
    return $ vars ++ fcs ++ body


parseProgram :: Parser Program
parseProgram = do
    _     <- symbol "program"
    name  <- parseInfront parseId (symbol ";")
    block <- parseBlock
    _     <- symbol "."
    return $ Program name block




parseInfront :: Parser a -> Parser b -> Parser a
parseInfront p1 p2 = do
    x <- p1
    _ <- p2
    return x
