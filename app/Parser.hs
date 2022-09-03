{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import           Data.Char                      ( isAlphaNum
                                                , isDigit
                                                , isSpace
                                                )
import           State

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

newtype Parser a = Parser {runParser :: StateT String (ExceptT ParseError DummyMonad) a}
  deriving newtype (Functor, Applicative, Monad) -- magie, StateT ... už je monáda, tak to jen práskneme Haskellu

liftStateOp :: StateT String (ExceptT ParseError DummyMonad) a -> Parser a
liftStateOp = Parser

getParser :: Parser String
getParser = liftStateOp get

setParser :: String -> Parser ()
setParser store = Parser $ set store

modifyParser :: (String -> String) -> Parser ()
modifyParser f = liftStateOp $ modify f

raiseError :: ParseError -> Parser a
raiseError err = Parser $ StateT $ \_ -> ExceptT $ return $ Left err


parseEof :: Parser ()
parseEof = do
    input <- getParser
    case input of
        []      -> return ()
        (c : _) -> raiseError $ ParseError "end of file" [c]

parseAny :: Parser Char
parseAny = do
    input <- getParser
    case input of
        []       -> raiseError $ ParseError "any character" "end of file"
        (c : cs) -> do
            _ <- setParser cs
            return c


(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ StateT $ \s -> ExceptT $ do
    a <- runExceptT $ runStateT p1 s
    case a of
        Left  _ -> runExceptT $ runStateT p2 s
        Right x -> return $ Right x


satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description p = do -- do-notace pro 'Parser Char'
    c <- parseAny
    if p c then return c else raiseError $ ParseError description [c]


parse :: Parser a -> String -> Either ParseError a
parse p s = runDummyMonad $ runExceptT $ do
    result <- runStateT (runParser go) s
    return $ snd result

  where
    go = do
        result <- p
        parseEof
        return result

parseDebug :: Parser a -> String -> Either ParseError String
parseDebug p s = runDummyMonad $ runExceptT $ do
    result <- runStateT (runParser p) s
    return $ fst result

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


parseLeftAssoc :: Parser t -> Parser (t -> t -> t) -> Parser t
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
between l r p = do
    _      <- l
    result <- p
    _      <- r
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
choice desc = foldr (<|>) noMatch
    where noMatch = raiseError $ ParseError desc "no match"



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


parseId :: Parser [Char]
parseId = removeSucceddingSpaces (many1 $ satisfy "parseId" isAlphaNum)
parseVar :: Parser Expression
parseVar = VarExpression <$> parseId

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseExprList :: Parser [Expression]
parseExprList = parens $ parseExpression `sepBy` symbol ","
parseFunctionCall :: Parser FunctionCall
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
parseSigned expr = choice
    "signedExpression"
    [ parseUnaryOp (UnaryExpression Neg) (symbol "-") expr
    , parseUnaryOp (UnaryExpression Pos) (symbol "+") expr
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
    return $ map (\id' -> (id', type')) ids

parseVarDeclaration :: Parser [Statement]
parseVarDeclaration = do
    _ <- symbol "var"
    typeDefsAsStatement <$> parseIdListWithType


typeDefsAsStatement :: [(Id, Type)] -> [Statement]
typeDefsAsStatement = map (uncurry DefVar)

parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = do
    _    <- symbol "function"
    name <- parseId
    args <-
        concat <$> parens (parseIdListWithType `sepBy` symbol ";") <|> return []
    _       <- symbol ":"
    retType <- parseType
    body    <- parseBlock
    return $ DefFc name (Func args body name retType)




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
