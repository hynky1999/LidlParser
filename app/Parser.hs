module Parser where

import           Control.Exception              ( SomeException
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

import  Expressions


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
execParser :: Parser a -> String -> Either ParseError a
execParser p = snd . runState (runParser p)


finalStateParser :: Parser a -> String -> String
finalStateParser p = fst . runState (runParser p)

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


run :: Parser a -> String -> Either ParseError a
run p s = snd $ runState (runParser go) s
  where
    go = do -- do-notace pro 'Parser a'
        result <- p
        parseEof
        return result

char :: Char -> Parser Char
char c = satisfy [c] (== c)

parseAhoj :: Parser String
parseAhoj = do
    _ <- char 'a'
    _ <- char 'h'
    _ <- char 'o'
    _ <- char 'j'
    return "ahoj"


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
number = read <$> many1 (satisfy "parseDigit" isDigit)


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



parseBinaryOp
    :: (Expression -> Expression -> a)
    -> Parser String
    -> Parser Expression
    -> Parser Expression
    -> Parser a
parseBinaryOp trans parserChar parserExp parserExp2 =
    trans <$> parserExp <*> (parserChar >> parserExp2)

parseUnaryOp
    :: (Expression -> a) -> Parser String -> Parser Expression -> Parser a
parseUnaryOp trans parserChar parserExp = trans <$> (parserChar >> parserExp)







parseRelOp :: Parser Expression
parseRelOp = choice
    "parseRelOp"
    [ parseBinaryOp (RelExpression Eq) (symbol "==") parseAddOp parseAddOp
    , parseAddOp
    ]


parseAddOp :: Parser Expression
parseAddOp = choice
    "parserNumOp"
    [ parseBinaryOp (NumExpression Add) (symbol "+") parseMulOp parseAddOp
    , parseMulOp
    ]

parseMulOp :: Parser Expression
parseMulOp = choice
    "parseMulOp"
    [ parseBinaryOp (NumExpression Mul) (symbol "*") parseFactor parseMulOp -- 1*2
    , parseFactor
    ]


parseId = many1 $ satisfy "parseId" isAlphaNum
parseVar = VarExpression <$> parseId

braces :: Parser a -> Parser a
braces = between (symbol "(") (symbol ")")

parseExprList = braces $ parseExpression `sepBy` symbol ","
parseFunctionCall = FunctionCall <$> parseId <*> parseExprList


parseFactor :: Parser Expression
parseFactor = choice
    "functor"
    [ FunctionCallExpression <$> parseFunctionCall -- f(x,y)
    , ValueExpression <$> IntValue <$> number -- 1234
    , braces parseExpression --(expression)
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
