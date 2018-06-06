import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric 
import Data.Ratio
import Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float
             | Ratio Rational
             | Complex (Complex Double)


main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

parseExpr :: Parser LispVal
parseExpr = try parseRational
        <|> try parseFloat
        <|> try parseString
        <|> try parseNumber
        <|> try parseBoolTrue
        <|> try parseCharacterLiteral
        <|> parseAtom

parseFloat :: Parser LispVal
parseFloat = do 
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (fst.head$readFloat (x++"."++y))

specialChars :: Parser Char
specialChars = do
    char '\\'
    switch <- oneOf "nrt\\"
    return $ case switch of
        'r'-> '\r'
        'n'-> '\n'
        't'-> '\t'
        '\\' -> '\\'

innerQuote :: Parser Char
innerQuote = do
    char '\\'
    answer <- oneOf "\\\""
    return answer

parseRational :: Parser LispVal
parseRational = do 
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))


parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (specialChars <|> innerQuote <|> noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parseBoolTrue :: Parser LispVal
parseBoolTrue = do
    char '#'
    dupa <- letter
    return $ case dupa of
        'f' -> Bool False
        't' -> Bool True



parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)


parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseDecimal :: Parser LispVal
parseDecimal = parseDecimal1 <|> parseDecimal2

parseComplex :: Parser LispVal
parseComplex = do 
        x <- (try parseFloat <|> parseDecimal)
        char '+' 
        y <- (try parseFloat <|> parseDecimal)
        char 'i' 
        return $ Complex (toDouble x :+ toDouble y)

parseCharacterLiteral :: Parser LispVal
parseCharacterLiteral = do
    char '#'
    char '\\'
    value <- try (string "newline" <|> string "space") 
        <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

    
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
float2dig x = fst $ readFloat x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs


