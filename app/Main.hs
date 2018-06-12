import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric 
import Data.Ratio
import Data.Complex
import Data.Array

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
            | Vector (Array Int LispVal)


main :: IO ()
main = getArgs >>= print . eval . readExpr . head  -- 1. take first value 2. read 3. evalueate 4. print

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right value ->  value

parseExpr :: Parser LispVal
parseExpr = try parseRational
        <|> try parseFloat
        <|> try parseString
        <|> try parseNumber
        <|> try parseBoolTrue
        <|> try parseCharacterLiteral
        <|> try (do 
                string "#("
                x <- parseVector
                char ')'
                return x)
        <|> try parseQuoted
        <|> try parseQuasiQuoted
        <|> try parseUnQuote
        <|> do
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x
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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
     char '`'
     x <- parseExpr
     return $ List [Atom "quasiquote", x]
     
parseUnQuote :: Parser LispVal
parseUnQuote = do
     char ','
     x <- parseExpr
     return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do 
    arrayValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal  -- map showVal applies  showVal to every element of list, returns a list of strings. unwords joinst elements of list with spaces

instance Show LispVal where show = showVal  -- so that we can use showVal like a standard show method

eval :: LispVal -> LispVal
eval val@(String _) = val  -- _ = don't care, match everything
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = 
    apply func $ map eval args  -- function evaluation -- function and list of arguments

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", unaryOp isBoolean),
              ("symbol?" , unaryOp isSymbol) ,
              ("string?" , unaryOp isString) ,
              ("number?" , unaryOp isNumber) ,
              ("list?" , unaryOp isList),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

isSymbol :: LispVal -> LispVal
isSymbol (Atom _)   = Bool True
isSymbol _          = Bool False
isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False
isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False
isBoolean :: LispVal -> LispVal
isBoolean   (Bool _)   = Bool True
isBoolean   _          = Bool False
isList :: LispVal -> LispVal
isList   (List _)   = Bool True
isList   (DottedList _ _) = Bool True
isList   _          = Bool False
-- isBoolean :: [LispVal] -> LispVal
-- isBoolean = "#t"

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- the following lines commented out because of chapter 3 ex 2
--unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
--                           if null parsed 
--                              then 0
--                              else fst $ parsed !! 0
--unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives  -- maybe returns false if lookup fails

