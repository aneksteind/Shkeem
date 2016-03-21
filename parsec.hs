import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio
import Data.Array

data LispVal = Nil
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Vector (Array Int LispVal)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\"\\"
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

parseNumber :: Parser LispVal
parseNumber = parseFloat <|> parseRatio <|> parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = parseDecimal1 <|> parseDecimal2

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

parseFloat :: Parser LispVal
parseFloat = do
              x <- many1 digit
              char '.'
              y <- many1 digit
              (return . Float . fst . head . readFloat) (x++"."++y)


oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseBool :: Parser LispVal
parseBool = do
            char '#'
            (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
 try $ string "#\\"
 value <- try (string "newline" <|> string "space") 
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
 return $ Character $ case value of
  "space" -> ' '
  "newline" -> '\n'
  otherwise -> (value !! 0)

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))


--parseList :: Parser LispVal
--parseList = (sepBy parseExpr spaces) >>= (return . List)

parseDottedList :: Parser LispVal
parseDottedList = do
                    hed <- endBy parseExpr spaces
                    tale <- char '.' >> spaces >> parseExpr
                    return $ DottedList hed tale

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
                string "#("
                x <- parseVectorHelper
                char ')'
                return x

parseVectorHelper :: Parser LispVal
parseVectorHelper = do 
  arrayValues <- sepBy parseExpr spaces
  return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)

parseList :: Parser LispVal
parseList = between beg end parseList1
           where beg = (char '(' >> skipMany space)
                 end = (skipMany space >> char ')')

parseList1 :: Parser LispVal
parseList1 = do list <- sepEndBy parseExpr spaces
                datum <- option Nil (char '.' >> spaces >> parseExpr)
                return $ case datum of
                   Nil -> List list
                   val  -> DottedList list val

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> try parseQuasiQuoted
         <|> try parseUnQuote
         <|> parseVector
         <|> try parseList

--TODO: SHOW THE OTHER TYPES OF LISPVAL'S
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"