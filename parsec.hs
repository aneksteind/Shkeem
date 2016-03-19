import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
escapedChars = do char '\\' -- a backslash
                  x <- oneOf "\\\"nrt" -- either backslash or doublequote
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
parseNumber = many1 digit >>= (\x -> (return . Number . read) x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber