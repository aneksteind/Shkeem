module REPL where

import Parser
import Evaluator
import Header
import ErrorHandler
import Control.Monad
import System.Environment
import Control.Monad.Except
import Control.Monad.Trans.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

readExpr :: String -> ThrowsException LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val


