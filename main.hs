{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Parser
import Evaluator
import Header
import ErrorHandler
import Control.Monad
import System.Environment
import Control.Monad.Except
import Control.Monad.Trans.Except
import Text.ParserCombinators.Parsec hiding (spaces)



main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsException LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val