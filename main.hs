{-# LANGUAGE ExistentialQuantification #-}

module Main where
    
import Parser
import Evaluator
import Header
import ErrorHandler
import REPL
import Control.Monad
import System.Environment
import Control.Monad.Except
import Control.Monad.Trans.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO



main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"



