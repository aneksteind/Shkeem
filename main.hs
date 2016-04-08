{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Header
import DataTypes
import IO
import Primitives
import ErrorHandler
import Assignment
import Evaluator


main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
