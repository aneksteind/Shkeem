{-# LANGUAGE ExistentialQuantification #-}

module Header where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


import Data.Array
import Data.Complex



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
             | Vector (Array Int LispVal) deriving (Eq)

data LispException = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsException a)

type ThrowsException = Either LispException