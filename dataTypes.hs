{-# LANGUAGE ExistentialQuantification #-}

module DataTypes where
import Header

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
             | PrimitiveFunc ([LispVal] -> ThrowsException LispVal)
             | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
             


data LispException = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsException a)

type ThrowsException = Either LispException

type Env = IORef [(String, IORef LispVal)]

type IOThrowsException = ExceptT LispException IO
