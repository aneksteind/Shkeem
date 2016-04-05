module ErrorHandler where
import Header
import DataTypes
import Parser

showError :: LispException -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispException where show = showError

trapError action = catchError action (return . show)

extractValue :: ThrowsException a -> a
extractValue (Right val) = val

liftThrows :: ThrowsException a -> IOThrowsException a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsException String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue