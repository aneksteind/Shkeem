module ErrorHandler where
import Header
import Parser
import Control.Monad.Except
import Control.Monad.Trans.Except

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