module Assignment where

import Data.IORef

nullEnv :: IO Env
nullEnv = newIORef []