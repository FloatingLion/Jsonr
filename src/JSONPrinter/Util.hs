module JSONPrinter.Util where

import           JSONDat

isSimpleJNDat :: JNDat -> Bool
isSimpleJNDat (JNScalar _ _ _) = True
isSimpleJNDat _                = False
