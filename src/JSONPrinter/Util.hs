module JSONPrinter.Util where

import           JSONDat

isSimpleJData :: JDat -> Bool
-- ^ 数字，字符串和常量为简单值
isSimpleJData (JNumber _) = True
isSimpleJData (JString _) = True
isSimpleJData (JBool   _) = True
isSimpleJData (JNull   _) = True
isSimpleJData _           = False
