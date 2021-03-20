module JSONPrinter.Compress (jnPrint) where

import           Data.List (intercalate)
import           JSONDat

jnPrint :: JNDat -> String
-- ^ 压缩格式输出，所有 'JDat' 将以最紧凑的格式转化为字符串。
jnPrint (JNScalar _ a _) = a
jnPrint (JNArray _ a _) = "["++ internel ++ "]"
  where internel = intercalate "," $ map jnPrint a
jnPrint (JNObject _ a _) = "{" ++ internel ++ "}"
  where internel = intercalate "," $ map jterm a
        jterm (k, v) = jnPrint k ++ ":" ++ jnPrint v
