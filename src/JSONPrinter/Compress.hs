module JSONPrinter.Compress (jprint) where

import           Data.List  (intercalate)
import           JSONDat    (JAtom (..), JDat (..))
import           JSONReader (jread)

jprint :: JDat -> String
jprint (JNull _) = "null"
jprint (JBool a) = if jval a then "true" else "false"
jprint (JNumber a) = show $ jval a
jprint (JString a) = show $ jval a
jprint (JArray a) = "["++ internel ++ "]"
  where internel = intercalate "," $ map jprint (jval a)
jprint (JObject a) = "{" ++ internel ++ "}"
  where internel = intercalate "," $ map jterm (jval a)
        jterm (k, v) = show (jval k) ++ ":" ++ jprint v
