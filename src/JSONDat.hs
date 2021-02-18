module JSONDat (JDat(..)) where

data JDat = JNothing
          | JNumber Float
          | JString String
          | JComment String
          | JBool Bool
          | JNull
          | JArray [JDat]
          | JObject [(String, JDat)] deriving (Eq, Ord, Show)
