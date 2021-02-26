module JSONReader.Util where

import           Data.Char (isSpace)

shorten :: String -> String
shorten = add . takeWhile (not.isSpace) . take 5
  where add s = if length s <= 5 then [] else s ++ "..."

skipSpace :: String -> String
skipSpace = dropWhile isSpace
