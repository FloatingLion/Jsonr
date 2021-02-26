module JSONReader.Util where

import           Data.Char (isSpace)

shorten :: String -> String
shorten s₀ = add $ takeWhile (not.isSpace) $ take 5 s₀
  where add s = if s == s₀ then s else s ++ "..."

skipSpace :: String -> String
skipSpace = dropWhile isSpace
