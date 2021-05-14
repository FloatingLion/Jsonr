
module JSONConfig.Util where

import           Control.Monad.State
import           Data.Char           (isSpace)

skipSpace :: State String ()
skipSpace = get >>= \s ->
  put (dropWhile isSpace s)

nextCharIs :: String -> Char -> Bool
nextCharIs c (c:_) = True
nextCharIs _    _  = False


