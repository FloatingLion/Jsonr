module JSONReader (jread) where

import           Control.Monad.State
import           Data.Char           (isAlpha)
import           JSONDat
import           Text.Read           (readsPrec)

jread :: String -> JDat
jread = undefined

data ParsingFlag = Unknown
                 | Fatal String
                 | Ok String
type JState = State ParsingFlag JDat

infixl 6 <+>
(<+>) :: JState -> JState -> JState
p₁ <+> p₂ = get >>= tryParse
  where tryParse :: ParsingFlag -> JState
        tryParse state₀@(Ok _) = let (result, state₁) = runState p₁ state₀
                                 in case state₁ of
                                      Unknown -> p₂
                                      _       -> put state₁ >> return result
        tryParse err           = put err >> return JNothing

numberParser :: JState

numberParser = get >>= \state₀@(Ok dat) ->
  case readFloat dat of
    Nothing -> put Unknown >> return JNothing
    Just (result, rest) -> if not (null rest) && isAlpha (head rest)
                           then die $ "数字后有非法字母：" ++ shorten rest
                           else put (Ok rest) >> return (JNumber result)
  where readFloat :: String -> Maybe (Float, String)
        readFloat = undefined



shorten :: String -> String
shorten s = take 5 s ++ "..."

die :: String -> JState
die msg = put (Fatal msg) >> return JNothing
