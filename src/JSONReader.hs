module JSONReader (jread) where

import           Control.Arrow       (first)
import           Control.Monad.State
import           Data.Char           (isAlpha, isAlphaNum, isSpace)
import           Data.List           (find, isPrefixOf)
import           JSONDat
import           Text.Read           (readsPrec)


jread :: String -> Either String JDat
jread s = let (result, flag) = runState jDatParser (Ok s)
          in case flag of Unknown   -> Left "传入的数据无法解析"
                          Fatal msg -> Left msg
                          Ok []     -> Right result
                          Ok _      -> Left "传入的数据格式错误"

type ErrorMsg = String
type Stream   = String

data ParsingFlag = Unknown
                 | Fatal   ErrorMsg
                 | Ok      Stream   deriving (Show)
type JParsingState = State ParsingFlag
type JState = JParsingState JDat

infixl 2 <|>
(<|>) :: JState -> JState -> JState
p₁ <|> p₂ = whenOk $ \state₀@(Ok _) ->
  let (result, state₁) = runState p₁ state₀
  in case state₁ of
       Unknown -> p₂
       _       -> put state₁ >> return result

constantParser, numberParser, stringParser :: JState
arrayParser, objectParser                  :: JState
jDatParser                                 :: JState

jDatParser = objectParser <|> arrayParser <|>
             stringParser <|> numberParser <|> constantParser

arrayParser = whenOk $ \ (Ok dat) ->
  case dat of '[':rest -> apaux $ skipSpace rest
              _        -> unknown
  where apaux :: String -> JState
        apaux [] = die "存在未闭合的数组"
        apaux (']':r) = put (Ok r) >> return (JArray [])
        apaux s₀      = put (Ok s₀) >> jDatParser >>= \ result ->
          whenOk $ \ (Ok dat) ->
                     case skipSpace dat of
                       s₁@(c:cs) -> let rest = if c == ',' then cs else s₁
                                    in apaux (skipSpace rest) >>= cons result
                       r -> apaux (skipSpace r)
        cons result (JArray jar) = return (JArray $ result:jar)
        cons _       JNothing    = return  JNothing

objectParser = whenOk $ \ (Ok dat) ->
  case dat of '{':rest -> opaux $ skipSpace rest
              _        -> unknown
  where opaux [] = die "存在未闭合的对象"
        opaux ('}':r) = put (Ok r) >> return (JObject [])
        opaux s₀      = put (Ok s₀) >> stringParser >>= \ keyr ->
          nextCharShould (==':')    >> jDatParser   >>= \ valr ->
          whenOk $ \ (Ok dat) ->
                     case skipSpace dat of
                       s₁@(c:cs) | c == ',' ->
                                   opaux (skipSpace cs) >>= cons (jstr keyr, valr)
                       s₁ -> nextCharShould (=='}') >>=
                         const (whenOk . const . return $ JObject [(jstr keyr, valr)])
        cons kv (JObject r) = return (JObject $ kv:r)
        cons _  JNothing    = return JNothing
        nextCharShould :: (Char -> Bool) -> JState
        nextCharShould pred = whenOk $ \ (Ok dat) ->
          case skipSpace dat of
            c:cs | pred c -> put (Ok $ skipSpace cs) >> return JNothing
            _             -> die ("存在语法错误 " ++ shorten dat)



numberParser = whenOk $ \state₀@(Ok dat) ->
  case readFloat dat of
    Nothing             -> unknown
    Just (result, rest) -> if not (null rest) && isAlpha (head rest)
                           then die $ "数字后有非法字母 " ++ shorten rest
                           else put (Ok rest) >> return (JNumber result)
  where readFloat :: String -> Maybe (Float, String)
        readFloat s₀ = case reads s₀ :: [(Float, String)] of
                         []  -> Nothing
                         r:_ -> Just r

stringParser = whenOk $ \state₀@(Ok dat) ->
  case dat of '\"':rest -> spaux rest
              _         -> unknown
  where spaux :: String -> JState
        spaux s = maybe (die "存在未闭合的字符串") str2jstate (extraString s)
        str2jstate :: (String, String) -> JState
        str2jstate (str, rest) = put (Ok rest) >> return (JString str)

constantParser = whenOk $ \state₀@(Ok dat) ->
  case find (\ (k, _) -> k `isPrefixOf` dat) alist of
    Nothing     -> unknown
    Just (k, v) -> case drop (length k) dat of
                     c:cs | isAlpha c || c == '_' ->
                            die $ "存在未知的字面量 " ++ shorten dat
                     dat' -> put (Ok dat') >> return v
  where alist = [ ("true", JBool True), ("false", JBool False), ("null", JNull)]

extraString :: String -> Maybe (String, String)
extraString s₀ = case break (\c -> c == '"' || c == '\\') s₀ of
                  (_, [])        -> Nothing
                  (s₁, '"':r)    -> Just (s₁, r)
                  (s₁, '\\':c:r) -> first ((s₁ ++ ['\\', c]) ++) <$> extraString r
                  (_,  '\\':[])  -> Nothing

shorten :: String -> String
shorten = add . takeWhile (not.isSpace) . take 5
  where add s = if null s then [] else s ++ "..."

skipSpace :: String -> String
skipSpace = dropWhile isSpace

die :: String -> JState
die msg = put (Fatal msg) >> return JNothing

unknown :: JState
unknown = put Unknown >> return JNothing

whenOk :: (ParsingFlag -> JState) -> JState
whenOk f = get >>= \state₀ ->
  case state₀ of
    Ok _ -> f state₀
    err  -> put err >> return JNothing

