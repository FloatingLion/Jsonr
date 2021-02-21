module JSONReader (jread) where

import           Control.Arrow       (first, second)
import           Control.Monad.State
import           Data.Char           (isAlpha, isAlphaNum, isSpace)
import           Data.List           (find, isPrefixOf)
import           JSONDat
import           Text.Read           (readsPrec)


jread :: String -> Either String JDat
jread s = let (result, flag) = runState jValueParser (Ok s)
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
jValueParser                               :: JState


jValueParser = objectParser <|> arrayParser  <|>
               stringParser <|> numberParser <|> constantParser

arrayParser = whenOk $ \ (Ok dat) ->
  case dat of '[':rest -> apaux $ skipSpace rest
              _        -> unknown
  where apaux :: String -> JState
        apaux [] = die "存在未闭合的数组"
        apaux (']':r) = put (Ok r) >> return (JArray [])
        apaux s₀      = put (Ok s₀) >> jValueParser >>= \ result ->
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
          nextCharShould (==':')    >> jValueParser   >>= \ valr ->
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



numberParser = withCheckComment $ \state₀@(Ok dat) ->
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

skipSpacer :: JParsingState ()
skipSpacer = whenOk $ \ (Ok dat) ->
  if isSpace (head dat)
  then put (Ok $ skipSpace dat)
  else return ()

die :: ErrorMsg -> JState
die msg = put (Fatal msg) >> return JNothing

unknown :: JState
unknown = put Unknown >> return JNothing

whenOk :: (ParsingFlag -> JState) -> JState
whenOk f = get >>= \state₀ ->
  case state₀ of
    Ok _ -> f state₀
    err  -> put err >> return JNothing


-- | 提取注释，不能用于解析器的组合当发现多行注释未闭合时会设置 Fatal
-- 状态。不会返回 Unknown。不会在内部调用 skipSpace。
commentParser :: JParsingState JComment
commentParser = whenOk $ \ (Ok dat) ->
  commit $ case dat of
             '/':'/':r -> inlineParser r
             '/':'*':r -> multilineParser r
             _         -> (Ok dat, JNoComment)
  where inlineParser []          = (Ok [], JInlineComment [])
        inlineParser ('\n':rest) = (Ok rest, JInlineComment [])
        inlineParser (c:rest)    = consc c `second` inlineParser rest
        multilineParser [] = (Fatal "存在未闭合的多行注释", JNoComment)
        multilineParser ('*':'/':rest) = (Ok rest, JMultilineComment [])
        multilineParser (c:rest)       = consc c `second` multilineParser rest
        consc c (JInlineComment ic)    = JInlineComment (c:ic)
        consc c (JMultilineComment mc) = JMultilineComment (c:mc)
        consc _                        = id
        commit (s, a) = put s >> return a

-- | 接受一个解析函数，在运行这个函数之前和之后检查表达式周围的注释。
-- 约定函数总是接受一个 'Ok' 状态，返回一个 'JState'，这个 'JState' 不
-- 负责解析（周围的）注释，即使解析了也会被覆盖掉。如果注释解析失败
-- （即 'Fatal'），函数会忽略参数函数的解析结果直接返回错误信息。注释
-- 的具体内容参见 'JSONDat.JComment'
--
-- 这个函数不会调用仅会在注释周围调用 'skipSpace'，不会在调用结束时，
-- 也就是尾部注释之后调用 'skipSpace'
withCheckComment :: (ParsingFlag -> JState) -> JState
-- withCheckComment f = whenOk $ \ state₀@(Ok dat) ->
--   let (state₁, preCmt) = parseComment dat
--       (result, state₂) = runState (whenOk f) state₁
--   in case state₂ of
--     Unknown   -> unknown
--     Fatal msg -> die msg
--     Ok rest -> let (state₃, postCmt) = parseComment rest
--                in case state₃ of
--                     Fatal msg' -> die msg'
--                     Ok _ -> put state₃ >> return (annotate result preCmt postCmt)
withCheckComment f = commentParser  >>=           \ cmt₁   ->
  whenOk $ \ _ -> skipSpacer >> get >>= f     >>= \ result ->
  whenOk $ \ _ -> skipSpacer >> commentParser >>= \ cmt₂   ->
  whenOk $ \ _ -> return (annotate result cmt₁ cmt₂)
