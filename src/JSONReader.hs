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

-- | 提取注释，不能用于解析器的组合当发现多行注释未闭合时会设置 Fatal
-- 状态。不会返回 Unknown。不会在内部调用 skipSpace。
commentParser :: JParsingState [JComment]
commentParser = maybeOk [] $ \ state₀@(Ok dat) ->
  case dat of
    '/':'/':r -> makComment (inlineParser r) >>= \ cmt ->
      first (JInlineComment cmt:) `mapState` commentParser
    '/':'*':r -> makComment (multilineParser r) >>= \ cmt ->
      first (JMultilineComment cmt:) `mapState` commentParser
    _         -> return []
  where inlineParser, multilineParser :: String -> (String, ParsingFlag)
        makComment :: (String, ParsingFlag) -> JParsingState String
        inlineParser []          = ([], Ok [])
        inlineParser ('\n':rest) = ([], Ok rest)
        inlineParser (c:rest)    = (c:) `first` inlineParser rest
        multilineParser []             = ([], Fatal "存在未闭合的多行注释")
        multilineParser ('*':'/':rest) = ([], Ok rest)
        multilineParser (c:rest)       = (c:) `first` multilineParser rest
        makComment (c, Ok s) = put (Ok $ skipSpace s) >> return c
        makComment (_, err)  = put err >> return []

-- | 接受一个解析函数，在运行这个函数之前和之后检查表达式周围的注释。
-- 约定函数总是接受一个 'Ok' 状态，返回一个 'JState'，这个 'JState' 不
-- 负责解析（周围的）注释，即使解析了也会被覆盖掉。如果注释解析失败
-- （即 'Fatal'），函数会忽略参数函数的解析结果直接返回错误信息。注释
-- 的具体内容参见 'JSONDat.JComment'
--
-- 这个函数不会调用仅会在注释周围调用 'skipSpace'，不会在调用结束时，
-- 也就是尾部注释之后调用 'skipSpace'
withCheckComment :: (ParsingFlag -> JState) -> JState
withCheckComment f = commentParser  >>=           \ cmt₁   ->
  whenOk $ \ _ -> skipSpacer >> get >>= f     >>= \ result ->
  whenOk $ \ _ -> skipSpacer >> commentParser >>= \ cmt₂   ->
  whenOk $ \ _ -> return (annotate result cmt₁ cmt₂)


arrayParser = withCheckComment $ \ (Ok dat) ->
  case dat of '[':rest -> put (Ok rest) >> (mapState $ first plainJArray) apaux
              _        -> unknown
  where apaux    :: JParsingState [JDat]
        extractE :: ParsingFlag -> JParsingState [JDat]
        apaux = maybeOk [] extractE
        extractE (Ok [])         = put (Fatal "存在未闭合的数组") >> return []
        extractE (Ok (']':rest)) = put (Ok rest)                  >> return []
        extractE _  = jValueParser >>= \ elt ->
          ignoreComma ']' >> apaux >>= \ rest ->
          return $ elt:rest

objectParser = withCheckComment $ \ (Ok dat) ->
  case dat of '{':rest -> put (Ok rest) >> (mapState $ first plainJObject) opaux
              _        -> unknown
  where extractO :: ParsingFlag -> JParsingState [(JAtom String, JDat)]
        opaux    :: JParsingState [(JAtom String, JDat)]
        opaux = maybeOk [] extractO
        extractO (Ok [])        = put (Fatal "存在未闭合的对象") >> return []
        extractO (Ok ('}':r))   = put (Ok r)                     >> return []
        extractO _              =   stringParser >>= \ keyr  ->
          nextCharShould (==':') >> jValueParser >>= \ valr  ->
          ignoreComma        '}' >> opaux        >>= \ restr ->
          return $ (jstr keyr, valr):restr

numberParser = withCheckComment $ \state₀@(Ok dat) ->
  case readFloat dat of
    Nothing                         -> unknown
    Just (_, rest@(c:_)) | isAlpha c -> die $ "数字后有非法字母 " ++ shorten rest
    Just (result, rest)             -> put (Ok rest) >> return (plainJNumber result)
  where readFloat :: String -> Maybe (Float, String)
        readFloat s₀ = case reads s₀ :: [(Float, String)] of
                         []  -> Nothing
                         r:_ -> Just r

stringParser = withCheckComment $ \state₀@(Ok dat) ->
  case dat of '\"':rest -> spaux rest
              _         -> unknown
  where spaux :: String -> JState
        spaux s = maybe (die "存在未闭合的字符串") str2jstate (extraString s)
        str2jstate :: (String, String) -> JState
        str2jstate (str, rest) = put (Ok rest) >> return (plainJString str)

constantParser = withCheckComment $ \state₀@(Ok dat) ->
  case find (\ (k, _) -> k `isPrefixOf` dat) alist of
    Nothing     -> unknown
    Just (k, v) -> case drop (length k) dat of
                     c:cs | isAlphaNum c || c == '_' ->
                            die $ "存在未知的字面量 " ++ shorten dat
                     dat' -> put (Ok dat') >> return v
  where alist = [ ("true", plainJBool True)
                , ("false", plainJBool False)
                , ("null", plainJNull)]

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
skipSpacer = maybeOk () $ \ (Ok s) -> put . Ok . skipSpace $ s

die :: ErrorMsg -> JState
die msg = put (Fatal msg) >> return JNothing

unknown :: JState
unknown = put Unknown >> return JNothing

maybeOk :: a -> (ParsingFlag -> JParsingState a) -> JParsingState a
maybeOk d f = get >>= \state₀ ->
  case state₀ of
    Ok _ -> f state₀
    err  -> put err >> return d

whenOk :: (ParsingFlag -> JState) -> JState
whenOk = maybeOk JNothing


nextCharShould :: (Char -> Bool) -> JState
nextCharShould pred = whenOk $ \ (Ok dat) ->
  case skipSpace dat of
    c:cs | pred c -> put (Ok $ skipSpace cs) >> return JNothing
    cs            -> die ("存在语法错误 " ++ shorten cs)
ignoreComma :: Char -> JState
ignoreComma closeSign =
  skipSpacer >> whenOk checkComma
  where checkComma (Ok (',':rest)) =
          case skipSpace rest of
            c:rest' | c == closeSign -> die $ "存在非法的逗号：, " ++ [c]
            rest'                    -> put (Ok rest') >> return JNothing
        checkComma _             = return JNothing
