{-|
Module    : JSONReader
Maintainer: lifoz@outlook.com

= 更改记录

== 2021-02-23 22:05:10

完成模块编写，做简单测试下一步可能的工作为

* 测试模块的功能和效率，并比对JSON语法标准做调整
* 替换 'numberParser' 中使用的数字解析函数，添加额外的数字格式
* 尝试重构 'arrayParser' 和 'objectParser'，减少模式匹配

-}
module JSONReader (jread) where

import           Control.Arrow       (first, second)
import           Control.Monad.State
import           Data.Char           (isAlpha, isAlphaNum, isSpace)
import           Data.List           (find, isPrefixOf)
import           JSONDat
import           Text.Read           (readsPrec)


{-|

读取一个字符串，尝试按照JSON的语法规则解析这个字符串，如果成功解析则返
回这个字符串对应的用 'JDat' 表示的字符串，否则返回一则包含错简单错误提
示的字符串。所有的解析函数都是 State 单子，解析状态定义为三个标志：

* 'Unknown' 代表该解析器不能识别这个字符串。
* 'Fatal'   解析器可识别这个字符串，但在解析时发现语法错误。
* 'Ok'      代表但前解析正确，包含剩余的未解析字符串。

如果发现所有合法的解析器都不能够解析某个字符串，函数 'jread' 会将其转
化为一个 'Fatal' 错误。某些地方可以接受任意合法的JSON值（如数组的每个
元素），而某些地方只能接受特定类型的值（如对象的键值），具体的解析的尝
试顺序见 'jValueParser'。每个JSON值之前和之后都可以出现一个或多个注释，
注释支持行内注释和多行注释两种格式。注释的解析器为 'commentParser'，解
析规则见 'JSONDat.JComment'。具体的解析规则罗列如下：

== 简单数据类型

简单数据类型包括数字，字符串，布尔值，和@NULL@值。

数字将被解析为浮点数，现阶段使用 _Haskell_ 的内置函数解析，以后有可能
另外编写函数解析，这可以将数字分成更多格式解析，现在的解析函数见
'numberParser'。字符串的解析函数是 'stringParser' 字符串从双引号开始，
双引号结束，被包裹的内容都不会解析，接受使用反斜杠转义双引号；除此之外
不会做更多处理。布尔值和NULL值会一起作为常量用 'constantParser' 解析，
这只会做简单的字符串匹配。

== 复合数据类型

复合数据类型包括数组和对象。数组是形如

@
                    [ JSONValue₁, JSONValue₂, ...]
@

的数据，其中每个元素可以为任意JSON值，所有项用逗号分隔比并用中括号包裹，
其数量为0个或多个。对象是形如

@
                 { Key₁ : Value₁, Key₂ : Value₂, ...}
@

的数据，其中的项用逗号分隔并用大括号包裹，其数量为0个或多个。每项为格
式为 @key:value@ 的序对，每个 @key@ 必须能够作为普通的JSON字符串解析，
值可以为任意合法的JSON值。数组和对象中值与值之间的空格只要在保证不影响
语意的前提下数量不限，都要求最后一项之后不能存在逗号，它们的解析函数分
别见 'arrayParser' 和 'objectParser'。

-}
jread :: String -> Either String JDat
jread s = let (result, flag) = runState jValueParser (Ok s)
          in case flag of Unknown   -> Left  "传入的数据无法解析"
                          Fatal msg -> Left  msg
                          Ok []     -> Right result
                          Ok _      -> Left  "传入的数据格式错误"

type ErrorMsg = String
type Stream   = String

data ParsingFlag = Unknown
                 | Fatal   ErrorMsg
                 | Ok      Stream   deriving (Show)
type JParsingState = State ParsingFlag
type JState        = JParsingState JDat

infixl 2 <|>
(<|>) :: JState -> JState -> JState
-- ^ 连接两个状态，表现为‘或’语意。
p₁ <|> p₂ = whenOk $ \state₀@(Ok _) ->
  let (result, state₁) = runState p₁ state₀
  in case state₁ of
       Unknown -> p₂
       _       -> put state₁ >> return result

constantParser, numberParser, stringParser :: JState
arrayParser   , objectParser, jValueParser :: JState

jValueParser = objectParser <|> arrayParser  <|>
               stringParser <|> numberParser <|> constantParser

withCheckComment :: (ParsingFlag -> JState) -> JState
-- ^ 接受一个解析函数，在运行这个函数之前和之后检查表达式周围的注释。
-- 约定总是向解析函数传递一个 'Ok' 状态，并要求其返回一个 'JState'，这
-- 个 'JState' 不解析（周围的）注释，如果解析器消耗了注释会导致位于代
-- 码之后的注释丢失。如果注释解析发生失败（即 'Fatal'），函数会忽略参
-- 数函数的解析结果直接返回错误信息。注释的具体内容参见
-- 'JSONDat.JComment'
--
-- 这个函数不会调用仅会在注释周围调用 'skipSpace'，不会在调用结束时，
-- 也就是尾部注释之后调用 'skipSpace'
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
  where spaux      :: String -> JState
        str2jstate :: (String, String) -> JState
        spaux s = maybe (die "存在未闭合的字符串") str2jstate (extraString s)
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

commentParser :: JParsingState [JComment]
-- ^ 提取注释，不能用于解析器的组合。当发现多行注释未闭合时会设置
-- 'Fatal' 状态。不会返回 'Unknown'。不会在内部调用 'skipSpace'。
commentParser = maybeOk [] $ \ state₀@(Ok dat) ->
  case dat of
    '/':'/':r -> makComment (inlineParser r)    >>= \ cmt ->
      first (JInlineComment cmt:) `mapState` commentParser
    '/':'*':r -> makComment (multilineParser r) >>= \ cmt ->
      first (JMultilineComment cmt:) `mapState` commentParser
    _         -> return []
  where makComment :: (String, ParsingFlag) -> JParsingState String
        inlineParser, multilineParser :: String -> (String, ParsingFlag)
        inlineParser []          = ([], Ok [])
        inlineParser ('\n':rest) = ([], Ok rest)
        inlineParser (c:rest)    = (c:) `first` inlineParser rest
        multilineParser []             = ([], Fatal "存在未闭合的多行注释")
        multilineParser ('*':'/':rest) = ([], Ok rest)
        multilineParser (c:rest)       = (c:) `first` multilineParser rest
        makComment (c, Ok s) = put (Ok $ skipSpace s) >> return c
        makComment (_, err)  = put err >> return []

extraString :: String -> Maybe (String, String)
extraString s₀ = case break (\c -> c == '"' || c == '\\') s₀ of
                  (_, [])        -> Nothing
                  (s₁, '"':r)    -> Just (s₁, r)
                  (s₁, '\\':c:r) -> first ((s₁ ++ ['\\', c]) ++) <$> extraString r
                  (_,  '\\':[])  -> Nothing

shorten :: String -> String
shorten = add . takeWhile (not.isSpace) . take 5
  where add s = if length s <= 5 then [] else s ++ "..."

skipSpace :: String -> String
skipSpace = dropWhile isSpace

skipSpacer :: JParsingState ()
skipSpacer = maybeOk () $ \ (Ok s) -> put . Ok . skipSpace $ s

die :: ErrorMsg -> JState
die msg = put (Fatal msg) >> return JNothing

unknown :: JState
unknown = put Unknown >> return JNothing

maybeOk :: a -> (ParsingFlag -> JParsingState a) -> JParsingState a
-- ^ 为一个签名为 @ParsingFlag -> JParsingState a@ 的函数添加守卫，仅
-- 当 'ParsingFlag' 取值为 'Ok' 时才调用该函数，否则直接向下一个状态传
-- 递错误。接受返回时的默认值作为参数。
maybeOk d f = get >>= \state₀ ->
  case state₀ of
    Ok _ -> f state₀
    err  -> put err >> return d

whenOk :: (ParsingFlag -> JState) -> JState
whenOk = maybeOk JNothing

nextCharShould :: (Char -> Bool) -> JState
-- ^ 接受一个谓词，检查下一个字符是否符合谓词，如果不符合会将状态转为
-- 'Fatal'，否则会消耗掉该字符。会消耗（跳过）检查字符之前的所有空格，
-- 如果谓词判断为真，还会消耗掉谓词后面的所有空格。
nextCharShould pred = whenOk $ \ (Ok dat) ->
  case skipSpace dat of
    c:cs | pred c -> put (Ok $ skipSpace cs) >> return JNothing
    cs            -> die ("存在语法错误 " ++ shorten cs)

ignoreComma :: Char -> JState
-- ^ 检查下一个字符是否为逗号（即‘,’），如果为逗号则额外检查逗号的下一
-- 个字符不为参数提供的“闭合字符”（如“)”，“]”，“}”等等），否则设置
-- 'Fatal' 标志。检查过程中会消耗逗号两侧的（如果存在）的空格。
ignoreComma closeSign =
  skipSpacer >> whenOk checkComma
  where checkComma (Ok (',':rest)) =
          case skipSpace rest of
            c:rest' | c == closeSign -> die $ "存在非法的逗号：, " ++ [c]
            rest'                    -> put (Ok rest') >> return JNothing
        checkComma _ = return JNothing
