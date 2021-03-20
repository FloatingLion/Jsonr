{-|
Module      : JSONReader
Description : 基于 _Standard ECMA-404_ 的JSON解析器
Maintainer  : lifoz@outlook.com

= 更改记录

== 2021-03-20 11:34:24

重构 'JNDat' 数据结构，将 'jread' 更名为 'jnRead' 。

== 2021-03-06 14:20:45

修改所有解析函数，其不在内部检查周围的注释。

这个修改是源于以下考虑。JSON解析中只存在两种可能的项：

- (number|string|constant|array|object)
- string

其中第二个仅在对象的键值中出现。因此，现在仅在两个地方调用注释解析函数
（ 'withCheckComment' ）：

- 'jvalueParser'
- 'objectParser' 中解析键值时

这样做可以减少重复匹配。但要注意：不能再单独调用各个解析器，能够被外界
调用的解析器只有 'jvalueParser' ，其他解析器或被 'jvalueParser' 调用，
或在包裹着 'withCheckComment' 地调用。最后，约定所有解析器假定输入的数
据是正常的（即 'Ok'）。

== 2021-03-06 13:38:35

为 arrayParser 和 objectParser 的开括号后添加 skipSpace。

== 2021-02-26 16:35:41

重写 'numberParser' 解析器，可以依据 @ECMA-404@ 标准解析数字。

== 2021-02-26 11:08:57

重构部分代码，将字符串和字面量的处理函数移动到 "JSONReader.String" 和
"JSONReader.Constant" 中。将部分不涉及解析细节的函数移动到
"JSONReader.Util" 中。

== 2021-02-23 22:05:10

完成模块编写，做简单测试。下一步可能的工作为

* 测试模块的功能和效率，并比对JSON语法标准做调整
* 替换 'numberParser' 中使用的数字解析函数，添加额外的数字格式
* 尝试重构 'arrayParser' 和 'objectParser'，减少模式匹配

另外，因为注释会被视为JSON值的一部分（见 'JNDat'），所以匹配时会导致注
释的重复解析。具体地讲，前缀注释解析之后才会解析实际的JSON值，如果相应
的解析器返回 'Unknown'，整个 'withCheckComment' 块会返回 'Unknown'，而
下一个被包裹在 'withCheckComment' 中的解析器会再次解析前缀注释。这个问
题的一个解决方案是将大部分 'jnRead' 的逻辑移动到 'withCheckComment' 中，
但在此先保留这个优化。

-}
module JSONReader (jnRead) where

import           Control.Arrow       (first, second)
import           Control.Monad.State
import           Data.Char           (isAlpha, isAlphaNum, isDigit)
import           Data.List           (find, isPrefixOf)
import           JSONDat
import           JSONReader.Constant (extractConstant)
import           JSONReader.Number   (extractNumber)
import           JSONReader.String   (extractString)
import           JSONReader.Util

{-|

读取一个字符串，尝试按照JSON的语法规则解析这个字符串，如果成功解析则返
回这个字符串对应的用 'JDat' 表示的字符串，否则返回一则包含错简单错误提
示的字符串。所有的解析函数都是 State 单子，解析状态定义为三个标志：

* 'Unknown' 代表该解析器不能识别这个字符串。
* 'Fatal'   解析器可识别这个字符串，但在解析时发现语法错误。
* 'Ok'      代表但前解析正确，包含剩余的未解析字符串。

如果发现所有合法的解析器都不能够解析某个字符串，函数 'jnRead' 会将其转
化为一个 'Fatal' 错误。某些地方可以接受任意合法的JSON值（如数组的每个
元素），而某些地方只能接受特定类型的值（如对象的键值），具体的解析的尝
试顺序见 'jValueParser'。每个JSON值之前和之后都可以出现一个或多个注释，
注释支持行内注释和多行注释两种格式。注释的解析器见 'commentParser'，解
析规则见 'JSONDat.JComment'。具体的JSON解析规则罗列如下：

== 简单数据类型

简单数据类型包括数字，字符串，布尔值，和@NULL@值。

数字会被解析为字符串，而非具有实际意义的值，更多数字的解析规则见数字的
解析函数 'numberParser'。字符串的解析函数是 'stringParser' 字符串从双
引号开始，双引号结束，被包裹的内容都不会解析，接受使用反斜杠转义双引号；
除此之外不会做更多处理。布尔值和NULL值会一起作为常量用
'constantParser' 解析，这只会做简单的字符串匹配。

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
jnRead :: String -> Either String JNDat
jnRead s = let (result, flag) = runState jValueParser (Ok (skipSpace s))
          in case flag of Unknown   -> Left  "传入的数据无法解析"
                          Fatal msg -> Left  msg
                          Ok []     -> Right result
                          Ok _      -> Left  "传入的数据格式错误，可能存在多余数据"

data ParsingFlag = Unknown
                 | Fatal   String
                 | Ok      String
                 deriving (Show)
type JParsingState = State ParsingFlag
type JState        = JParsingState JNDat

infixl 2 <|>
(<|>) :: JState -> JState -> JState
-- ^ 连接两个状态，表达‘或’语意。
p₁ <|> p₂ = whenOk $ \state₀@(Ok _) ->
  let (result, state₁) = runState p₁ state₀
  in case state₁ of
       Unknown -> p₂
       _       -> put state₁ >> return result

jValueParser :: JState
-- ^ 解析所有具有实际意义的JSON值。下面的组合顺序即尝试解析的顺序，遵
-- 从 @ECMA-404@ 文档中 *Figure 1* 的顺序。如果所有的解析器都不能解析，
-- 'jValueParser' 不做额外处理，直接设置 'Unknown' 标志。
jValueParser = withCheckComment $ \ (Ok dat)
  -> objectParser     <|> arrayParser
     <|> numberParser <|> stringParser
     <|> constantParser

withCheckComment :: (ParsingFlag -> JState) -> JState
-- ^ 接受一个解析函数，在运行这个函数之前和之后检查表达式周围的注释。
-- 约定总是向解析函数传递一个 'Ok' 状态，并要求其返回一个 'JState'，这
-- 个 'JState' 不解析（周围的）注释，如果解析器消耗了注释会导致位于代
-- 码之后的注释丢失。如果注释解析发生失败（即 'Fatal'），函数会忽略参
-- 数函数的解析结果直接返回错误信息。注释的具体内容参见
-- 'JSONDat.JNComment'
--
-- 这个函数不会调用仅会在注释周围调用 'skipSpace'，不会在调用结束时，
-- 也就是尾部注释之后调用 'skipSpace'
withCheckComment f = commentParser  >>=           \ cmt₁   ->
  whenOk $ \ _ -> skipSpacer >> get >>= f     >>= \ result ->
  whenOk $ \ _ -> skipSpacer >> commentParser >>= \ cmt₂   ->
  whenOk $ \ _ -> return (annotate cmt₁ cmt₂ result)

arrayParser, objectParser :: JState
arrayParser = get >>= \ (Ok dat) ->
  case dat of '[':rest -> put (Ok (skipSpace rest))
                          >> (mapState $ first plainJNArray) apaux
              _        -> unknown
  where apaux    :: JParsingState [JNDat]
        extractE :: ParsingFlag -> JParsingState [JNDat]
        apaux = maybeOk [] extractE
        extractE (Ok [])         = put (Fatal "存在未闭合的数组") >> return []
        extractE (Ok (']':rest)) = put (Ok rest)                  >> return []
        extractE _  = jValueParser >>= \ elt  ->
          ignoreComma ']' >> apaux >>= \ rest ->
          return $ elt:rest

objectParser = get >>= \ (Ok dat) ->
  case dat of '{':rest -> put (Ok (skipSpace rest))
                          >> (mapState $ first plainJNObject) opaux
              _        -> unknown
  where extractO :: ParsingFlag -> JParsingState [(JNDat, JNDat)]
        opaux    :: JParsingState [(JNDat, JNDat)]
        opaux = maybeOk [] extractO
        extractO (Ok [])        = put (Fatal "存在未闭合的对象") >> return []
        extractO (Ok ('}':r))   = put (Ok r)                     >> return []
        extractO _              =
          withCheckComment (const stringParser)  >>= \ keyr  ->
          nextCharShould (==':') >> jValueParser >>= \ valr  ->
          ignoreComma        '}' >> opaux        >>= \ restr ->
          return $ (keyr, valr):restr

numberParser :: JState
-- ^ 解析一个数字。如果流以字符“-”或一个数字开头，则会被识别为一个数字；
-- 否则返回 'Unknown'。解析标准见 'JSONReader.Number.extractNumber' 或
-- 者 @ECMA-404@ 标准 *Figure 4* 相应内容。
numberParser = get >>= \state₀@(Ok dat) ->
  case dat of
    s@(c:_) | c == '-' || isDigit c
              -> extractNumber s `whenRight` \ (s, rest) ->
                put (Ok rest) >> return (plainJNScalar s)
    _         -> unknown

stringParser :: JState
-- ^ 解析一个字符串，字符串必须用双引号包裹。详细的解析方法见
-- 'JSONReader.String.extractString'
stringParser = get >>= \state₀@(Ok dat) ->
  case dat of '\"':_ -> extractString dat `whenRight` \ (str, rest) ->
                put (Ok rest) >> return (plainJNScalar str)
              _      -> unknown

constantParser :: JState
-- ^ 处理 @true@、 @false@ 和 @null@ 三个字面量。约定如果字符流以一个
-- *Unicode* 单词开始会被认为是一个字面量，如 @true@、 @foo10@、@你好@
-- 等等，如果不被认为是一个字面量的值会返回 'Unknown'，否则会被
-- 'JSONReader.Constant.extractConstant' 解析，如果解析失败会返回
-- 'Fatal' 。
constantParser = get >>= \state₀@(Ok dat) ->
  case dat of c:_ | isAlpha c -> extractConstant dat `whenRight` \ (v, rest) ->
                      put (Ok rest) >> return (plainJNScalar v)
              _               -> unknown

commentParser :: JParsingState [JNComment]
-- ^ 提取注释，不能用于解析器的组合。当发现多行注释未闭合时会设置
-- 'Fatal' 状态。不会返回 'Unknown'。不会在内部调用 'skipSpace'。
commentParser = maybeOk [] $ \ state₀@(Ok dat) ->
  case dat of
    '/':'/':r -> makComment (inlineParser r)    >>= \ cmt ->
      first (JNInlineComment cmt:) `mapState` commentParser
    '/':'*':r -> makComment (multilineParser r) >>= \ cmt ->
      first (JNMultilineComment cmt:) `mapState` commentParser
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

die :: String -> JState
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

whenRight :: Either String b -> (b -> JState) -> JState
whenRight = flip $ either die

skipSpacer :: JParsingState ()
skipSpacer = maybeOk () $ \ (Ok s) -> put . Ok . skipSpace $ s

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
