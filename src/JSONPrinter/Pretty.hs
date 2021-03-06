{-|
Module: JSONPrinter.Pretty
Description: 美观打印JSON数据

这是一个美观打印的简单实现，其简单性在于不考虑打印宽度，只根据一些简单
的规则转换数据。下面是打印规则：

1. 数字、字符串和常量直接打印

1.0        "WARNING!"        null

2. 数组左括号不换行，如果元素均为数字、字符串或常量，则单行打印；否则
   多行打印。

[ true ]

[
    [1, 2, 3]
]

[
    "yes",
    "or",
    ["yes", "yes"]
]

3. 对象左括号不换行，如果仅有一项且其值为数字、字符串或常量，则单行打
   印；否则多行打印。

{ "Just": false }

{
    "name": "age",
    "John": 12
}

4. 现阶段不提供注释的格式化输出


-}
module JSONPrinter.Pretty (jprint) where

import           Data.List         (intercalate)
import           JSONDat
import           JSONPrinter.Util
import           JSONReader        (jread)
import           Text.Show.Unicode (ushow)

jprint :: JDat -> String
jprint = jprint' 0


jprint' :: Int    -- ^ 语法深度
        -> JDat   -- ^ 数据
        -> String -- ^ 打印结果
jprint' _ (JNumber a) = jval a
jprint' _ (JString a) = ushow (jval a)
jprint' _ (JBool a) = if jval a then "true" else "false"
jprint' _ (JNull _) = "null"
jprint' depth array@(JArray a) =
  if and $ map isSimpleJData (jval a)
  then inlinePrint array
  else multilinePrint depth array
jprint' depth object@(JObject a) =
  case jval a of
    [(_, v)] | isSimpleJData v -> inlinePrint object
    []                         -> "{ }"
    _                          -> multilinePrint depth object

inlinePrint :: JDat -> String
multilinePrint :: Int -> JDat -> String

inlinePrint (JArray a)
  = "[" ++ visualize (jval a) ++ " ]"
  where visualize = intercalate "," . map ((' ':).jprint)

inlinePrint (JObject a)
  = let [(k, v)] = jval a
    in "{ " ++ ushow (jval k) ++ ": " ++ jprint v ++ " }"

multilinePrint depth (JArray a)
  = "[\n" ++ intercalate ",\n" items ++ "\n" ++ tabs depth ++ "]"
  where items = map ((tabs (1 + depth)++) . jprint' (1 + depth)) (jval a)

multilinePrint depth (JObject a)
  = "{\n" ++ intercalate ",\n" items ++ "\n" ++ tabs depth ++ "}"
  where items = flip map (jval a) $ \ (k, v) ->
          let newDepth = 1 + depth
          in tabs newDepth ++ ushow (jval k) ++ ": " ++ jprint' newDepth v

tabs :: Int -> String
tabs n = replicate (4 * n) ' '
