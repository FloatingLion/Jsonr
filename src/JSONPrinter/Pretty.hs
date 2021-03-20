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
module JSONPrinter.Pretty (jnPrint) where

import           Data.List        (intercalate)
import           JSONDat
import           JSONPrinter.Util
import           JSONReader       (jnRead)

jnPrint :: JNDat -> String
jnPrint = jnPrint' 0


jnPrint' :: Int    -- ^ 语法深度
        -> JNDat   -- ^ 数据
        -> String  -- ^ 打印结果
jnPrint' _ (JNScalar _ v _) = v
jnPrint' depth array@(JNArray _ a _) =
  if and $ map isSimpleJNDat a
  then inlinePrint array
  else multilinePrint depth array
jnPrint' depth object@(JNObject _ a _) =
  case a of
    [(_, v)] | isSimpleJNDat v -> inlinePrint object
    []                         -> "{ }"
    _                          -> multilinePrint depth object

inlinePrint :: JNDat -> String
multilinePrint :: Int -> JNDat -> String

inlinePrint (JNArray _ a _)
  = "[" ++ visualize a ++ " ]"
  where visualize = intercalate "," . map ((' ':).jnPrint)

inlinePrint (JNObject _ a _)
  = let [(k, v)] = a
    in "{ " ++ jnPrint k ++ ": " ++ jnPrint v ++ " }"

multilinePrint depth (JNArray _ a _)
  = "[\n" ++ intercalate ",\n" items ++ "\n" ++ tabs depth ++ "]"
  where items = map ((tabs (1 + depth)++) . jnPrint' (1 + depth)) a

multilinePrint depth (JNObject _ a _)
  = "{\n" ++ intercalate ",\n" items ++ "\n" ++ tabs depth ++ "}"
  where items = flip map a $ \ (k, v) ->
          let newDepth = 1 + depth
          in tabs newDepth ++ jnPrint k ++ ": " ++ jnPrint' newDepth v

tabs :: Int -> String
tabs n = replicate (4 * n) ' '
