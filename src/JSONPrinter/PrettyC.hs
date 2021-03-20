module JSONPrinter.PrettyC (jnPrint) where

import           Control.Lens
import           Data.Char        (isSpace)
import           Data.List        (intercalate)
import           JSONDat
import           JSONPrinter.Util (isSimpleJNDat)
import           JSONReader

jnPrint :: JNDat -> String
{-^

美化打印JSON数据，会局部地考虑注释。具体地说，某些地方的注释可以被美化
打印，某些地方的注释只会被直接输出。也就是说这个打印器保证如果仅在特定
的位置书写注释则可以保证美化输出。具体的输出规则是仅有顶层对象（包括标
量、数组、对象）之前和之后的注释、数组的每个项之前的注释和对象的每个键
值之前的注释会被美化输出，其余所有位置的注释都会被直接输出。也就是说，
下列各个位置中：

<OK>
{
    <OK>
    "button"<IGNORE>: <IGNORE>[
        <OK>
        {
            <OK>
            "type": "click",
            "name": "今日歌曲",
            "key": "V1001_TODAY_MUSIC" <IGNORE>
        }<IGNORE>,
        {
            "type": "click",
            "name": "歌手简介",
            "key": "V1001_TODAY_SINGER"
        } <IGNORE>
    ] <IGNORE>
} <OK>

其中位于 <OK> 位置的注释会具有语法缩进。行内注释会从当前的语法缩进开始，
多行注释的起始标志和结束标志都会提行并具有当前的语法缩进，其中的内容从
第二行开始都会具有比当前缩进长度增加一个空格的语法缩进。对于 <IGNORE>
的注释，打印器只保证“会”打印出来，不做其余保证。

-}
jnPrint x = jnPrintCmt 0 (view jnPreCmtLens x)
            ++ jnPrintInner 0 x ++ (if null postc then "" else "\n")
            ++ jnPrintCmt 0 postc
  where postc = view jnPostCmtLens x

jnPrint' :: Int     -- ^ 语法深度
         -> JNDat   -- ^ 数据
         -> String  -- ^ 打印结果
-- ^ 仅美化打印代码前的注释，用于数组的项和对象的项的键
jnPrint' depth x
  = jnPrintCmt  depth (view jnPreCmtLens x)
    ++ jnPrintInner depth x ++
    jnPrintCmt' (view jnPostCmtLens x)

jnPrint'' :: Int -> JNDat -> String
-- ^ 前后的注释都不处理。用于对象的项的值
jnPrint'' depth x
  = jnPrintCmt' (view jnPreCmtLens x)
    ++ jnPrintInner depth x ++
    jnPrintCmt' (view jnPostCmtLens x)

jnPrintInner :: Int -> JNDat -> String
jnPrintInner _ (JNScalar _ v _) = v
jnPrintInner depth (JNArray _ v _)
  = case v of
      []                    -> "[]"
      [(JNScalar [] v' [])] -> "[ " ++ v' ++ " ]"
      _                     -> "[\n" ++ p v ++ "\n" ++ space depth ++ "]"
  where p = intercalate ",\n" . map ((space depth' ++) . jnPrint' depth')
        depth' = tabSize + depth
jnPrintInner depth (JNObject _ v _)
  = case v of
      [] -> "{}"
      [(JNScalar [] key [], JNScalar [] val [])] -> "{ " ++ key ++ ": " ++ val ++ " }"
      _ -> "{\n" ++ p v ++ "\n" ++ space depth ++ "}"
  where p = intercalate ",\n" . map pterm
        depth' = tabSize + depth
        pterm (key, val) =
          space depth' ++ jnPrint' depth' key ++ ": " ++ jnPrint'' depth' val

jnPrintCmt :: Int -> [JNComment] -> String
jnPrintCmt _     [] = ""
jnPrintCmt depth cs = (unlines $ pindent $ map psign cs) ++ space depth
  where psign (JNInlineComment    s) = "//" ++ s
        psign (JNMultilineComment s) =
          let texts = lines s
          in case texts of
               [] -> "/**/"
               [c] -> "/*" ++ c ++ "*/"
               c:cs -> "/*" ++ c ++ "\n" ++
                       foldr (\s ss -> pterm s ++ ss) "" (filter (not . all isSpace) cs)
                       ++ space depth ++ "*/"
        pterm s = space (1 + depth) ++ (dropWhile isSpace s) ++ "\n"
        pindent []     = []
        pindent (s:ss) = s:map (space depth ++) ss

jnPrintCmt' :: [JNComment] -> String
jnPrintCmt' = foldr (\c s -> p c ++ s) ""
  where p (JNInlineComment s)    = "//" ++ s ++ "\n"
        p (JNMultilineComment s) = "/*" ++ s ++ "*/"

tabSize :: Int
tabSize = 4

space :: Int -> String
space = (`replicate` ' ')
