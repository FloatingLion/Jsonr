module JSONReader.Util where

import           Control.Lens
import           Data.Char    (isSpace)
import           JSONDat

shorten :: String -> String
shorten s₀ = add $ takeWhile (not.isSpace) $ take 5 s₀
  where add s = if s == s₀ then s else s ++ "..."

skipSpace :: String -> String
skipSpace = dropWhile isSpace

-- | 为一个JSON数据结构添加注释
annotate :: [JNComment] -- ^ 前缀注释
         -> [JNComment] -- ^ 后缀注释
         -> JNDat       -- ^ 语法结构
         -> JNDat       -- ^ 新的语法结构
annotate a b = set jnPreCmtLens a . set jnPostCmtLens b


-- | 返回一个不带注释的JSON值
plainJNScalar s = JNScalar [] s []
plainJNArray  s = JNArray [] s []
plainJNObject s = JNObject [] s []
