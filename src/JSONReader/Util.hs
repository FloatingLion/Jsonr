module JSONReader.Util where

import           Data.Char (isSpace)
import           JSONDat

shorten :: String -> String
shorten s₀ = add $ takeWhile (not.isSpace) $ take 5 s₀
  where add s = if s == s₀ then s else s ++ "..."

skipSpace :: String -> String
skipSpace = dropWhile isSpace

-- | 为一个JSON数据结构添加注释
annotate :: JDat      -- ^ 语法结构
        -> [JComment] -- ^ 前缀注释
        -> [JComment] -- ^ 后缀注释
        -> JDat       -- ^ 新的语法结构
annotate (JNumber (JAtom _ x _)) c₁ c₂ = JNumber (JAtom c₁ x c₂)
annotate (JString (JAtom _ x _)) c₁ c₂ = JString (JAtom c₁ x c₂)
annotate (JBool   (JAtom _ x _)) c₁ c₂ = JBool   (JAtom c₁ x c₂)
annotate (JNull   (JAtom _ x _)) c₁ c₂ = JNull   (JAtom c₁ x c₂)
annotate (JArray  (JAtom _ x _)) c₁ c₂ = JArray  (JAtom c₁ x c₂)
annotate (JObject (JAtom _ x _)) c₁ c₂ = JObject (JAtom c₁ x c₂)
annotate x                       _  _  = x

-- | 返回一个不带注释的JSON值
plainAtom n = JAtom { jprec  = []
                    , jval   = n
                    , jpostc = [] }
plainJNumber = JNumber . plainAtom
plainJString = JString . plainAtom
plainJBool   = JBool   . plainAtom
plainJArray  = JArray  . plainAtom
plainJObject = JObject . plainAtom
plainJNull   = JNull (plainAtom ())
