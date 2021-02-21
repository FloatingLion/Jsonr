module JSONDat where

data JComment = JInlineComment String
              | JMultilineComment String
              | JNoComment
              deriving (Eq, Ord, Show)

data JAtom a = JAtom
               { jprec  :: JComment
               , jval   :: a
               , jpostc :: JComment}
               deriving (Eq, Ord, Show)

data JDat = JNothing                         -- ^ 空值
          | JNumber (JAtom Float)            -- ^ 数字
          | JString { jstr :: JAtom String } -- ^ 字符串
          | JBool   (JAtom Bool)             -- ^ 布尔值，即 true | false
          | JNull   (JAtom ())               -- ^ 字面量 null
          | JArray  (JAtom [JDat])           -- ^ 数组，即 [JDat, ..]
          | JObject (JAtom [(String, JDat)]) -- ^ 对象，即 {JString:JDat, ..}
          deriving (Eq, Ord, Show)

instance Functor JAtom where
  f `fmap` a = a { jval = f $ jval a }

annotate :: JDat     -- ^ 语法结构
        -> JComment -- ^ 前缀注释
        -> JComment -- ^ 后缀注释
        -> JDat     -- ^ 新的语法结构
annotate (JNumber (JAtom _ x _)) c₁ c₂ = JNumber (JAtom c₁ x c₂)
annotate (JString (JAtom _ x _)) c₁ c₂ = JString (JAtom c₁ x c₂)
annotate (JBool   (JAtom _ x _)) c₁ c₂ = JBool   (JAtom c₁ x c₂)
annotate (JNull   (JAtom _ x _)) c₁ c₂ = JNull   (JAtom c₁ x c₂)
annotate (JArray  (JAtom _ x _)) c₁ c₂ = JArray  (JAtom c₁ x c₂)
annotate (JObject (JAtom _ x _)) c₁ c₂ = JObject (JAtom c₁ x c₂)
annotate x                       _  _  = x

