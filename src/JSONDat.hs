module JSONDat where

-- | 代表JSON里的注释，其定义主要是为了区分不同的注释风格。其值分为行
-- 内注释和多行注释。行内注释以“\/\/”开始，换行符“\\n”结束，之间的所有
-- 内容都会被解释为注释；多行注释以字符序列“\/*”开始，以字符序列“*/”结
-- 束。多行注释不嵌套，之中的所有字符都被解释为注释。字符串中的字符序
-- 列不会被作为注释解析。
data JComment = JInlineComment String
              | JMultilineComment String
              | JNoComment
              deriving (Eq, Ord, Show)

-- | 象征所有具有实际意义的JSON值。这个数据结构的主要意义是记录注释。
-- 考虑到一段JSON代码中的任何地方都可以插入注释，并且去掉任何地方的注
-- 释不会影响这段代码的求值，所以任意一段注释一定在一个JSON数据结构的
-- 之前或者之后。在此之上，一段注释可以是连续的多个注释，并且注释风格
-- （行内注释或者多行注释）可能不同，所以每个数据结构由前缀注释、实际
-- 值、后缀注释组成，其中注释可能为空。另外，这里对注释的判断只是为了
-- 定位注释而非为了在语法上解析注释，因此一段为下方代码做的注释，可能
-- 被解析到上方的代码中。
data JAtom a = JAtom
               { jprec  :: [JComment] -- ^ 实际值之前的注释
               , jval   :: a          -- ^ 实际包含的值
               , jpostc :: [JComment] -- ^ 实际值之后的注释
               } deriving (Eq, Ord, Show)

-- | 代表实际的JSON数据。除了特殊值空值以外，所有值都是对'JAtom'值的包
-- 装。这里定义的JSON值分为：
--
--  [@JNumber@] 数字，内部使用一个浮点数表示，还不能分辨不同的数字类型
--  [@JString@] 字符串，现在仅接受由双引号包裹的字符串
--  [@JBool@]   布尔值，包含 /true/ 和 /false/ 两个字面量
--  [@JNull@]   NULL值，即字面量 /null/
--  [@JArray@]  数组，即由方括号包裹的、逗号分隔的一组JSON值
--  [@JObject@] 对象，即由方括号包裹的、逗号分隔的、一组序对，每个序对
--  的左侧为一个字符串，右侧为一个任意JSON值。
--
-- 其中数组和对象为复合数据，数据结构中的项都由逗号分隔。约定逗号不出
-- 现在最后一项之后。
data JDat = JNothing                               -- ^ 空值
          | JNumber (JAtom Float)                  -- ^ 数字
          | JString { jstr :: JAtom String }       -- ^ 字符串
          | JBool   (JAtom Bool)                   -- ^ 布尔值，即 true | false
          | JNull   (JAtom ())                     -- ^ 字面量 null
          | JArray  (JAtom [JDat])                 -- ^ 数组，即 [JDat, ..]
          | JObject (JAtom [(JAtom String, JDat)]) -- ^ 对象，即 {JString:JDat, ..}
          deriving (Eq, Ord, Show)

instance Functor JAtom where
  f `fmap` a = a { jval = f $ jval a }

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
