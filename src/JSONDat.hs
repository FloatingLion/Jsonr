module JSONDat where

import           Control.Lens

-- | 代表JSON里的注释，区分不同的注释风格。分为行内注释和多行注释。行
-- 内注释以“\/\/”开始，换行符“\\n”结束，之间的所有内容都会被解释为注释；
-- 多行注释以字符序列“\/*”开始，以字符序列“*/”结束。多行注释不嵌套，之
-- 中的所有字符都被解释为注释。字符串中的字符序列不会被作为注释解析。
data JNComment = JNInlineComment String
               | JNMultilineComment String
               deriving (Eq, Show)

-- | 代表实际的JSON数据。定义的JSON值分为：
--
--  [@JNScalar@] 包括数字、字符串、布尔值和空值
--  [@JNArray@]  数组，即由方括号包裹的、逗号分隔的一组JSON值
--  [@JNObject@] 对象，即由方括号包裹的、逗号分隔的、一组序对，每个序对
--               的左侧为一个字符串，右侧为一个任意JSON值。
--  [@JNothing@] 不表示任何值，仅作为发生错误时的返回值。
--
-- 其中数组和对象为复合数据，数据结构中的项都由逗号分隔。约定逗号不出
-- 现在最后一项之后。
-- 而标量指不可分割的值，其内容由一个字符串 'jnsv' 记录。如果 'jnsv'
-- 是一个数字则 'jnsv' 包含所有能够明确该数字的字符；如果 'jnsv' 是一
-- 个字符串则其首字符是“"”并且最后一个字符也是“"”，之间的内容是字符串
-- 的内容；如果 'jnsv' 是一个常量，即 'true'，'false' 和 'null' 则
-- 'jnsv' 的值就是 'true' 'false' 和 'null' 。
-- 此外每个数据结构都包含了注释。考虑到一段JSON代码中的任何地方都可以
-- 插入注释，并且去掉任何地方的注释不会影响这段代码的求值，所以任意一
-- 段注释一定在一个JSON数据结构的之前或者之后。在此之上，一段注释可以
-- 是连续的多个注释，并且注释风格（行内注释或者多行注释）可能不同，所
-- 以每个数据结构由前缀注释、实际值、后缀注释组成，其中注释可能为空。
-- 另外，这里对注释的判断只是为了定位注释而非为了在语法上解析注释，因
-- 此一段为下方代码做的注释，可能被解析到上方的代码中。也就是说这里的
-- 注释是语法上的而非语义上的。
--
-- 最后，一个对象的键值一定是一个字符串，在这里也就是一定是一
-- 个'JNScalar' 。这个约定应当为每个可能创建 'JNDat' 的函数遵守。有关
-- 类型检验见 "JSONUtil.jnTypeof"
data JNDat =
  JNScalar { jnspc :: [JNComment]   -- ^ JSON Scalar Previous Comment
           , jnsv  :: String        -- ^ JSON Scalar Value
           , jnsnc :: [JNComment] } -- ^ JSON Scalar Next Comment
  | JNArray { jnapc :: [JNComment]   -- ^ JSON Array Previous Comment
            , jnav  :: [JNDat]       -- ^ JSON Array Value
            , jnanc :: [JNComment] } -- ^ JSON Array Next Comment
  | JNObject { jnopc :: [JNComment]      -- ^ JSON Object Previous Comment
             , jnov  :: [(JNDat, JNDat)] -- ^ JSON Object Value
             , jnonc :: [JNComment] }    -- ^ JSON Object Next Comment
  | JNothing
  deriving (Eq, Show)

jnPreCmtLens, jnPostCmtLens :: Lens' JNDat [JNComment]
jnPreCmtLens f s@(JNScalar c _ _) = fmap (\x -> s{ jnspc = x }) (f c)
jnPreCmtLens f s@(JNArray  c _ _) = fmap (\x -> s{ jnapc = x }) (f c)
jnPreCmtLens f s@(JNObject c _ _) = fmap (\x -> s{ jnopc = x }) (f c)

jnPostCmtLens f s@(JNScalar _ _ c) = fmap (\x -> s{ jnsnc = x}) (f c)
jnPostCmtLens f s@(JNArray  _ _ c) = fmap (\x -> s{ jnanc = x}) (f c)
jnPostCmtLens f s@(JNObject _ _ c) = fmap (\x -> s{ jnonc = x}) (f c)

jnScalarLens :: Lens' JNDat String
jnScalarLens f s@(JNScalar _ v _) = fmap (\x -> s{ jnsv = x}) (f v)
jnScalarLens _ _                  = error "不能将数组或者对象传入 jnScalarLens"

jnArrayLens :: Lens' JNDat [JNDat]
jnArrayLens f s@(JNArray _ v _) = fmap (\x -> s{ jnav = x}) (f v)
jnArrayLens _ _                 = error "不能将标量或者对象传入 jnArrayLens"

jnObjectLens :: Lens' JNDat [(JNDat, JNDat)]
jnObjectLens f s@(JNObject _ v _) = fmap (\x -> s{ jnov = x}) (f v)
jnObjectLens _ _                  = error "不能将标量或者数组传入 jnObjectLens"
