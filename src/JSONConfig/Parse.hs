{-# LANGUAGE LambdaCase #-}
module JSONConfig.Parse (parseconf) where

import           Data.Char        (isAlpha, isSpace)
import qualified Data.Map         as Map
import           JSONConfig.JSONR
import           JSONConfig.Util

parseconf :: String -> JSONR
parseconf = undefined

defaultConfig :: [(String, String)]
{-^ 默认的JSONR配置，同时也是现在支持的所有配置。这个配置方案已知的一个
问题是不能处理字符串中的转义字符，也就是说源数据中的所有转义字符只能以
原格式输出到目标数据中。因此这要求目标格式的文件的转义字符集必须是JSON
标准的转义字符集的超集。下面依次定义各个属性项的含义：

- indent-style

  取值为 space 或者 tab，代表使用空格缩进还是使用TAB缩进。这对于配置文
  件中的空格（32）和TAB（9）无效，仅用于替换特殊变量“$tab”指示生成的缩
  进符起作用。

- indent-size

  如果设置“indent-style”为“space”则该变量的值指示使用几个空格缩进。其
  值应当为一个简单的正整数。即取值 (1,2,3,…)。

- before-content

  在所有转换之前出现且仅出现一次。可以用于放置注释、声明或者其他特殊结
  构。其值为任意字符串。

- after-content

  在所有转换之后出现且仅出现一次。使用方法参考“before-content”。

- true-value, false-value

  分别指代真值和假值，其值为任意字符串。

- open-string, close-string

  定义一个字符串的起始和终止。正如开头描述的，字符串内部的转义字符不会
  被做额外处理，同时对于字符串的内容中的内容也不会自动转义，包括这里定
  义的起止符。最稳妥的是将这两个值均设置为“"”。

- open-array, close-array, open-object, close-object

  分别用于定义数组和对象的起始和终止。内容为任意字符串。

- array-separator, object-separator

  用于定义数组项和对象的字段之间的分隔符。内容为任意字符串。

另外需要说明，除了某些特殊变量，配置文件中对各个属性的定义中的所有字符都不会省略或者额外解释。这意味着包括换行符在内的空白符也会作为属性的一部分解释。

-}
defaultConfig =
  [ ("indent-style", "space")
  , ("indent-size", "4")
  , ("before-content", "")
  , ("after-content", "")
  , ("true-value", "true")
  , ("false-value", "false")
  , ("open-string", "\"")
  , ("close-string", "\"")
  , ("open-array", "[")
  , ("close-array", "]")
  , ("array-separator", ",")
  , ("open-object", "{")
  , ("close-object", "}")
  , ("object-separator", ",") ]

defaultConfigMap :: JSONR
defaultConfigMap = Map.fromList defaultConfig

-- 未测试
readPlainConfig :: State String (Either String [(String, String)])
readPlainConfig = get >>= \ case
  [] -> return (Right [])
  s@(c:_) | isWordChar c -> getKey >>= \case
              Left err -> return (Left err)
              Right key -> getValue >>= \case
                Left err -> return (Left err)
                Right val ->
                  get >>= \ rest ->
                  let (rez, _) = runState (skipState >> readPlainConfig) rest
                  in return ((:) <$> Right (key, value) <*> rez)

getKey :: State String (Either String String)
getKey = get >>= \ s ->
  let (key, rest) = span isWordChar s
  in case rest of
       [] -> return $ Left ("属性" ++ key ++ "没有对应的值")
       (c:_) | not (isSpace c) -> return $
               Left ("无效字符：<" ++ [c] ++ ">（码点：" ++ show (ord c) ++ "）")
       (c:rest) -> put rest >> return (Right key)


getValue :: State String String
getValue = undefined

isWordChar :: Char -> Bool
isWordChar c =
  isAlpha c && '-' == c
