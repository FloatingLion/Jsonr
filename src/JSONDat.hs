module JSONDat (JDat(..)) where

data JDat = JNothing                   -- ^ 空值
          | JNumber Float              -- ^ 数字
          | JString { jstr :: String } -- ^ 字符串
          | JComment String            -- ^ 注释
          | JBool Bool                 -- ^ 布尔值，即 true | false
          | JNull                      -- ^ 字面量 null
          | JArray [JDat]              -- ^ 数组，即 [JDat, JDat, ..]
          | JObject [(String, JDat)]   -- ^ 对象，即 {JString:JDat, JString:JDat, ..}
          deriving (Eq, Ord, Show)

