module JSONUtil where

import           Data.Char (isDigit)
import           JSONDat

data JNType = JSONNumber
            | JSONString
            | JSONBoolean
            | JSONNull
            | JSONArray
            | JSONObject
            | JSONUnknownScalar
            deriving (Eq, Show)

jnTypeof :: JNDat -> JNType
-- ^ 检测一个 'JNDat' 的类型，包含数字、字符串、布尔、空值、数组和对象。
-- 仅当标量（'JNScalar'）中包含非法内容时返回 'JSONUnknownScalar' 代表
-- 错误。标量的内容规则见 'JNDat' 。
jnTypeof (JNScalar _ s@(c:_) _)
  | '"' == c                    = JSONString
  | isDigit c || '-' == c       = JSONNumber
  | s == "true" || s == "false" = JSONBoolean
  | s == "null"                 = JSONNull
  | otherwise                   = JSONUnknownScalar
jnTypeof (JNArray  _ _ _) = JSONArray
jnTypeof (JNObject _ _ _) = JSONObject
