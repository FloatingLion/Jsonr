module JSONReader.Constant (extractConstant) where

import           Data.Char       (isSpace)
import           Data.List       (find, isPrefixOf)
import           JSONReader.Util

extractConstant :: String -> Either String (String, String)
-- ^ 从一个字符流中提取预定义的字面量。其中的字面量由 'constList' 定
-- 义，且约定这些字面量位于字符流的开头。如果字面量匹配，会检查字面量
-- 之后的字符是否为合法的分隔字符，分隔字符按照 *ECMA-404* 标准定义，
-- 包括：
--
--     * @,@ 对象和数组的项分隔符，如 @[true, 0]@
--     * @}@ 对象的结束符，如         @{\"really\": false}@
--     * @]@ 数组的结束符，如         @[null]@
--     * @/@ 注释的起始符，如         @true \/\/ ...@
--     * 空白和EOF，后者用 [] 表示。
--
-- 其中注释非标准定义。
extractConstant s = maybe (Left $ "存在未知的字面量" ++ shorten s) `flip`
                    find (\ k -> k `isPrefixOf` s) constList            $
                    \ k -> case drop (length k) s of
                             s'@(c:_) | not (isSpace c) && c `notElem` sepList
                                        -> Left ("字面量" ++ k ++ "后有非法字符" ++ [c])
                             s'         -> Right (k, s')

constList = ["true", "false", "null"]

sepList = [',', '}', ']', '/']
