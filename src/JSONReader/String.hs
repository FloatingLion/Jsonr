module JSONReader.String (extractString) where

import           Control.Arrow (first)

extractString :: String -> Either String (String, String)
-- ^ 从一个字符流中提取出被 @\"@ 包裹的部分。约定字符流的第一个字符为
-- 打开字符串的双引号 @\"@，并且存在闭合双引号 @\"@。依照 *ECMA-404*
-- 标准，字符串中可以存在任意 Unicode 码点，因此解析时只对 @\\\"@ 做转
-- 义。如果字符流的第一个字符不是双引号或者字符流剩余的部分没有闭合双
-- 引号，都会返回一个错误。成功解析会返回由解析结果和剩余字符流组成的
-- 双元组。元组中的解析结果包含了双引号
extractString ('"':s₀) = either Left (Right . first (\s -> "\"" ++ s ++ "\"")) result
  where result = collectString s₀
extractString _        = error "参数的第一个字符应当是双引号"

collectString :: String -> Either String (String, String)
collectString []       = Left errorMsg
collectString ('"':r)  = Right ([], r)
collectString ('\\':r) | null r    = Left errorMsg
                       | otherwise = first (['\\', head r]++) <$> collectString (tail r)
collectString s₀ = let (s₁, r) = break (\c -> c == '"' || c == '\\') s₀
                   in first (s₁++) <$> collectString r

errorMsg = "存在未闭合的字符串"

