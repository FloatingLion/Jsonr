module PrinterSpec.Util where

import           System.Environment (getEnv)
import           System.FilePath    ((</>))

makSpec :: String             -- ^ 期望文件后缀名
        -> (String -> String) -- ^ 处理函数
        -> FilePath           -- ^ 实际样本文件
        -> IO Bool            -- ^ 比对结果
-- ^ 生成一个测试，会从 @$JSONR_TEST_DIRECTORY\/storage@ 中读取名为
-- @FilePath@ 的文件，将其内容和 @FilePath ++ String@ ，即加上第一个参
-- 数作为后缀名的文件相比对，如果二者相等则返回 @True@ ，否则返回
-- @False@ 。任何错误都会引发一个异常。
makSpec ext f filename = do
  testdir <- getEnv "JSONR_TEST_DIRECTORY"
  let storage = testdir </> "storage"
  let filename' = storage </> filename
  sample <- readFile filename'
  expected <- readFile (filename' ++ ext)
  return $ f sample == expected
