{-|
Module: Main
Description: 单元测试的主模块

如果要运行该测试，需要设置环境变量 @JSONR_TEST_DIRECTORY@ 指向该文件所
在的目录。

-}
module Main where

import qualified PrinterSpec.JSONCompressSpec
import qualified PrinterSpec.JSONPrettyCSpec
import qualified PrinterSpec.JSONPrettySpec
import qualified ReaderSpec.JConstantSpec
import qualified ReaderSpec.JNumberSpec
import qualified ReaderSpec.JStringSpec
import           Test.Hspec

main = hspec $ do
  describe "简单数据解析器" $ do
    ReaderSpec.JNumberSpec.spec
    ReaderSpec.JStringSpec.spec
    ReaderSpec.JConstantSpec.spec

  describe "JSON数据压缩器" $ do
    PrinterSpec.JSONCompressSpec.spec

  describe "JSON数据美化打印" $ do
    PrinterSpec.JSONPrettySpec.spec

  describe "JSON数据美化打印（局部处理注释）" $ do
    PrinterSpec.JSONPrettyCSpec.spec

