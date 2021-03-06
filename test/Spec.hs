module Main where

import qualified ReaderSpec.JConstantSpec
import qualified ReaderSpec.JNumberSpec
import qualified ReaderSpec.JStringSpec
import           Test.Hspec

main = hspec $ do
  describe "简单数据解析器" $ do
    ReaderSpec.JNumberSpec.spec
    ReaderSpec.JStringSpec.spec
    ReaderSpec.JConstantSpec.spec
