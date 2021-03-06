module ReaderSpec.JConstantSpec where

import           JSONReader.Constant (extractConstant)
import           JSONReader.Util
import           Test.Hspec

spec = describe "JSON常量解析器" $ do
  it "解析 true" $ do
    extractConstant "true, true, true" `shouldBe` Right (plainJBool True, ", true, true")
  it "解析 false" $ do
    extractConstant "false }" `shouldBe` Right (plainJBool False, " }")
  it "解析 null" $ do
    extractConstant "null" `shouldBe` Right (plainJNull, "")
