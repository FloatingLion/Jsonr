module ReaderSpec.JNumberSpec where

import           JSONReader.Number (extractNumber)
import           Test.Hspec

spec = describe "JSON数字提取器" $ do
  it "处理普通数字" $ do
    extractNumber "0, 1, 2" `shouldBe` Right ("0", ", 1, 2")
  it "处理普通数字" $ do
    extractNumber "255 }" `shouldBe` Right ("255", " }")
  it "处理带符号数字" $ do
    extractNumber "-10 // comment" `shouldBe` Right ("-10", " // comment")
  it "处理带符号数字" $ do
    extractNumber "-0 ]," `shouldBe` Right ("-0", " ],")
  it "处理小数" $ do
    extractNumber "1.2 ,// rest" `shouldBe` Right ("1.2", " ,// rest")
  it "处理科学计数" $ do
    extractNumber "1e10" `shouldBe` Right ("1e10", "")
  it "处理带小数的科学计数" $ do
    extractNumber "1.2E-010 /* */" `shouldBe` Right ("1.2E-010", " /* */")
  it "处理异形科学计数" $ do
    extractNumber "0e0\n" `shouldBe` Right ("0e0", "\n")
