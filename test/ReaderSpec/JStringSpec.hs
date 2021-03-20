module ReaderSpec.JStringSpec where

import           JSONReader.String (extractString)
import           Test.Hspec

spec = describe "JSON字符串提取器" $ do
  it "提取普通字符串" $ do
    extractString "\"hello, world!\" some/rest/contents" `shouldBe`
      Right ("\"hello, world!\"", " some/rest/contents")
  it "提取包含简单转义字符的字符串" $ do
    extractString "\"h\\/e\\bl\\fl\\no\\r!\\t\\u0388\\\\\" right?" `shouldBe`
      Right ("\"h\\/e\\bl\\fl\\no\\r!\\t\\u0388\\\\\"", " right?")
  it "提取包含其他转义字符的字符串" $ do
    extractString "\"Then she said: \\\"Really!\\\"\": false" `shouldBe`
      Right ("\"Then she said: \\\"Really!\\\"\"", ": false")

