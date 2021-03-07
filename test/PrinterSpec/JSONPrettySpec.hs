module PrinterSpec.JSONPrettySpec (spec) where

import           JSONPrinter.Pretty (jprint)
import           JSONReader         (jread)
import           PrinterSpec.Util
import           Test.Hspec

spec = do
  it "处理简单的JSON数据" $ do
    prettySpec "sample-1" `shouldReturn` True

  it "处理复杂的JSON数据" $ do
    prettySpec "sample-2" `shouldReturn` True

  it "处理带注释的JSON数据" $ do
    prettySpec "sample-3" `shouldReturn` True

  it "单行转化JSON数据" $ do
    prettySpec "sample-4" `shouldReturn` True

prettySpec = makSpec ".pretty.expected" (either id jprint . jread)
