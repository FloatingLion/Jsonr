module PrinterSpec.JSONCompressSpec (spec) where

import           JSONPrinter.Compress (jnPrint)
import           JSONReader           (jnRead)
import           PrinterSpec.Util
import           Test.Hspec

compressTest = makSpec ".compress.expected" (either id jnPrint . jnRead)

spec = do
  it "压缩简单的JSON数据" $ do
    compressTest "sample-1" `shouldReturn` True

  it "压缩复杂的JSON数据" $ do
    compressTest "sample-2" `shouldReturn` True

  it "压缩带注释的JSON数据" $ do
    compressTest "sample-3" `shouldReturn` True

