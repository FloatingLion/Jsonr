module PrinterSpec.JSONPrettyCSpec where

import           JSONPrinter.PrettyC (jnPrint)
import           JSONReader          (jnRead)
import           PrinterSpec.Util
import           Test.Hspec

spec :: Spec
spec = do
  it "处理不带注释的JSON文件(A)" $ do
    prettySpec' "sample-1" `shouldReturn` True

  it "处理不带注释的JSON文件(B)" $ do
    prettySpec' "sample-2" `shouldReturn` True

  it "处理带简单注释的JSON文件" $ do
    prettySpec "sample-5" `shouldReturn` True

  it "处理带复杂注释的JSON文件" $ do
    prettySpec "sample-3" `shouldReturn` True

prettySpec  = makSpec ".prettyc.expected" (either id jnPrint . jnRead)
prettySpec' = makSpec ".pretty.expected" (either id jnPrint . jnRead)
