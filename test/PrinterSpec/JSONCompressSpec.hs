module PrinterSpec.JSONCompressSpec (spec) where

import           JSONPrinter.Compress (jprint)
import           JSONReader           (jread)
import           System.Directory     (doesFileExist)
import           System.Environment   (getEnv)
import           System.FilePath      ((</>))
import           Test.Hspec

compress :: FilePath -> IO String
compress filename = do
  json₀ <- readFile filename
  return $ either id jprint (jread json₀)

compressTest :: FilePath -> IO Bool
compressTest filename = do
  storage <- getEnv "JSONR_TEST_DIRECTORY"
  let filename' = storage </> filename
  compressed <- compress filename'
  expected <- readFile (filename' ++ ".expected")
  return $ compressed == expected

spec = do
  it "压缩简单的JSON数据" $ do
    compressTest "sample-1" `shouldReturn` True

  it "压缩复杂的JSON数据" $ do
    compressTest "sample-2" `shouldReturn` True

  it "压缩带注释的JSON数据" $ do
    compressTest "sample-3" `shouldReturn` True

