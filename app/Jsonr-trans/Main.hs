module Main where

import           Control.Exception    (bracket)
import           Data.Semigroup       ((<>))
import qualified JSONPrinter.Compress as C (jnPrint)
import qualified JSONPrinter.Pretty   as P (jnPrint)
import qualified JSONPrinter.PrettyC  as PC (jnPrint)
import           JSONReader           (jnRead)
import           Options.Applicative
import           System.Directory     (doesFileExist)
import           System.IO

main = do
  opt <- execParser opts
  let ipath = inFilename opt
  ok <- doesFileExist ipath
  if not ok
    then putStrLn $ "文件" ++ ipath ++ "不存在\n  程序异常终止"
    else do
    icont <- readFile ipath
    case transform icont opt of
      Left err -> putStrLn $ "错误：" ++ err ++ "\n  程序异常终止"
      Right r  -> if null (outFilename opt)
                  then putStrLn r
                  else writeFile (outFilename opt) (r ++ "\n")

transform s opt
  = either Left `flip` jnRead s $ \ dat -> Right $
  if shouldCompress opt
  then C.jnPrint dat
  else if careComment opt
       then PC.jnPrint dat
       else P.jnPrint dat

data TransOptions = TransOptions
  { inFilename     :: FilePath
  , outFilename    :: FilePath
  , shouldCompress :: Bool
  , careComment    :: Bool }

transOptions :: Parser TransOptions
transOptions = TransOptions
  <$> argument str (metavar "INPUT"                                            <>
                    help "原始文件路径，文件必须存在且具有正确的JSON格式")
  <*> strOption (metavar "OUTPUT"                                              <>
                 help "目标文件路径，如果文件存在则会被覆盖。默认为标准输出流" <>
                 long "output"                                                 <>
                 value ""                                                      <>
                 short 'o')
  <*> switch (help "压缩JSON文件，如果该标志存在则 --comment 标志会被忽略"     <>
              long "minimize"                                                  <>
              short 'm')
  <*> switch (help "美化打印时保留注释，仅会调整部分注释的位置"                <>
              long "comment"                                                   <>
              short 'c')

opts = info (transOptions <**> helper)
      ( fullDesc
     <> progDesc "将 INPUT 中的内容美化/压缩至 OUTPUT 中"
     <> header "Jsonr-trans ;; JSON数据转换器" )
