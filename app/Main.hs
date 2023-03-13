module Main (main) where

import Gen.Gen
import Info
import Data.Text (unpack)
import System.IO
import Options.Applicative
import Data.Yaml (decodeFileEither, ParseException)

data Arguments = Arguments
  { outFile :: String
  , inFile :: String
  }

argParser :: Parser Arguments
argParser = Arguments
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "OUTPUT"
    <> help "output file"
    )
  <*> strOption
    ( long "input-file"
    <> short 'i'
    <> help "input file"
    )

main :: IO ()
main = do
  args <- execParser opts

  resumeData <- decodeFileEither (inFile args) :: IO (Either ParseException Resume)
  print resumeData

  case resumeData of
    Left err -> do
      print err
    Right rsm -> do
      texFile <- openFile (outFile args) WriteMode
      latex <- someFunc rsm
      let latexString = latex
      hPutStrLn texFile latexString
      hClose texFile
      putStrLn latexString
    where
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "Create LaTex Resume files"
        <> header   "Resumeow")
