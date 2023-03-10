module Main (main) where

import LatexGen
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

  yamlFile <- decodeFileEither (inFile args) :: IO (Either ParseException Resume)
  print yamlFile

  texFile <- openFile (outFile args) WriteMode
  latex <- someFunc
  let latexString = latex
  hPutStrLn texFile latexString
  hClose texFile

  putStrLn latexString
    where
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "Create LaTex Resume files"
        <> header   "Resumeow")
