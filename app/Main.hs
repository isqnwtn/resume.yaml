module Main (main) where

import LatexGen
import Data.Text (unpack)
import System.IO
import Options.Applicative

data Arguments = Arguments
  { outFile :: String
  , display :: Bool
  }

argParser :: Parser Arguments
argParser = Arguments
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "OUTPUT"
    <> help "output file"
    )
  <*> switch
    ( long "display"
    <> short 'd'
    <> help "whether to display the output in terminal"
    )

main :: IO ()
main = do
  latex <- someFunc
  args <- execParser opts
  texFile <- openFile (outFile args) WriteMode
  let latexString = latex
  hPutStrLn texFile latexString
  hClose texFile
  putStrLn latexString
    where
      opts = info (argParser <**> helper)
        ( fullDesc
        <> progDesc "Create LaTex Resume files"
        <> header   "Resumeow")
