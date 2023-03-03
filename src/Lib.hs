{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Prelude hiding (unlines)
import Data.Text

class Render a where
  render:: a -> Text

instance Render Text where
  render = id

data Tag a b = T a b
instance (Render a,Render b) => Render (Tag a b) where
  render (T a b) = "\\"<>(render a)<>"{"<>render b<>"}"

data Document = D Text
instance Render Document where
  render (D body) = unlines
                  [ render (T "begin" "document":: Tag Text Text)
                  , body
                  , render (T "end" "document":: Tag Text Text)
                  ]


someFunc :: IO ()
someFunc = putStrLn $ unpack $
  render $ D "hello world"
