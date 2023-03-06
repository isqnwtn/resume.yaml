{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
    ( someFunc
    ) where

import Prelude hiding (unlines,concat)
import Data.Text hiding (map)
import Data.String (fromString)
import Latex

class (Render b,Attribute a) => LatexObj a b  where
  toObj :: (a,b) -> Obj a b

data Document a = D a
instance (Render a) => Render (Document a) where
  render (D body) = render (O (NM ("document")) body)

data Latex a = LTX
  { docclass :: Text
  , packages :: [Package]
  , document :: Document a
  }
instance (Render a) => Render (Latex a) where
  render LTX{..} = unlines $
       [ render $ OS (NMCU "documentclass" ["article"])]
    <> (map render packages)
    <> [render document]

data Package = PKG
  { pkgname :: Text
  , attrib  :: Maybe [Text]
  }
instance Render Package where
  render PKG{..} = case attrib of
    Nothing -> render $  OS (NMCU "usepackage" [pkgname])
    Just a  -> render $  OS (NMALL "usepackage" a [pkgname])

someFunc :: IO Text
someFunc = return $
  render $ LTX
  { docclass = "article"::Text
  ,packages = [ PKG "graphicx" Nothing
              , PKG "tabularx" Nothing
              ]
  ,document = D ("Hello world"::Text)
  }
