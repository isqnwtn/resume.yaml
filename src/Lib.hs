{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( someFunc
    ) where

import Prelude hiding (unlines,concat)
import Data.Text hiding (map)
import Latex

data Document a = D a
instance (Render a) => Render (Document a) where
  render (D body) = render (O (NM ("document" :: Text)) body)

data Latex a = LTX
  { docclass :: Text
  , packages :: [Package]
  , document :: Document a
  }
instance (Render a) => Render (Latex a) where
  render LTX{..} = unlines $
       [ render $ T ("documentclass"::Text) (EC [docclass])]
    <> (map render packages)
    <> [render document]

data Package = PKG
  { pkgname :: Text
  , attrib  :: Maybe [Text]
  }
instance Render Package where
  render PKG{..} = case attrib of
    Nothing -> render $  T ("usepackage"::Text) (EC [pkgname])
    Just a  -> render $  F ("usepackage"::Text) (ES a) (EC [pkgname])

someFunc :: IO Text
someFunc = return $
  render $ LTX
  { docclass = "article"::Text
  ,packages = [ PKG ("graphicx"::Text) Nothing
              , PKG ("tabularx"::Text) Nothing
              ]
  ,document = D ("Hello world"::Text)
  }
