-- |
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Latex
  ( Render(..)
  , Tag(..)
  , EnclosedList(..)
  , Obj(..)
  )where

import Prelude hiding (unlines,concat)
import Data.Text hiding (map)

class Render a where
  render:: a -> Text

instance Render Text where
  render = id

instance Render () where
  render _ = ""

data Tag a b c where
  T :: a -> c      -> Tag a () c
  S :: a           -> Tag a () ()
  F :: a -> b -> c -> Tag a b c
instance (Render a,Render b,Render c) => Render (Tag a b c) where
  render (T name curl   ) = "\\"<>(render name)<>render curl
  render (S name        ) = "\\"<>(render name)
  render (F name sq curl) = "\\"<>(render name)<>(render sq)<>(render curl)


data EnclosedList a = EC [a]
                    | ES [a]
instance (Render a) => Render (EnclosedList a) where
  render (EC l) = concat $ map (renderSingle . render ) l
    where
      renderSingle x = "{" <> x <> "}"
  render (ES l) = "[" <> ( intercalate "," (map render l)  ) <> "]"

data Obj a = O Text a
instance (Render a) => Render (Obj a) where
  render (O objName body)
    = unlines
    [ render (T "begin" (EC [objName]) :: Tag Text () (EnclosedList Text))
    , render body
    , render (T "end" (EC [objName]) :: Tag Text () (EnclosedList Text))
    ]
