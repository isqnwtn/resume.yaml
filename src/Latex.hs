-- |
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Latex
  ( Render(..)
  , Tag(..)
  , EnclosedList(..)
  , Obj(..)
  , Name(..)
  )where

import Prelude hiding (unlines,concat)
import Data.Text hiding (map,null)

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

class Attribute a where
  attrib:: a -> (Text,[Text],[Text])

data Name a = NM a
instance (Render a) => Attribute (Name a) where
  attrib (NM a) = (render a,[],[])


data EnclosedList a = EC [a]
                    | ES [a]
instance (Render a) => Render (EnclosedList a) where
  render (EC l) = concat $ map (renderSingle . render ) l
    where
      renderSingle x = "{" <> x <> "}"
  render (ES l) = if (null l)
    then ""
    else "[" <> ( intercalate "," (map render l)  ) <> "]"

data Obj a b where
  O  :: a -> b -> Obj a b
  OS :: a -> Obj a ()
  CON :: (Obj a b) -> (Obj a b) -> (Obj a b)
instance (Attribute a, Render b) => Render (Obj a b) where
  render (O attr body)
    = unlines
    [ render (F "begin" (ES sqAttr) (EC $ [oname] <> curlAttr)
              :: Tag Text (EnclosedList Text) (EnclosedList Text))
    , render body
    , render (T "end" (EC [oname]) :: Tag Text () (EnclosedList Text))
    ]
    where
      (oname,sqAttr,curlAttr) = attrib attr
  render (OS attr) = render $ T oname $ EC curlstuff
    where
      (oname,_,curlstuff) = attrib attr
  render (CON obj1 obj2) = unlines $ [render obj1, render obj2]
