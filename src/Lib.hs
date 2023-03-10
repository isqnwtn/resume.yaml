{-# LANGUAGE GADTs #-}

module Lib
  ( Ltx(..)
  , LtxModOpn(..)
  , LtxModCld(..)
  , Linable(..)
  , Modulable(..)
  , toLines
  ) where

import Latex
import Data.List (intercalate)

data Ltx where
  Opn :: LtxModOpn -> Ltx
  Cld :: LtxModCld -> Ltx

data LtxModOpn
  = DocClass String
  | Package [String] String
  | DefineColor String (Float,Float,Float)
  -- the following is for adhoc use cases
  | SLE (Line String)
data LtxModCld
  = Document
  | ColorBox (Line String)
  | MiniPage (Line String)
  | Tabularx (Line String)
  -- the following two are for adhoc use cases
  | InCurl   (Line String) -- latex elemenets enclosed inside a curly bracket
  | BegEnd   String (Line String) -- latex elements enclosed inside begin{stuff} end{stuff} format

instance Linable Ltx where
  linn (Opn (DocClass x)) = Slash "documentclass" :<@> Curl x
  linn (Opn (Package attr pkg)) =
    if (not . null) attr then
      Slash "usepackage"
      :<@> Square (intercalate "," attr):<@> Curl pkg
    else
      Slash "usepackage" :<@> Curl pkg
  linn (Opn (SLE x)) = x
  linn (Opn (DefineColor col (r,g,b)))
    = Slash "definecolor" :<@> Curl col :<@> Curl "rgb"
    :<@> Curl (intercalate "," [show r,show g,show b])
  linn _ = Str ""


instance Modulable Ltx where
  toModName (Cld Document) = Curl "document"
  toModName (Cld (MiniPage _)) = Curl "minipage"
  toModName (Cld (Tabularx _)) = Curl "tabularx"
  toModName (Cld (BegEnd name _)) = Curl name
  toModName _ = Str ""

  toMod (Cld Document) = Curl "document"
  toMod (Cld (MiniPage attr)) = Curl "minipage" :<@> attr
  toMod (Cld (Tabularx attr)) = Curl "tabularx" :<@> attr
  toMod (Cld (ColorBox attr)) = Str "colorbox" :<@> attr
  toMod (Cld (InCurl name))   = name
  toMod (Cld (BegEnd name attr)) = Curl name :<@> attr
  toMod _ = Str ""


toLines :: Latex a -> [Line String]
toLines (Empty) = [Str ""]
toLines (LX e)  = [linn e]
toLines (i :#>> rest) = (toLines i) <> (toLines rest)
toLines (modu :<&> body) =  [(Slash "begin") :<@> (toMod modu)] <> (toLines body)
                          <> [(Slash "end"  ) :<@> (toModName modu)]
toLines (modu :<^> body) = [(Slash "") :<@> (toMod modu) :<@> Str "{%"]
                        <> (toLines body)
                        <> [Str "}%"]
