{-# LANGUAGE GADTs #-}

module Lib
    ( someFunc
    ) where

import Latex

data Ltx where
  Opn :: LtxModOpn -> Ltx
  Cld :: LtxModCld -> Ltx

data LtxModOpn
  = DocClass String
  | Package [String] String
  | DefineColor String (Float,Float,Float)
  | SingleLineElement String
data LtxModCld
  = Document
  | ColorBox String
  | MiniPage [String]
  | Tabularx [String]

instance Linable Ltx where
  linn (Opn (DocClass x)) = Slash "documentclass" :<@> Curl x
  linn (Opn (Package attr pkg)) = Slash "usepackage" :<@> Curl pkg
  linn (Opn (SingleLineElement x)) = Slash x
  linn _ = Str ""

instance Modulable Ltx where
  toModName (Cld Document) = Curl "document"
  toModName (Cld (MiniPage _)) = Curl "minipage"
  toModName (Cld (Tabularx _)) = Curl "tabularx"
  toModName _ = Str ""

  toMod = toModName

toLines :: Latex a -> [Line String]
toLines (Empty) = [Str ""]
toLines (LX e)  = [linn e]
toLines (i :#>> rest) = (toLines i) <> (toLines rest)
toLines (modu :<&> body) =  [(Slash "begin") :<@> (toMod modu)] <> (toLines body)
                          <> [(Slash "end"  ) :<@> (toModName modu)]

someFunc :: IO String
someFunc = do
  let tab = (Cld (Tabularx ["table"])) :<&> Empty
  let bod = (Cld Document) :<&> tab :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
        :#>> (LX (Opn (Package [] "tabularx")))
        :#>> (LX (Opn (Package [] "graphicx")))
        :#>> bod
  let lns = map render li
  return $ unlines lns
