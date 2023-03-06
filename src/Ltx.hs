{-# LANGUAGE GADTs #-}
module Ltx
  (test)
where

class Linable a where
  linn :: a -> Line String

class Renderable a where
  rendr :: a -> String

class Modulable a where
  toModName :: a -> Line String
  toMod :: a -> Line String

data Latex a where
  Empty    :: Latex a
  LX       :: (Linable a) => a -> Latex a
  (:#>>)   :: Latex a -> Latex a -> Latex a
  (:<&>)   :: (Modulable a) => a -> Latex a -> Latex a

infixr 8 :#>>
infixr 6 :<&>

data Line a where
  Str     :: a -> Line a
  Slash   :: a -> Line a
  Curl    :: a -> Line a
  Square  :: a -> Line a
  (:<@>)  :: Line a -> Line a -> Line a

infixr 5 :<@>

instance Renderable String where
  rendr = id

instance (Renderable a) => (Renderable (Line a)) where
  rendr (Str   x) = rendr x
  rendr (Slash x) = "\\" <> (rendr x)
  rendr (Curl  x) = "{"<>rendr x<>"}"
  rendr (Square x) = "["<>rendr x<>"]"
  rendr ( x :<@> y) = (rendr x)<>(rendr y)

data Ltx where
  Opn :: LtxModOpn -> Ltx
  Cld :: LtxModCld -> Ltx

data LtxModOpn
  = DocClass String
  | Package [String] String
  | DefineColor String (Int,Int,Int)
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

test :: IO String
test = do
  let bod = (Cld Document) :<&> Empty :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
         :#>> (LX (Opn (Package [] "tabularx")))
         :#>> (LX (Opn (Package [] "graphicx")))
         :#>> bod
  let lns = map rendr li
  return $ unlines lns
