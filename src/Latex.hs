{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE FlexibleInstances #-}

module Latex
  ( Render(..)
  , Linable(..)
  , Modulable(..)
  , Latex(..)
  , Line(..)
  )where

class Linable a where
  linn :: a -> Line String

class Render a where
  render :: a -> String

class Modulable a where
  toModName :: a -> Line String
  toMod :: a -> Line String

data Latex a where
  Empty    :: Latex a
  LX       :: (Linable a) => a -> Latex a
  (:#>>)   :: Latex a -> Latex a -> Latex a
  (:<&>)   :: (Modulable a) => a -> Latex a -> Latex a
  (:<^>)   :: (Modulable a) => a -> Latex a -> Latex a

infixr 8 :#>>
infixr 6 :<&>
infixr 5 :<^>

data Line a where
  Str     :: a -> Line a
  Slash   :: a -> Line a
  Curl    :: a -> Line a
  Square  :: a -> Line a
  (:<@>)  :: Line a -> Line a -> Line a

infixr 5 :<@>

instance Render String where
  render = id

instance (Render a) => (Render (Line a)) where
  render (Str   x) = render x
  render (Slash x) = "\\" <> (render x)
  render (Curl  x) = "{"<>render x<>"}"
  render (Square x) = "["<>render x<>"]"
  render ( x :<@> y) = (render x)<>(render y)
