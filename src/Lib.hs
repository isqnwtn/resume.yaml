{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where

import Prelude hiding (unlines,concat)
import Data.Text hiding (map)

class Render a where
  render:: a -> Text

instance Render Text where
  render = id

data Tag a b where
  T :: a -> b -> Tag a b
  S :: a      -> Tag a ()

data EnclosedList a = EC  [a]
                    | ES [a]
instance (Render a) => Render (EnclosedList a) where
  render (EC l) = concat $ map (renderSingle . render ) l
    where
      renderSingle x = "{" <> x <> "}"
  render (ES l) = "[" <> ( intercalate "," (map render l)  ) <> "]"

instance (Render a,Render b) => Render (Tag a b) where
  render (T a b) = "\\"<>(render a)<>render b
  render (S a  ) = "\\"<>(render a)

data Obj a = O Text a
instance (Render a) => Render (Obj a) where
  render (O objName body) = unlines
                       [ render (T "begin" (EC [objName]) :: Tag Text (EnclosedList Text))
                       , (render body)
                       , render (T "end" (EC [objName]) :: Tag Text (EnclosedList Text))
                       ]

data Document a = D a
instance (Render a) => Render (Document a) where
  render (D body) = render (O "document" body)


someFunc :: IO ()
someFunc = putStrLn $ unpack $
  render $ (D "hello world" :: Document Text)
