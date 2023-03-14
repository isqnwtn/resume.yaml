-- |

module Gen.Common where

import Lib
import Latex

-- Helper functions

-- create single line latex element
sle :: Line String -> Latex Ltx
sle a = LX $ Opn $ SLE a

-- create package
pkg :: [String] -> String -> Latex Ltx
pkg attr pname = LX $ Opn $ Package attr pname

-- | concatenate a list of Latex elements
concatLtx :: [Latex a] -> Latex a
concatLtx (x:xs) = x :#>> (concatLtx xs)
concatLtx [] = Empty

-- | Generate a table with predifined size
table :: (Float,Float) -- | table size
      -> String    -- | color
      -> Latex Ltx -- | left content of table
      -> Latex Ltx -- | right content of table
      -> Latex Ltx
table (lhs,rhs) color leftLtx rightLtx=
  let tab = ( Cld $ Tabularx $ Curl "\\linewidth" :<@> tabattr ) :<&> body
      body = leftLtx :#>> (sle $ Str "&") :#>> rightLtx
      tabhsize :: Float -> String
      tabhsize x = render $ Curl $ render $ Slash "hsize" :<@> Str ("="<>show x) :<@> Slash "hsize"

      tabattr = Curl $ "|>" <> tabhsize lhs <> "X|>" <> tabhsize rhs <> "X|"

      lenset = (sle $ Slash "setlength" :<@> Curl "\\extrarowheight" :<@> Curl "2pt")
          :#>> (sle $ Slash "setlength" :<@> Curl "\\arrayrulewidth" :<@> Curl "10pt")
      arrayrulecolor = sle $ Slash "arrayrulecolor" :<@> Curl color
      noindent = sle $ Slash "noindent"
  in (noindent :#>> lenset :#>> arrayrulecolor :#>> tab)
