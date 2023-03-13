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

concatLtx :: [Latex a] -> Latex a
concatLtx (x:xs) = x :#>> (concatLtx xs)
concatLtx [] = Empty
