{-# LANGUAGE RecordWildCards #-}

module Gen.Body
  ( resumeBody )
where

import Gen.Common
import Gen.Work
import Latex
import Info
import Lib
import Data.Text (unpack)

resumeBody :: Resume -> Latex Ltx
resumeBody rsm@Resume{..} =
  let tab = ( Cld $ Tabularx $ Curl "\\linewidth" :<@> tabattr ) :<&> tableBody rsm

      tabhsize :: Double -> String
      tabhsize x = render $ Curl $ render $ Slash "hsize" :<@> Str ("="<>show x) :<@> Slash "hsize"

      tabattr = Curl $ "|>" <> tabhsize 1.0 <> "X|>" <> tabhsize 1.0 <> "X|"

      lenset = (sle $ Slash "setlength" :<@> Curl "\\extrarowheight" :<@> Curl "2pt")
          :#>> (sle $ Slash "setlength" :<@> Curl "\\arrayrulewidth" :<@> Curl "10pt")
      arrayrulecolor = sle $ Slash "arrayrulecolor" :<@> Curl "white"
      noindent = sle $ Slash "noindent"
  in (noindent :#>> lenset :#>> arrayrulecolor :#>> tab)

tableBody :: Resume -> Latex Ltx
tableBody Resume{..} = workExp workExperience
       :#>> (sle $ Str "&")
       :#>> (sle $ Str "meow meow")
       :#>> Empty

