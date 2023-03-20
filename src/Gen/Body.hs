{-# LANGUAGE RecordWildCards #-}

module Gen.Body
  ( resumeBody
  , header
  ) where

import Gen.Common
import Gen.Work
import Gen.Skills
import Gen.Education
import Gen.Projects
import Latex
import Info
import Lib
import Data.Text (unpack,pack)

resumeBody :: Resume -> Latex Ltx
resumeBody Resume{..} =
  let left = (workExp workExperience)
       :#>> (education educationInfo)
      right = renderSkills skills
       :#>> (sle $ Slash "newline")
       :#>> (sle $ Slash "vspace" :<@> Curl "1cm")
       :#>> (projectSection projects)
  in table (1.0,1.0) "white" (left >&< right)


header :: PersonalInfo -> Latex Ltx
header pInfo@PersonalInfo{..} =
  let defcol = LX (Opn (DefineColor "lightBlue" (0.9,0.9,1.0)))
      noindent = sle $ Slash "noindent"
      left = sle $ Str ""
      right = (sle $ Slash "Huge")
            :#>> (sle $ Str $ unpack name)
            :#>> (sle $ Slash "vspace" :<@> Curl "0.2cm")
            -- position
            :#>> (sle $ Slash "normalsize")
            :#>> (sle $ Slash "newline")
            :#>> (sle $ Slash "textit" :<@> (Curl $ unpack title) )
            :#>> (sle $ Slash "vspace" :<@> Curl "0.2cm")

            -- description
            :#>> (sle $ Slash "newline")
            :#>> (sle $ Str $ unpack description)
      minipage
        = (Cld (MiniPage (Curl "1.0\\textwidth")))
        :<&> (   (sle $ Slash "vspace" :<@> Curl "0.5cm" )
            :#>> table (0.25,1.75) "lightBlue" (left >&< right)
            -- :#>> (sle $ Slash "rule" :<@> Curl "\\linewidth" :<@> Curl "4pt")
            :#>> (sle $ Slash "vspace" :<@> Curl "0.1cm")
            )
      colbox = (Cld (ColorBox (Curl "lightBlue"))) :<^> minipage
      hrule = sle $ Slash "rule" :<@> Curl "\\textwidth" :<@> Curl "0.4pt"
   in (defcol :#>> noindent :#>> colbox :#>> (personalInfoBox pInfo) :#>> hrule)

personalInfoBox :: PersonalInfo -> Latex Ltx
personalInfoBox PersonalInfo{..} =
  let defcol = LX (Opn (DefineColor "kindaDarkBlue" (0.8,0.8,1.0)))
      infoBox icon info = sle $ (Slash "faIcon") :<@> (Curl icon) :<@> (Str $ " " <> unpack info)
      infoRow1 = (infoBox "envelope" email) >&< (infoBox "phone" phoneNum)
      infoRow2 = (infoBox "map-marker-alt" location) >&< (infoBox "globe"  website)
      infoRow3 = (infoBox "github" github) >&< (infoBox "linkedin" $ pack "")
      noindent = sle $ Slash "noindent"
      minipage
        = (Cld (MiniPage (Curl "1.0\\textwidth")))
        :<&> (   (sle $ Slash "vspace" :<@> Curl "0.2cm" )
            :#>> table (1.0,1.0) "kindaDarkBlue" (infoRow1 <//> infoRow2 <//> infoRow3)
            -- :#>> (sle $ Slash "rule" :<@> Curl "\\linewidth" :<@> Curl "4pt")
            :#>> (sle $ Slash "vspace" :<@> Curl "0.2cm")
            )
      colbox = (Cld (ColorBox (Curl "kindaDarkBlue"))) :<^> minipage
  in (defcol :#>> noindent :#>> colbox)
