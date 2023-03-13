{-# LANGUAGE RecordWildCards #-}

module Gen.Gen
 (someFunc)
where

import Gen.Common
import Gen.Body
import Latex
import Info
import Lib
import Data.Text (unpack)

someFunc :: Resume -> IO String
someFunc resume@Resume{..} = do
  let body = (Cld Document) :<&> ( (header personalInfo) :#>> resumeBody resume) :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
        :#>> pkg [] "graphicx"
        :#>> pkg ["a4paper","margin=0in"] "geometry"
        :#>> pkg ["svgnames","table"] "xcolor"
        :#>> pkg [] "tabularx"
        :#>> body
  let lns = map render li
  return $ unlines lns

setSectionSpacing :: Latex Ltx
setSectionSpacing
  = let t = Slash "titlespacing"
        s = Slash "subsection"
        space = Curl "0pt plus 1pt minus 2pt"
    in (sle $ t :<@> s :<@> space)

header :: PersonalInfo -> Latex Ltx
header PersonalInfo{..} =
  let defcol = LX (Opn (DefineColor "lightRed" (1.0,0.85,0.9)))
      noindent = sle $ Slash "noindent"
      minipage
        = (Cld (MiniPage (Curl "1.0\\textwidth")))
        :<&> (   (sle $ Slash "vspace" :<@> Curl "0.5cm" )
            :#>> (sle $ Slash "Huge")
            :#>> (sle $ Str $ unpack name)
            -- :#>> (sle $ Slash "rule" :<@> Curl "\\linewidth" :<@> Curl "4pt")
            :#>> (sle $ Slash "vspace" :<@> Curl "1cm")
            )
      colbox = (Cld (ColorBox (Curl "lightRed"))) :<^> minipage
   in (defcol :#>> noindent :#>> colbox)


