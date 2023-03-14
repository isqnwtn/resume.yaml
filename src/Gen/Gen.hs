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
        :#>> pkg ["legalpaper","margin=0in"] "geometry"
        :#>> pkg ["svgnames","table"] "xcolor"
        :#>> pkg [] "tabularx"
        :#>> pkg [] "tikz"
        :#>> body
  let lns = map render li
  return $ unlines lns


header :: PersonalInfo -> Latex Ltx
header PersonalInfo{..} =
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
            :#>> table (0.25,1.75) "lightBlue" left right
            -- :#>> (sle $ Slash "rule" :<@> Curl "\\linewidth" :<@> Curl "4pt")
            :#>> (sle $ Slash "vspace" :<@> Curl "1cm")
            )
      colbox = (Cld (ColorBox (Curl "lightBlue"))) :<^> minipage
   in (defcol :#>> noindent :#>> colbox)


