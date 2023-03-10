
module LatexGen
 (someFunc)
where

import Latex
import Lib

someFunc :: IO String
someFunc = do
  let body = (Cld Document) :<&> ( header :#>> resumeBody) :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
        :#>> pkg [] "graphicx"
        :#>> pkg ["a4paper","margin=0in"] "geometry"
        :#>> pkg ["svgnames","table"] "xcolor"
        :#>> pkg [] "tabularx"
        :#>> body
  let lns = map render li
  return $ unlines lns

resumeBody :: Latex Ltx
resumeBody =
  let tab = ( Cld $ Tabularx $ Curl "\\linewidth" :<@> tabattr ) :<&> tableBody

      tabhsize :: Double -> String
      tabhsize x = render $ Curl $ render $ Slash "hsize" :<@> Str ("="<>show x) :<@> Slash "hsize"

      tabattr = Curl $ "|>" <> tabhsize 1.0 <> "X|>" <> tabhsize 1.0 <> "X|"

      lenset = (sle $ Slash "setlength" :<@> Curl "\\extrarowheight" :<@> Curl "2pt")
          :#>> (sle $ Slash "setlength" :<@> Curl "\\arrayrulewidth" :<@> Curl "10pt")
      arrayrulecolor = sle $ Slash "arrayrulecolor" :<@> Curl "white"
      noindent = sle $ Slash "noindent"
  in (noindent :#>> lenset :#>> arrayrulecolor :#>> tab)

tableBody :: Latex Ltx
tableBody = workExp
       :#>> (sle $ Str "&")
       :#>> (sle $ Str "meow meow")
       :#>> Empty

workExp :: Latex Ltx
workExp =
  let x = 3
  in (sle $ Slash "hline")
     :#>> (sle $ Slash "section*" :<@> Curl "Work Experience")

header :: Latex Ltx
header =
  let defcol = LX (Opn (DefineColor "lightRed" (1.0,0.85,0.9)))
      noindent = sle $ Slash "noindent"
      minipage
        = (Cld (MiniPage (Curl "1.0\\textwidth")))
        :<&> (   (sle $ Slash "vspace" :<@> Curl "0.5cm" )
            :#>> (sle $ Slash "Huge")
            :#>> (sle $ Str "Name name")
            -- :#>> (sle $ Slash "rule" :<@> Curl "\\linewidth" :<@> Curl "4pt")
            :#>> (sle $ Slash "vspace" :<@> Curl "1cm")
            )
      colbox = (Cld (ColorBox (Curl "lightRed"))) :<^> minipage
   in (defcol :#>> noindent :#>> colbox)


-- Helper functions

-- create single line latex element
sle :: Line String -> Latex Ltx
sle a = LX $ Opn $ SLE a

-- create package
pkg :: [String] -> String -> Latex Ltx
pkg attr pname = LX $ Opn $ Package attr pname
