-- |

module LatexGen
 (someFunc)
where

import Latex
import Lib

someFunc :: IO String
someFunc = do
  let bod = (Cld Document) :<&> ( header :#>> resumeBody) :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
        :#>> (LX (Opn (Package [] "graphicx")))
        :#>> (LX (Opn (Package ["a4paper","margin=0in"] "geometry")))
        :#>> (LX (Opn (Package ["svgnames","table"] "xcolor")))
        :#>> (LX (Opn (Package [] "tabularx")))
        :#>> bod
  let lns = map render li
  return $ unlines lns

resumeBody :: Latex Ltx
resumeBody =
  let tab = (Cld (Tabularx (Str "table"))) :<&> Empty
  in (Empty)

header :: Latex Ltx
header =
  let defcol = LX (Opn (DefineColor "lightRed" (1.0,0.85,0.9)))
      noindent = LX (Opn (SLE $ Slash "noindent"))
      minipage
        = (Cld (MiniPage (Curl "1.0\\textwidth")))
        :<&> (   (LX $ Opn $ SLE $ Slash "vspace" :<@> Curl "2cm" )
            :#>> (LX $ Opn $ SLE $ Slash "Huge")
            :#>> (LX $ Opn $ SLE $ Slash "centering")
            :#>> (LX $ Opn $ SLE $ Str "Name name")
            :#>> (LX $ Opn $ SLE $ Slash "rule" :<@> Curl "\\linewidth" :<@> Curl "1pt")
            )
      colbox = (Cld (ColorBox (Curl "lightRed"))) :<^> minipage
   in (defcol :#>> noindent :#>> colbox)
