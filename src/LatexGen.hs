-- |

module LatexGen
 (someFunc)
where

import Latex
import Lib

someFunc :: IO String
someFunc = do

  let defcol = LX (Opn (DefineColor "lightRed" (1.0,0.85,0.9)))
  let colbox = (Cld (ColorBox (Curl "lightRed"))) :<^> Empty

  let tab = (Cld (Tabularx (Str "table"))) :<&> Empty

  let bod = (Cld Document) :<&> ( defcol :#>> colbox :#>> tab) :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
        :#>> (LX (Opn (Package [] "tabularx")))
        :#>> (LX (Opn (Package [] "graphicx")))
        :#>> bod
  let lns = map render li
  return $ unlines lns
