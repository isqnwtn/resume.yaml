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
  let li = toLines $ LX (Opn (DocClass ["18pt"] "article"))
        :#>> pkg [] "graphicx"
        :#>> pkg ["legalpaper","margin=0in"] "geometry"
        :#>> pkg ["svgnames","table"] "xcolor"
        :#>> pkg [] "tabularx"
        :#>> pkg [] "pgf-pie"
        :#>> pkg [] "tikz"
        :#>> pkg [] "hyperref"
        :#>> pkg [] "fontawesome5"

        -- latex by default uses seriff font, we will change it for sans as a part of making resume
        :#>> (sle $ Slash "renewcommand" :<@> Curl "\\familydefault" :<@> Curl "\\sfdefault")

        -- add image folder
        :#>> (sle $ Slash "graphicspath" :<@> Curl "{./images/}")
        :#>> body
  let lns = map render li
  return $ unlines lns

