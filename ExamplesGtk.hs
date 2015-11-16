module ExamplesGtk
where

import qualified Control.Monad as Mo

import qualified Accidental as A
import qualified Draw as D
import qualified DrawCairo as Dc
import qualified Examples as E
import qualified GuiGtk as G
import qualified Music as M
import qualified Pitch as P

blocks :: IO ()
blocks = display "blocks" D.defDrawParm drawing
    where
        drawing :: D.Drawing
        drawing =
            D.Translate 0 200 $
                D.overlay [D.staff
                    , M.draw2 M.defLevel1Parm [
                        M.ClefChange M.ClefG
                        , M.KeySignature 7
                        , M.TimeSignature 12345 67890
                        , M.ENote M.D1 (P.MkPitch P.F A.sharp 8)
                        , M.ENote M.D1 (P.MkPitch P.G A.flat 8)
                        , M.ENote M.D1 (P.MkPitch P.A A.natural 8)
                        , M.ENote M.D1 (P.MkPitch P.B A.none 8)
                        , M.ENote M.D_2 (P.MkPitch P.B A.none 8)
                        , M.ENote M.D_4 (P.MkPitch P.B A.none 8)
                        , M.ENote M.D_4 (P.MkPitch P.B A.none 9)
                        , M.ENote M.D1 (P.MkPitch P.C A.none 10)
                        , M.ENote M.D_4 (P.MkPitch P.C A.none 6)
                        , M.ENote M.D_4 (P.MkPitch P.D A.none 6)
                        , M.ENote M.D_4 (P.MkPitch P.E A.none 6)
                        , M.ENote M.D_4 (P.MkPitch P.C A.none 8)
                        , M.ENote M.D_4 (P.MkPitch P.B A.none 7)
                        , M.ENote M.D_4 (P.MkPitch P.D A.none 8)
                        , M.ERest M.D1
                        , M.ERest M.D_2
                        , M.ERest M.D_4
                        , M.ERest M.D_8
                        , M.ERest M.D_16
                        , M.ERest M.D_32
                        , M.ERest M.D_64
                    ]
                    ]

amazingGrace :: IO ()
amazingGrace = display "Amazing Grace" D.numberedDrawParm E.amazingGraceDrawing

display :: String -> D.Param -> D.Drawing -> IO ()
display title parm drawing = G.run $ do
    w <- G.window >>= G.setTitle title
    c <- G.canvasWith $
        Dc.draw parm drawing
    Mo.void $ return ()
        >> G.add c w
        >> G.setSizeRequest 640 480 c
    G.showAll w
    G.onDestroy w G.quit
