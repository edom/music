module Examples
where

import qualified Control.Monad as Mo

import qualified Accidental as A
import qualified Draw as D
import qualified DrawCairo as Dc
import qualified GUI as G
import qualified Lyrics as L
import qualified Music as M
import qualified Numbered as N
import qualified NumberedDraw as Nd
import qualified Pitch as P
import qualified Table as T

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
amazingGrace = display "Amazing Grace" D.numberedDrawParm drawing
    where
        drawing :: D.Drawing
        drawing = D.Translate 100 200 $ D.Table table
        table = T.prependColumn colMinHeights [rowMinWidths, upperBeaming, upper, lyrics]
        upperMusic = map (\ (d,p,o) -> N.note d p o) [
                (1, P.So, -1)
                , (2, P.Do, 0)
                , (1/2, P.Mi, 0)
                , (1/2, P.Do, 0)
                , (1, P.Mi, 0)
            ] ++ [N.rest 1]
            ++ map (\ (d,p,o) -> N.note d p o) [
                (1, P.Re, 0)
                , (2, P.Do, 0)
                , (1, P.La, -1)
                , (2, P.So, -1)
            ]
        colMinHeights = replicate (length upper) (D.VGap 32)
        rowMinWidths = replicate (length upper) (D.HGap 40)
        upper = Nd.barElemsRow $ N.translate upperMusic
        upperBeaming = Nd.beamRow $ N.beaming upperMusic
        lyrics = L.row [
            L.Syllable "A", L.Syllable "ma", L.Dash, L.Syllable "zing", L.Underline
            , L.Syllable "grace!", L.Space, L.Syllable "How", L.Syllable "sweet", L.Underline
            , L.Syllable "the", L.Syllable "sound"
            ]

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
