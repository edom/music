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
                        , M.ENote M.D1 (M.MkPitch M.F (Just A.Sharp) 8)
                        , M.ENote M.D1 (M.MkPitch M.G (Just A.Flat) 8)
                        , M.ENote M.D1 (M.MkPitch M.A (Just A.Natural) 8)
                        , M.ENote M.D1 (M.MkPitch M.B Nothing 8)
                        , M.ENote M.D_2 (M.MkPitch M.B Nothing 8)
                        , M.ENote M.D_4 (M.MkPitch M.B Nothing 8)
                        , M.ENote M.D_4 (M.MkPitch M.B Nothing 9)
                        , M.ENote M.D1 (M.MkPitch M.C Nothing 10)
                        , M.ENote M.D_4 (M.MkPitch M.C Nothing 6)
                        , M.ENote M.D_4 (M.MkPitch M.D Nothing 6)
                        , M.ENote M.D_4 (M.MkPitch M.E Nothing 6)
                        , M.ENote M.D_4 (M.MkPitch M.C Nothing 8)
                        , M.ENote M.D_4 (M.MkPitch M.B Nothing 7)
                        , M.ENote M.D_4 (M.MkPitch M.D Nothing 8)
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
        upperMusic = [
                N.MNote (N.MkPitch N.So (-1)) 1
                , N.MNote (N.MkPitch N.Do 0) 2
                , N.MNote (N.MkPitch N.Mi 0) (1/2)
                , N.MNote (N.MkPitch N.Do 0) (1/2)
                , N.MNote (N.MkPitch N.Mi 0) 1
                , N.MRest 1
                , N.MNote (N.MkPitch N.Re 0) 1
                , N.MNote (N.MkPitch N.Do 0) 2
                , N.MNote (N.MkPitch N.La (-1)) 1
                , N.MNote (N.MkPitch N.So (-1)) 2
            ]
        colMinHeights = replicate (length upper) (D.VGap 32)
        rowMinWidths = replicate (length upper) (D.HGap 40)
        upper = N.barElemsRow $ N.translate N.defMusicParam upperMusic
        upperBeaming = N.beamRow $ N.musicBeaming upperMusic
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
