module Examples
where

import qualified Control.Monad as Mo

import qualified Accidental as A
import qualified Draw as D
import qualified GUI as G
import qualified Lyrics as L
import qualified Numbered as N
import qualified NumberedDraw as Nd
import qualified NumberedTable as Nt
import qualified Pitch as P
import qualified Table as T
import qualified Voice as V

import qualified Graphics.UI.Threepenny as Gt

amazingGraceDrawing :: D.Drawing
amazingGraceDrawing = D.Translate 100 200 $ D.Table table
    where
        table = T.prependColumn colMinHeights $ T.fromRowList [rowMinWidths, upperBeaming, upper, lyrics]
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
        colMinHeights = replicate (length upper) (D.Gap 0 32)
        rowMinWidths = replicate (length upper) (D.HGap 40)
        upper = Nd.barElemsRow $ N.translate upperMusic
        upperBeaming = Nd.beamRow $ N.beaming upperMusic
        lyrics = L.row [
            L.Syllable "A", L.Syllable "ma", L.Dash, L.Syllable "zing", L.Underline
            , L.Syllable "grace!", L.Space, L.Syllable "How", L.Syllable "sweet", L.Underline
            , L.Syllable "the", L.Syllable "sound"
            ]

-- * Example

exampleTable :: T.Table T.RowMajor Nt.Cell
exampleTable = id
    . T.transform
    . T.juxtapose
    . map T.transform
    . map T.reverseRows
    . T.equalizeMajor Nt.empty
    . map T.reverseRows
    $
    [
        Nt.eventTable $ V.expand $ V.note (5/2) P.Do A.none 2
        , Nt.eventTable $ V.expand $ V.note (5 + 3/4) P.Re A.none 1
        , Nt.eventTable $ V.expand $ V.note (7 + 15/16) P.Mi A.none 0
        , Nt.eventTable $ V.expand $ V.note (2 + 63/64) P.Mi A.none (-3)
    ]

-- | See 'G.threepennyExample' for the port number.
threepenny :: IO ()
threepenny = G.threepennyExample $ \ window -> do
    Mo.void $ Gt.getBody window Gt.#+ [Nt.render exampleTable]
