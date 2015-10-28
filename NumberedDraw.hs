module NumberedDraw
where

import qualified Data.List as L

import qualified Draw as D
import qualified Glyph as G
import qualified Pitch as Pd
import qualified Rect as R

-- * Setting numbered notation into a table

{- |
The output is a row of 'D.Drawing's.
-}
beamRow :: [NumBeam] -> [D.Drawing]
beamRow [] = []
beamRow (0 : ns) = D.Empty : beamRow ns
beamRow (n : ns) = D.GetCellBounds (\ cell -> D.overlay (L.intersperse (D.VGap 4) (replicate n $ D.HLine $ R.width cell))) : beamRow ns

{- |
The output is a row of 'D.Drawing's.
-}
barElemsRow :: [BarElem] -> [D.Drawing]
barElemsRow = map f
    where
        f Beat = D.char G.bullet
        f Rest = D.char '0'
        f (Note cls oct) = note cls oct
        note :: Pd.Doremi -> Pd.Octave -> D.Drawing
        note cls oct =
            octaveDots oct $ D.Textual (show $ Pd.doremiNumber cls)
            where
                -- octaveDots n d adds n octave dots to drawing d.
                octaveDots n d | n >= 0 = upperDots n `D.above` d
                octaveDots n d | otherwise = lowerDots (negate n) `D.below` d
                upperDots n =
                    if n <= 0
                        then D.Empty
                        else (D.char G.bullet `D.above` D.VGap dotSpacing) `D.above` upperDots (n - 1)
                lowerDots n =
                    if n <= 0
                        then D.Empty
                        else (D.char G.bullet `D.below` D.VGap dotSpacing) `D.below` lowerDots (n - 1)
                dotSpacing :: D.UserUnit
                dotSpacing = 4

-- * Drawing inside a bar

{- |
An inhabitant of this type corresponds
to a column in the row in the drawing of a voice.

This does not deal with beaming.
-}
data BarElem
    -- | a dot that adds one beat to the duration of the previous event
    = Beat
    -- | a one-beat rest
    | Rest
    -- | a one-beat note
    | Note Pd.Doremi Pd.Octave
    deriving (Show)

-- | A 'NumBeam' describes the number of beams above a column.
type NumBeam = Int
