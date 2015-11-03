module Music
where

import qualified Data.List as L
import qualified Data.Monoid as Mo

import qualified Accidental as A
import qualified Draw as D
import qualified Glyph as Gl
import qualified Pitch as P
import qualified Rect as R

-- Music notation editor

{- |
The user does 'Action's.

An 'Action' can be undone.

An 'Action' transforms a 'Music' into another 'Music'.

A list of 'Action's is the difference between two 'Music'.

A save file contains a list of 'Action' to reconstruct the 'Music' from the empty 'Music'.

'show' and 'read' can be used for saving and loading.
-}
data Action
    = Put Event Location
    | Select Span
    deriving (Show, Read, Eq)

{- |
The module "Numbered" can transform this 'Music' into a Drawing, which can then be drawn.
-}
data Music
    = MkMusic
    deriving (Show, Read, Eq)

-- assume single staff, 5-bar

{- |
@
apply (x \<\> inverse x) = id
apply (inverse x \<\> x) = id
@
-}
apply :: Action -> Music -> Music
apply = undefined

inverse :: Action -> Action
inverse = undefined

-- | Negative for flats.
type SharpCount = Int

-- | The unit is bar/measure.
type Location = Rational

-- | Inclusive, exclusive.
data Span = MkSpan Location Location deriving (Show, Read, Eq)

data DClef
    = ClefF
    | ClefG
    | ClefC
    deriving (Show, Read, Eq)

data Position
    = MkPosition Int Int
    deriving (Show)

data Length
    = Space Double
    | User Double
    deriving (Show)

-- LEVEL-1

-- | Draw the contents of a bar.
draw1 :: Level1Parm -> Bar -> D.Drawing
draw1 (MkLevel1Parm) = f
    where
        spaceFromNoteLeft = 5
        accidentalSpace = -1.5
        spaceFromClefLeft = 5
        spaceFromKeySignatureLeft = 5
        headGlyph x = case x of
            D1 -> Gl.head1
            D_2 -> Gl.head_2
            _ -> Gl.head_4
        sharpedPitchClassNumbers :: [Int]
        sharpedPitchClassNumbers = [3,7..]
        topLinePitchClassNumber = 3
        sharpedHalfSpaces = map (\ x -> (7 + topLinePitchClassNumber - x) `mod` 7) sharpedPitchClassNumbers
        f [] = Mo.mempty
        f (x : xs) = case x of
            -- These rest glyphs have what baselines?
            ERest D1 -> D.spaceDown 1 (D.char Gl.rest1) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ERest D_2 -> D.spaceDown 1.5 (D.char Gl.rest_2) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ERest D_4 -> D.spaceDown 2 (D.char Gl.rest_4) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ERest D_8 -> D.spaceDown 2 (D.char Gl.rest_8) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ERest D_16 -> D.spaceDown 2 (D.char Gl.rest_16) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ERest D_32 -> D.spaceDown 2 (D.char Gl.rest_32) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ERest D_64 -> D.spaceDown 2 (D.char Gl.rest_64) Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ClefChange ClefF -> D.spaceDown 1 (D.char Gl.clefF) Mo.<> D.spaceRight spaceFromClefLeft (f xs)
            ClefChange ClefG ->
                let
                    d = D.char Gl.clefG
                in
                    D.Lambda $ D.GetBounds d $ \ boundRect -> D.spaceDown 3 d Mo.<> D.Translate (R.width boundRect) 0 (f xs)
            KeySignature nsharps | nsharps > 0 ->
                foldr
                    (\ small big -> D.spaceDown (fromIntegral small / 2) (D.char Gl.sharp) Mo.<> D.spaceRight 1 big)
                    Mo.mempty
                    (take nsharps sharpedHalfSpaces)
                Mo.<> D.spaceRight spaceFromKeySignatureLeft (f xs)
            TimeSignature num den ->
                D.spaceDown 1 (D.string $ Gl.timeSigGlyphs num)
                Mo.<> D.spaceDown 3 (D.string $ Gl.timeSigGlyphs den)
                Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            ENote dur pitch ->
                let
                    P.MkPitch _ mbAcci _ = pitch
                    halfSpaceDiff = P.diatoneNum gclefTopmostLine - P.diatoneNum pitch
                    down = fromIntegral halfSpaceDiff / 2
                    drawAcci = D.spaceRight accidentalSpace . D.char . A.glyph
                    drawHead = D.char (headGlyph dur)
                    numUpperLedgers =
                        if halfSpaceDiff <= -2
                            then negate halfSpaceDiff `div` 2
                            else 0
                    numLowerLedgers =
                        if halfSpaceDiff >= 8 -- the number of spaces from the topmost staff line to the first lower ledger line
                            then (halfSpaceDiff - 8) `div` 2
                            else 0
                    drawUpperLedgers =
                        D.Lambda $ D.GetBounds drawHead $ \ boundRect ->
                        D.overlay $ flip map [1 .. numUpperLedgers] $ \ i ->
                        D.spaceDown (fromIntegral $ negate i) $
                        let
                            headWidth = R.width boundRect
                            ledgerWidth = 1.5 * headWidth
                        in
                            D.Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ D.line ledgerWidth 0
                    drawLowerLedgers =
                        D.Lambda $ D.GetBounds drawHead $ \ boundRect ->
                        D.overlay $ flip map [1 .. numLowerLedgers] $ \ i ->
                        D.spaceDown (fromIntegral $ 5 + i - 1) $
                        let
                            headWidth = R.width boundRect
                            ledgerWidth = 2 * headWidth
                        in
                            D.Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ D.line ledgerWidth 0
                    drawAcciAndHead = drawAcci mbAcci Mo.<> drawHead
                in
                    D.spaceDown down drawAcciAndHead
                    Mo.<> drawUpperLedgers
                    Mo.<> drawLowerLedgers
                    Mo.<> D.spaceRight spaceFromNoteLeft (f xs)
            _ -> f xs
        gclefTopmostLine = P.MkPitch P.F A.none 9

-- | Enhancement of draw1 using automatic layout.
draw2 :: Level1Parm -> Bar -> D.Drawing
-- draw2 (MkLevel1Parm) = juxtapose . L.intersperse (HGap 20) . map f
draw2 (MkLevel1Parm) = D.HFit 1360 . L.intersperse (D.HGap 20) . map f
    where
        accidentalSpace = -1.5
        headGlyph x = case x of
            D1 -> Gl.head1
            D_2 -> Gl.head_2
            _ -> Gl.head_4
        f (ERest D1) = D.spaceDown 1 (D.char Gl.rest1)
        f (ERest D_2) = D.spaceDown 1.5 (D.char Gl.rest_2)
        f (ERest D_4) = D.spaceDown 2 (D.char Gl.rest_4)
        f (ClefChange ClefG) = D.spaceDown 3 $ D.char Gl.clefG
        f (ENote dur pitch) =
            D.spaceDown down drawAcciAndHead
            Mo.<> drawUpperLedgers
            Mo.<> drawLowerLedgers
            where
                P.MkPitch _ mbAcci _ = pitch
                halfSpaceDiff = P.diatoneNum gclefTopmostLine - P.diatoneNum pitch
                down = fromIntegral halfSpaceDiff / 2
                drawAcci = D.spaceRight accidentalSpace . D.char . A.glyph
                drawHead = D.char (headGlyph dur)
                numUpperLedgers =
                    if halfSpaceDiff <= -2
                        then negate halfSpaceDiff `div` 2
                        else 0
                numLowerLedgers =
                    if halfSpaceDiff >= 8 -- the number of spaces from the topmost staff line to the first lower ledger line
                        then (halfSpaceDiff - 8) `div` 2
                        else 0
                drawUpperLedgers =
                    D.Lambda $ D.GetBounds drawHead $ \ boundRect ->
                    D.overlay $ flip map [1 .. numUpperLedgers] $ \ i ->
                    D.spaceDown (fromIntegral $ negate i) $
                    let
                        headWidth = R.width boundRect
                        ledgerWidth = 1.5 * headWidth
                    in
                        D.Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ D.line ledgerWidth 0
                drawLowerLedgers =
                    D.Lambda $ D.GetBounds drawHead $ \ boundRect ->
                    D.overlay $ flip map [1 .. numLowerLedgers] $ \ i ->
                    D.spaceDown (fromIntegral $ 5 + i - 1) $
                    let
                        headWidth = R.width boundRect
                        ledgerWidth = 2 * headWidth
                    in
                        D.Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ D.line ledgerWidth 0
                drawAcciAndHead = drawAcci mbAcci Mo.<> drawHead
        f _ = D.Empty
        gclefTopmostLine = P.MkPitch P.F A.none 9

data Level1Parm
    = MkLevel1Parm
    deriving (Show, Read, Eq)

defLevel1Parm :: Level1Parm
defLevel1Parm = MkLevel1Parm

type Part = [Bar]
type Bar = [Event]

data Event
    = ClefChange DClef
    | ERest Duration
    | ENote Duration (P.Pitch P.Abc)
    | KeySignature SharpCount
    | TimeSignature Int Int
    | Split Event Event -- two voices
    deriving (Show, Read, Eq)

-- | The unit is bar/measure.
data Duration = D4 | D2 | D1 | D_2 | D_4 | D_8 | D_16 | D_32 | D_64
    deriving (Show, Read, Eq)

type Size = Double
type MinSize = Double
