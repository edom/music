module Music
where

import qualified Data.List as L
import qualified Data.Monoid as Mo

import Draw
import qualified Accidental as A
import qualified Glyph as Gl
import qualified Rect as R

-- Music notation editor

{- |
Undo-redo.
-}
data Action
    = Put Event Location
    | Select Span
    deriving (Show, Read, Eq)
-- assume single staff, 5-bar

-- apply (x <> inverse x) = id
-- apply (inverse x <> x) = id
inverse :: Action -> Action
inverse = undefined

{- |
Pitch class C octave number 0 is C0 (1 Hz in C256 tuning).
Twelve-tone equal temperament.

The octave number of middle C is 8.
-}
type OctaveNumber = Int

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
draw1 :: Level1Parm -> Bar -> Drawing
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
            ERest D1 -> spaceDown 1 (char Gl.rest1) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ERest D_2 -> spaceDown 1.5 (char Gl.rest_2) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ERest D_4 -> spaceDown 2 (char Gl.rest_4) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ERest D_8 -> spaceDown 2 (char Gl.rest_8) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ERest D_16 -> spaceDown 2 (char Gl.rest_16) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ERest D_32 -> spaceDown 2 (char Gl.rest_32) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ERest D_64 -> spaceDown 2 (char Gl.rest_64) Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ClefChange ClefF -> spaceDown 1 (char Gl.clefF) Mo.<> spaceRight spaceFromClefLeft (f xs)
            ClefChange ClefG ->
                let
                    d = char Gl.clefG
                in
                    GetBounds d $ \ boundRect -> spaceDown 3 d Mo.<> Translate (R.width boundRect) 0 (f xs)
            KeySignature nsharps | nsharps > 0 ->
                foldr
                    (\ small big -> spaceDown (fromIntegral small / 2) (char Gl.sharp) Mo.<> spaceRight 1 big)
                    Mo.mempty
                    (take nsharps sharpedHalfSpaces)
                Mo.<> spaceRight spaceFromKeySignatureLeft (f xs)
            TimeSignature num den ->
                spaceDown 1 (Textual $ Gl.timeSigGlyphs num)
                Mo.<> spaceDown 3 (Textual $ Gl.timeSigGlyphs den)
                Mo.<> spaceRight spaceFromNoteLeft (f xs)
            ENote dur pitch ->
                let
                    MkPitch _ mbAcci _ = pitch
                    halfSpaceDiff = pitchHalfSpace gclefTopmostLine - pitchHalfSpace pitch
                    down = fromIntegral halfSpaceDiff / 2
                    drawMbAcci = maybe Mo.mempty (spaceRight accidentalSpace . char . A.glyph)
                    drawHead = char (headGlyph dur)
                    numUpperLedgers =
                        if halfSpaceDiff <= -2
                            then negate halfSpaceDiff `div` 2
                            else 0
                    numLowerLedgers =
                        if halfSpaceDiff >= 8 -- the number of spaces from the topmost staff line to the first lower ledger line
                            then (halfSpaceDiff - 8) `div` 2
                            else 0
                    drawUpperLedgers =
                        GetBounds drawHead $ \ boundRect ->
                        overlay $ flip map [1 .. numUpperLedgers] $ \ i ->
                        spaceDown (fromIntegral $ negate i) $
                        let
                            headWidth = R.width boundRect
                            ledgerWidth = 1.5 * headWidth
                        in
                            Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ HLine ledgerWidth
                    drawLowerLedgers =
                        GetBounds drawHead $ \ boundRect ->
                        overlay $ flip map [1 .. numLowerLedgers] $ \ i ->
                        spaceDown (fromIntegral $ 5 + i - 1) $
                        let
                            headWidth = R.width boundRect
                            ledgerWidth = 2 * headWidth
                        in
                            Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ HLine ledgerWidth
                    drawAcciAndHead = drawMbAcci mbAcci Mo.<> drawHead
                in
                    spaceDown down drawAcciAndHead
                    Mo.<> drawUpperLedgers
                    Mo.<> drawLowerLedgers
                    Mo.<> spaceRight spaceFromNoteLeft (f xs)
            _ -> f xs
        gclefTopmostLine = MkPitch F Nothing 9

-- | Enhancement of draw1 using automatic layout.
draw2 :: Level1Parm -> Bar -> Drawing
-- draw2 (MkLevel1Parm) = juxtapose . L.intersperse (HGap 20) . map f
draw2 (MkLevel1Parm) = HFit 1360 . L.intersperse (HGap 20) . map f
    where
        accidentalSpace = -1.5
        headGlyph x = case x of
            D1 -> Gl.head1
            D_2 -> Gl.head_2
            _ -> Gl.head_4
        f (ERest D1) = spaceDown 1 (char Gl.rest1)
        f (ERest D_2) = spaceDown 1.5 (char Gl.rest_2)
        f (ERest D_4) = spaceDown 2 (char Gl.rest_4)
        f (ClefChange ClefG) = spaceDown 3 $ char Gl.clefG
        f (ENote dur pitch) =
            spaceDown down drawAcciAndHead
            Mo.<> drawUpperLedgers
            Mo.<> drawLowerLedgers
            where
                MkPitch _ mbAcci _ = pitch
                halfSpaceDiff = pitchHalfSpace gclefTopmostLine - pitchHalfSpace pitch
                down = fromIntegral halfSpaceDiff / 2
                drawMbAcci = maybe Mo.mempty (spaceRight accidentalSpace . char . A.glyph)
                drawHead = char (headGlyph dur)
                numUpperLedgers =
                    if halfSpaceDiff <= -2
                        then negate halfSpaceDiff `div` 2
                        else 0
                numLowerLedgers =
                    if halfSpaceDiff >= 8 -- the number of spaces from the topmost staff line to the first lower ledger line
                        then (halfSpaceDiff - 8) `div` 2
                        else 0
                drawUpperLedgers =
                    GetBounds drawHead $ \ boundRect ->
                    overlay $ flip map [1 .. numUpperLedgers] $ \ i ->
                    spaceDown (fromIntegral $ negate i) $
                    let
                        headWidth = R.width boundRect
                        ledgerWidth = 1.5 * headWidth
                    in
                        Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ HLine ledgerWidth
                drawLowerLedgers =
                    GetBounds drawHead $ \ boundRect ->
                    overlay $ flip map [1 .. numLowerLedgers] $ \ i ->
                    spaceDown (fromIntegral $ 5 + i - 1) $
                    let
                        headWidth = R.width boundRect
                        ledgerWidth = 2 * headWidth
                    in
                        Translate (negate $ (ledgerWidth - headWidth) / 2) 0 $ HLine ledgerWidth
                drawAcciAndHead = drawMbAcci mbAcci Mo.<> drawHead
        f _ = Empty
        gclefTopmostLine = MkPitch F Nothing 9

-- | Twelve-tone system.
data Pitch
    = MkPitch PitchClass (Maybe A.Accidental) OctaveNumber
    deriving (Show, Read, Eq)

pitchHalfSpace :: Pitch -> Int
pitchHalfSpace (MkPitch pitchClass _ octave) =
    fromEnum pitchClass + 7 * octave

pitchToNumber :: Pitch -> Int
pitchToNumber (MkPitch pitchClass maybeAccidental octave) =
    pitchClassNum pitchClass + maybe 0 A.offset maybeAccidental + 12 * octave
    where
        pitchClassNum x = case x of
            C -> 0
            D -> 2
            E -> 4
            F -> 5
            G -> 7
            A -> 9
            B -> 11

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
    | ENote Duration Pitch
    | KeySignature SharpCount
    | TimeSignature Int Int
    | Split Event Event -- two voices
    deriving (Show, Read, Eq)

-- | The unit is bar/measure.
data Duration = D4 | D2 | D1 | D_2 | D_4 | D_8 | D_16 | D_32 | D_64
    deriving (Show, Read, Eq)

-- | Twelve-tone equal temperament.
data PitchClass = C | D | E | F | G | A | B deriving (Show, Read, Eq, Enum)

type Size = Double
type MinSize = Double
