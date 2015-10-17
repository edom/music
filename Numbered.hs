{- |

Indonesian numbered musical notation.

Mostly similar to this, but with some small differences:

See the Wikipedia article for <https://en.wikipedia.org/wiki/Numbered_musical_notation Numbered musical notation>.
-}
module Numbered
where

import qualified Data.List as L

import qualified Draw as D
import qualified Rect as R

-- * Setting numbered notation into a table

barElemsRow :: [BarElem] -> [D.Drawing]
barElemsRow = map f
    where
        f Beat = dot
        f Rest = D.char '0'
        f (Note (MkPitch cls oct)) = note cls oct

beamRow :: [NumBeam] -> [D.Drawing]
beamRow [] = []
beamRow (0 : ns) = D.Empty : beamRow ns
beamRow (n : ns) = D.GetCellBounds (\ cell -> D.overlay (L.intersperse (D.VGap 4) (replicate n $ D.HLine $ R.width cell))) : beamRow ns

-- * Drawing inside a bar

data BarElem
    -- | a dot that adds one beat to the duration of the previous event
    = Beat
    -- | a one-beat rest
    | Rest
    -- | a one-beat note
    | Note Pitch
    deriving (Show)

-- * Notes

type OctaveNumber = Int

note :: Class -> OctaveNumber -> D.Drawing
note cls oct =
    octaveDots oct $ D.Textual (show $ number cls)
    where
        -- octaveDots n d adds n octave dots to drawing d.
        octaveDots n d | n >= 0 = upperDots n `D.above` d
        octaveDots n d | otherwise = lowerDots (negate n) `D.below` d
        upperDots n =
            if n <= 0
                then D.Empty
                else (dot `D.above` D.VGap dotSpacing) `D.above` upperDots (n - 1)
        lowerDots n =
            if n <= 0
                then D.Empty
                else (dot `D.below` D.VGap dotSpacing) `D.below` lowerDots (n - 1)

-- * Beams

type ColumnWidth = D.UserUnit
-- | A 'NumBeam' describes the number of beams above a column.
type NumBeam = Int

-- * Rhythmical stuff (pretty much everything)

{- |
An inhabitant of @'Rhythmical' t@ is a @t@ that happens with a duration.
-}
data Rhythmical a
    = MkRhythmical Expansion a
    deriving (Show)

rhythmical :: NumBeat -> a -> Rhythmical a
rhythmical b x = MkRhythmical (expand b) x

-- * Lyrics

type Lyrics = [LyricsElem]

drawLyrics :: Lyrics -> [D.Drawing]
drawLyrics = map f
    where
        f (Syllable s) = D.string s
        f Space = D.HGap 16
        f Dash = D.char '-'
        f Underline = D.HLine 32

data LyricsElem
    = Syllable String
    -- | should only be used on a rest
    | Space
    -- | connects two adjacent syllables in a word
    | Dash
    -- | extends a vowel
    | Underline
    deriving (Show)

-- * User mental model

type Music = [VoiceElem]

musicBeaming :: Music -> [NumBeam]
musicBeaming = concatMap f
    where
        f (MNote _ d) = beaming (expand d)
        f (MRest d) = beaming (expand d)

beaming :: Expansion -> [NumBeam]
beaming One = [0]
beaming (Half a) = map (1 +) (beaming a)
beaming (Plus a b) = beaming a ++ beaming b

{- |
In order to render a 'Music', you need to get a @['BarElem']@ and a @['NumBeam']@ out of it.
-}
translate :: MusicParam -> Music -> [BarElem]
translate param = concatMap f
    where
        f (MNote p d) = replaceFirstBeat $ g $ expand nb
            where
                nb = d * numBeatPerDur
                replaceFirstBeat [] = []
                replaceFirstBeat (x : xs) = h x : xs
                h Beat = Note p
                h u = u
                g One = [Beat]
                g (Half b) = g b
                g (Plus a b) = g a ++ g b
        f (MRest d) = g $ expand nb
            where
                nb = d * numBeatPerDur
                g One = [Rest]
                g (Half b) = g b
                g (Plus a b) = g a ++ g b
        numBeatPerDur = mpNumBeatPerDur param

data VoiceElem
    = MNote Pitch NumBeat
    | MRest NumBeat
    deriving (Show)

type NumBeat = Rational

data MusicParam
    = MkMusicParam
    {
        mpNumBeatPerDur :: Rational
    }
    deriving (Show)

defMusicParam :: MusicParam
defMusicParam = MkMusicParam 1

-- * Beat expansion

{- |
An inhabitant of this type represents the expansion of a duration.

Numbered notation can only represent notes whose duration can be stated in the following /expanded/ form:

@
n0 \/ 2**0 + n1 \/ 2**1 + n2 \/ 2**2 + ...
@

where n0, n1, n2, and so on are nonnegative integers.

While users prefer writing (2 + 3\/4), it can only be notated as 1 + 1 + (1 + 1\/2)\/2.

The function 'expand' implements that expansion.
-}
data Expansion
    = One -- ^ the duration of one dot
    | Half Expansion -- ^ half the given amount
    | Plus Expansion Expansion -- ^ sum of the given amounts
    deriving (Show)

{- |
Expand a duration so that it corresponds to its notation.

The denominator of the input duration should be a power of 2;
otherwise the representation will be truncated.
-}
expand :: NumBeat -> Expansion
expand = f numHalfsAllowed
    where
        f _ 1 = One
        f d x | x > 1 = Plus One $ f d (x - 1)
        f d x | d > 0 = Half $ f (d - 1) (x * 2)
        f _ _ = One
        numHalfsAllowed = 8 :: Int

-- * Glyphs

dot :: D.Drawing
dot = D.char '\x2022' -- bullet

dotSpacing :: D.UserUnit
dotSpacing = 4

hspacing :: D.UserUnit
hspacing = 8

-- * Pitch

-- | Pitch class.
data Class
    = Do
    | Re
    | Mi
    | Fa
    | So
    | La
    | Ti
    deriving (Show, Read, Eq, Enum)

number :: Class -> Int
number = (1 +) . fromEnum

data Pitch
    = MkPitch Class OctaveNumber
    deriving (Show)
