{-# LANGUAGE DeriveFunctor #-}

{- |

Indonesian numbered musical notation.

Mostly similar to
<https://en.wikipedia.org/wiki/Numbered_musical_notation numbered musical notation>.

-}
module Numbered
where

import qualified BeatExpansion as B
import qualified NumberedDraw as Nd
import qualified Pitch as Pd

-- * User model

{- |
A voice is a sequence of pitch-duration pairs.

In order to render a 'Voice',
you need to get a @['Nd.BarElem']@ (using 'translate')
and a @['Nd.NumBeam']@ (using 'beaming') out of it.

Then you use "NumberedDraw" to render both of them as two separate rows.
-}
type Voice = [VoiceElem]

note :: NumBeat -> Pd.Doremi -> Pd.Octave -> VoiceElem
note numbeat cls oct = rhythmical numbeat $ Note cls oct

rest :: NumBeat -> VoiceElem
rest numbeat = rhythmical numbeat Rest

-- * Mapping user model to machine model

{- |
Compute the beams of the music.
-}
beaming :: Voice -> [Nd.NumBeam]
beaming = concatMap f
    where
        f (MkRhythmical d _) = B.beaming d

{- |
Translate the music into a row of bar elements (notes, dots, and rests).

This maps the user-friendly representation to the machine-friendly representation.
-}
translate :: Voice -> [Nd.BarElem]
translate = concatMap f
    where
        f (MkRhythmical d x) = g x d
        g (Note c o) = replaceFirstBeat . B.fold [Nd.Beat] id (++)
            where
                replaceFirstBeat [] = []
                replaceFirstBeat (x : xs) = h x : xs
                h Nd.Beat = Nd.Note c o
                h u = u
        g Rest = B.fold [Nd.Rest] id (++)

-- * Internals of the user model

type VoiceElem = Rhythmical Pitch

{- |
An inhabitant of @'Rhythmical' t@ is a @t@ that happens with a duration.
-}
data Rhythmical a
    = MkRhythmical B.Expansion a
    deriving (Read, Show)

rhythmical :: NumBeat -> a -> Rhythmical a
rhythmical b x = MkRhythmical (B.expand b) x

data Pitch
    = Note Pd.Doremi Pd.Octave
    | Rest
    deriving (Read, Show)

-- * Splitting a voice into bars

data Bar a
    = MkBar
    {
        _content :: [(Duration, a)] -- ^ the content
        , _slur :: Bool -- ^ whether the bar ends with a slur
    }
    deriving (Read, Show, Functor)

splitFirstBar
    :: NumBeatPerBar -- ^ time signature
    -> [(Duration, a)] -- ^ the input voice
    -> (Bar a, [(Duration, a)]) -- ^ (the bar, the rest of the voice that doesn't make it into the bar)
splitFirstBar numBeatPerBar voice = f [] numBeatPerBar voice
    where
        f bar _ [] = (MkBar (reverse bar) False, [])
        f bar n vs | n <= 0 = (MkBar (reverse bar) False, vs)
        f bar n (v@(dur,_) : vs) | dur <= n = f (v : bar) (n - dur) vs
        f bar n ((dur,a) : vs) = (MkBar (reverse ((n,a) : bar)) True, ((dur - n, a) : vs))

{-
data BarElem pitch
    = Note Pitch Duration
    | Rest Duration
    | Dynamics String
-}

splitIntoBars :: NumBeatPerBar -> [(Duration, a)] -> [Bar a]
splitIntoBars _ [] = []
splitIntoBars n v = firstBar : splitIntoBars n theRest
    where
        (firstBar, theRest) = splitFirstBar n v

type NumBeatPerBar = Rational
type NumBeat = Rational
type Duration = NumBeat
