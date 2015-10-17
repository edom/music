{- |

Indonesian numbered musical notation.

Mostly similar to
<https://en.wikipedia.org/wiki/Numbered_musical_notation numbered musical notation>.

-}
module Numbered
where

import qualified BeatExpansion as B
import qualified NumberedDraw as Nd
import qualified PitchDoremi as Pd

-- * User model

{- |
A voice is a sequence of pitch-duration pairs.

In order to render a 'Voice',
you need to get a @['Nd.BarElem']@ (using 'translate')
and a @['Nd.NumBeam']@ (using 'beaming') out of it.

Then you use "NumberedDraw" to render both of them as two separate rows.
-}
type Voice = [VoiceElem]

note :: NumBeat -> Pd.Class -> Pd.Octave -> VoiceElem
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
    = Note Pd.Class Pd.Octave
    | Rest
    deriving (Read, Show)

type NumBeat = Rational
