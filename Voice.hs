module Voice
where

import qualified Accidental as A
import qualified BeatExpansion as B
import qualified Pitch as P

{- |
A voice is a sequence of duration-pitch pairs.

[@p@] pitch class type.
Usually 'P.Abc' or 'P.Doremi'.
-}
type Voice p = [Event p]

data Event p
    = MkEvent B.Expansion (Type p)
    deriving (Read, Show)

note :: Rational -> p -> A.Accidental -> Int -> Event p
note rhy cls aci oct = MkEvent (B.expand rhy) $ Note (P.MkPitch cls aci oct)

rhythmOf :: Event p -> B.Expansion
rhythmOf (MkEvent x _) = x

{- |
Note or rest.
-}
data Type p
    = Rest
    | Note (P.Pitch p)
    deriving (Read, Show)
