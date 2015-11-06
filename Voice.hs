{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Voice
where

import qualified Accidental as A
import qualified BeatExpansion as B
import qualified Pitch as P

{- |
A voice is a sequence of duration-pitch pairs.

[@d@] duration type

[@p@] payload type
-}
type Voice d p = [Event d p]

data Event d p
    = MkEvent d p
    deriving (Read, Show)

note :: BeatCount -> p -> A.Accidental -> Int -> Event BeatCount (Type p)
note rhy cls aci oct = MkEvent rhy $ Note (P.MkPitch cls aci oct)

-- | Number of beats.
type BeatCount = Rational

expand :: Event BeatCount p -> Event B.Expansion p
expand (MkEvent d x) = MkEvent (B.expand d) x

durationOf :: Event d p -> d
durationOf (MkEvent x _) = x

{- |
Note or rest.

[@p@] pitch class type.
Usually 'P.Abc' or 'P.Doremi'.
-}
data Type c
    = Rest
    | Note (P.Pitch c)
    deriving (Read, Show)
