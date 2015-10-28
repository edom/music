{- |
Twelve-tone equal temperament.

This module deprecates "PitchAbc" and "PitchDoremi".
-}
module Pitch
where

import qualified Control.Applicative as Ap
import qualified Control.Monad as M

import qualified Test.QuickCheck as Q

import qualified Accidental as A

import Prelude hiding (break)

-- * Pitch class types

data Abc = C | D | E | F | G | A | B deriving (Show, Read, Eq, Enum, Bounded)
data Doremi = Do | Re | Mi | Fa | So | La | Ti deriving (Show, Read, Eq, Enum, Bounded)

doremiNumber :: Doremi -> Int
doremiNumber = (1 +) . fromEnum

type What = (Int, Int, Int)

-- * Diatone number

class DiatoneNum a where
    diatoneNum :: a -> Int

instance DiatoneNum Abc where diatoneNum = fromEnum
instance DiatoneNum Doremi where diatoneNum = fromEnum
instance (DiatoneNum c) => DiatoneNum (Pitch c) where diatoneNum (MkPitch c _ o) = diatoneNum c + 7 * o

class DiatoneDiff a where
    diatoneDiff :: a -> a -> Int

instance DiatoneDiff Abc where diatoneDiff x y = diatoneNum y - diatoneNum x
instance DiatoneDiff Doremi where diatoneDiff x y = diatoneNum y - diatoneNum x

-- * Semitone number

class SemitoneNum a where
    {- |
Zero corresponds to C0 (1 Hz in C8=256 tuning).
    -}
    semitoneNum :: a -> Int

{- |
Pitch class C octave number 0 is C0 (1 Hz in C256 tuning).
Twelve-tone equal temperament.

The octave number of middle C is 8.
-}
type Octave = Int

{- |
[c] The pitch class type (usually 'Abc' or 'Doremi').

An inhabitant of this type corresponds with a frequency in a particular tuning system.

An inhabitant of this type corresponds with a MIDI note number.
-}
data Pitch c
    = MkPitch c A.Accidental Octave
    deriving (Show, Read, Eq)

-- * Semitone distance

class SemitoneDiff a where
    {- |
The distance from the first pitch to the second pitch, in number of semitones.

Positive distance means that the second pitch is higher.
    -}
    semitoneDiff :: a -> a -> Distance

-- | Number of semitones.
type Distance = Int

instance (SemitoneNum c) => SemitoneDiff (Pitch c) where
    semitoneDiff x y = semitoneNum y - semitoneNum x

instance SemitoneDiff Abc where semitoneDiff x y = semitoneNum y - semitoneNum x
instance SemitoneDiff Doremi where semitoneDiff x y = semitoneNum y - semitoneNum x

instance SemitoneNum Abc where
    semitoneNum x = case x of
        C -> 0
        D -> 2
        E -> 4
        F -> 5
        G -> 7
        A -> 9
        B -> 11

instance SemitoneNum Doremi where
    semitoneNum x = case x of
        Do -> 0
        Re -> 2
        Mi -> 4
        Fa -> 5
        So -> 7
        La -> 9
        Ti -> 11

instance (SemitoneNum c) => SemitoneNum (Pitch c) where
    semitoneNum (MkPitch cls aci oct) = semitoneNum cls + semitoneNum aci + 12 * oct

instance SemitoneNum A.Accidental where
    semitoneNum = A.offset

-- * Relativization

{- |
An inhabitant of this type states the correspondence of two pitches.
-}
data KeySignature
    = MkKeySignature (Pitch Doremi) (Pitch Abc)
    deriving (Show, Read)

instance Q.Arbitrary Abc where arbitrary = Q.arbitraryBoundedEnum
instance Q.Arbitrary Doremi where arbitrary = Q.arbitraryBoundedEnum
instance (Q.Arbitrary c) => Q.Arbitrary (Pitch c) where arbitrary = M.liftM3 MkPitch Q.arbitrary Q.arbitrary Q.arbitrary
instance Q.Arbitrary KeySignature where arbitrary = Ap.liftA2 MkKeySignature Q.arbitrary Q.arbitrary

{- |
Translate a pitch from ABC pitch space to doremi pitch space.
-}
relativize :: KeySignature -> Pitch Abc -> Pitch Doremi
relativize (MkKeySignature drmBase abcBase) abc = drm
    where
        (drmBaseDia, _, drmBaseOct) = break drmBase
        (abcBaseDia, _, abcBaseOct) = break abcBase
        (abcDia, _, abcOct) = break abc
        (drmDia, drmOct) = normalize
            (
                drmBaseDia + abcDia - abcBaseDia
                , drmBaseOct + abcOct - abcBaseOct
            )
        drmCls = toEnum drmDia
        drmAco = semitoneDiff abcBase abc - semitoneDiff drmBase (MkPitch drmCls A.none drmOct)
        drmAci = A.prettyFromOffset drmAco
        drm = MkPitch drmCls drmAci drmOct
        normalize (dia, oct)
            | dia >= 7 = normalize (dia - 7, oct + 1)
            | dia < 0 = normalize (dia + 7, oct - 1)
            | otherwise = (dia, oct)

{- |
Break a pitch into its diatone number, its accidental offset, and its octave number.
-}
break :: (DiatoneNum c) => Pitch c -> (Int, Int, Int)
break (MkPitch cls aci oct) = (diatoneNum cls, semitoneNum aci, oct)

-- * QuickCheck properties

break_preserves_semitone_number :: Q.Property
break_preserves_semitone_number = Q.property prop
    where
        prop :: Pitch Abc -> Bool
        prop p = semitoneNum p == semitoneNum cls + aco + 12 * oct
            where
                (dia, aco, oct) = break p
                cls = toEnum dia :: Abc

pitch_class_space_is_a_subspace_of_pitch_space :: Q.Property
pitch_class_space_is_a_subspace_of_pitch_space = Q.property prop
    where
        prop :: Abc -> Bool
        prop c = semitoneNum c == semitoneNum (MkPitch c A.None 0)

relativization_preserves_distance :: Q.Property
relativization_preserves_distance = Q.property prop
    where
        prop :: KeySignature -> Pitch Abc -> Pitch Abc -> Q.Property
        prop k p q =
            Q.counterexample ("failing: " ++ show rp ++ "; " ++ show rq)
            $ semitoneDiff p q Q.=== semitoneDiff rp rq
            where
                rp = relativize k p
                rq = relativize k q

relativization_preserves_diatone :: Q.Property
relativization_preserves_diatone = Q.property prop
    where
        prop :: KeySignature -> Pitch Abc -> Pitch Abc -> Q.Property
        prop k p q =
            mod (diatoneDiff (cls p) (cls q)) 7
            Q.===
            mod (diatoneDiff (cls $ relativize k p) (cls $ relativize k q)) 7
        cls (MkPitch c _ _) = c
