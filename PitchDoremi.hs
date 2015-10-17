module PitchDoremi
where

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
    = MkPitch Class Octave
    deriving (Read, Show)

type Octave = Int
