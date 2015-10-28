module Accidental
where

import qualified Control.Applicative as Ap

import qualified Test.QuickCheck as Q

import qualified Glyph as Gl

sharp :: Accidental
sharp = Many 1 Sharp

flat :: Accidental
flat = Many 1 Flat

natural :: Accidental
natural = Natural

none :: Accidental
none = None

-- | True iff the accidental is none, natural, single sharp, or single flat.
isSimple :: Accidental -> Bool
isSimple None = True
isSimple Natural = True
isSimple (Many x _) | abs x == 1 = True
isSimple _ = False

data Accidental
    = None
    | Natural
    | Many Int BasicAccidental -- ^ microtones are not yet supported
    deriving (Show, Read, Eq)

data BasicAccidental
    = Flat
    | Sharp
    deriving (Show, Read, Eq, Enum, Bounded)

instance Q.Arbitrary BasicAccidental where arbitrary = Q.arbitraryBoundedEnum

instance Q.Arbitrary Accidental where
    arbitrary = Q.oneof [return None, return Natural, Ap.liftA2 Many Q.arbitrary Q.arbitrary]

glyph :: Accidental -> Char
glyph x = case x of
    None -> ' '
    Natural -> Gl.natural
    Many 2 Flat -> Gl.doubleFlat
    Many 1 Flat -> Gl.flat
    Many 1 Sharp -> Gl.sharp
    Many 2 Sharp -> Gl.doubleSharp
    _ -> '?'

-- | Semitone offset.
offset :: Accidental -> NumSemitone
offset None = 0
offset Natural = 0
offset (Many n b) = n * ofst b
    where
        ofst Flat = -1
        ofst Sharp = 1

type NumSemitone = Int

diff :: Accidental -> Accidental -> NumSemitone
diff x y = offset x - offset y

-- * Internals

prettyFromOffset :: Int -> Accidental
prettyFromOffset n | n < 0 = Many (negate n) Flat
prettyFromOffset n | n > 0 = Many n Sharp
prettyFromOffset _ = None
