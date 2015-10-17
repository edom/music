module BeatExpansion
where

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
    deriving (Read, Show)

fold
    :: a -- ^ this replaces a 'One'
    -> (a -> a) -- ^ this folds a 'Half'
    -> (a -> a -> a) -- ^ this folds a 'Plus'
    -> Expansion
    -> a
fold one half plus = g
    where
        g One = one
        g (Half x) = half (g x)
        g (Plus x y) = plus (g x) (g y)

{- |
Expand a duration so that it corresponds to its notation.

The denominator of the input duration should be a small power of 2;
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

{- |
This is the inverse of 'expand' (if the expansion is not truncated).
-}
collapse :: Expansion -> NumBeat
collapse = fold 1 (/ 2) (+)

type NumBeat = Rational

-- * Beaming

beaming :: Expansion -> [NumBeam]
beaming One = [0]
beaming (Half a) = map (1 +) (beaming a)
beaming (Plus a b) = beaming a ++ beaming b

type NumBeam = Int
