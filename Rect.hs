-- | Bounding rectangle.

module Rect
where

import qualified Data.Monoid as Mo

-- * The Rect a type

data Rect a
    = MkRect
    {
        x0 :: a
        , y0 :: a
        , x1 :: a
        , y1 :: a
    }
    deriving (Show, Read)

-- * Making rectangles

empty :: (Num a) => Rect a
empty = MkRect 0 0 0 0

{- |
Specify a bounding rectangle by its top left point (x and y), its width, and its height.
-}
xywh :: (Num a) => a -> a -> a -> a -> Rect a
xywh x y w h = MkRect x y (x + w) (y + h)

-- * Specifying rectangles with respect to other rectangles

{- |
@r \`after\` s@ moves r so that its left side intersects the right side of s.
-}
after :: (Num a) => Rect a -> Rect a -> Rect a
after toBeMoved willBeAtLeft =
    translate t 0 toBeMoved
    where
        t = x1 willBeAtLeft - x0 toBeMoved

-- * Size

width :: (Num a) => Rect a -> a
width (MkRect x0_ _ x1_ _) = x1_ - x0_

height :: (Num a) => Rect a -> a
height (MkRect _ y0_ _ y1_) = y1_ - y0_

-- * Moving

translate :: (Num a) => a -> a -> Rect a -> Rect a
translate dx dy (MkRect x0_ y0_ x1_ y1_) = MkRect (x0_ + dx) (y0_ + dy) (x1_ + dx) (y1_ + dy)

instance (Num a, Ord a) => Mo.Monoid (Rect a) where
    mempty = empty
    mappend (MkRect a0 b0 a1 b1) (MkRect x0_ y0_ x1_ y1_) =
        MkRect (min a0 x0_) (min b0 y0_) (max a1 x1_) (max b1 y1_)
