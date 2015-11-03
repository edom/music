module TableLayout
where

import qualified Data.List as L
import qualified Data.Monoid as M

import qualified Rect as R
import qualified Table as T

-- * Placing things into a table

{- |
Compute the offsets needed to lay out the drawings into a table.

Each row can have a different number of cells.

Each offset moves the origin to the top-left corner of the corresponding cell.

The difference between a table and a grid:

A grid is this constraint:

* every cell has the same size

A table is this set of constraints:

* all cells in a row have the same height

* all cells in a column have the same width

-}
table :: (Num a, Ord a) => T.Table (Size a) -> T.Table (R.Rect a)
table inputSizes = T.fromRowList $ zipWith row rowHeights $ L.scanl (+) 0 rowHeights
    where
        row h_ y_ = tail $ L.scanl (\ brPrevRow w -> R.xywh (R.x1 brPrevRow) y_ w h_) (R.xywh 0 y_ 0 h_) columnWidths
        rowHeights = L.map rowHeight $ T.rowsOf inputSizes
        columnWidths = L.map columnWidth $ T.columnsOf inputSizes

-- * Computing table size

{- |
The height of a row is the height of its tallest column.

All columns in a row have the same height.
-}
rowHeight :: (Num a, Ord a) => [Size a] -> a
rowHeight = L.foldl' max 0 . L.map height

{- |
The width of a column is the width of its widest row.

All rows in a column have the same width.
-}
columnWidth :: (Num a, Ord a) => [Size a] -> a
columnWidth = L.foldl' max 0 . L.map width

-- * Size

{- |
An inhabitant of this type represents the size of the bounding rectangle of a drawing.
-}
data Size a
    = MkSize
    {
        width :: a
        , height :: a
    }
    deriving (Show)

instance (Num a, Ord a) => M.Monoid (Size a) where
    mempty = MkSize 0 0
    mappend (MkSize w0 h0) (MkSize w1 h1) = MkSize (max w0 w1) (max h0 h1)

-- * Rectangle

rectSize :: (Num a) => R.Rect a -> Size a
rectSize r = MkSize (R.width r) (R.height r)

-- * Example

example :: T.Table (R.Rect Double)
example =
    table $ fmap rectSize $ T.fromRowList [
        [R.xywh 0 0 1 1, R.xywh 0 0 2 4]
        , [R.xywh 0 0 2 3, R.xywh 0 0 1 2]
        , [R.xywh 0 0 1 1, R.xywh 0 0 1 1]
    ]
