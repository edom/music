module Table
where

import qualified Data.Monoid as M
import qualified Data.List as L

import qualified Rect as R

-- * Placing things into a table

{- |
An inhabitant of this type represents a table
(something like a two-dimensional array of cells).

The table is in a row-major order.
The table is represented as a list of rows.
Each row is a list of cells.

Each row can have a different number of cells.
-}
type Table a = [[a]]

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
table :: (Num a, Ord a) => Table (Size a) -> Table (R.Rect a)
table inputSizes = zipWith row rowHeights $ L.scanl (+) 0 rowHeights
    where
        row h_ y_ = tail $ L.scanl (\ brPrevRow w -> R.xywh (R.x1 brPrevRow) y_ w h_) (R.xywh 0 y_ 0 h_) columnWidths
        rowHeights = L.map rowHeight inputSizes
        columnWidths = L.map columnWidth $ L.transpose inputSizes

-- | Add a row before the top of the table.
prependRow :: [a] -> Table a -> Table a
prependRow = (:)

-- | Add a column before the left border of the table.
prependColumn :: [a] -> Table a -> Table a
prependColumn = zipWith (:)

map :: (a -> b) -> Table a -> Table b
map = L.map . L.map

-- * Example

example :: Table (R.Rect Double)
example =
    table $ Table.map rectSize [
        [R.xywh 0 0 1 1, R.xywh 0 0 2 4]
        , [R.xywh 0 0 2 3, R.xywh 0 0 1 2]
        , [R.xywh 0 0 1 1, R.xywh 0 0 1 1]
    ]

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

{- |
You can access the element at row r column c like this:

@
tab \`at\` r \`at\` c
@
-}
at :: (M.Monoid a) => [a] -> Int -> a
at (x : _) 0 = x
at (_ : y) n | n > 0 = at y (n - 1)
at _ _ = M.mempty
infixl 1 `at`

-- * Rectangle

rectSize :: (Num a) => R.Rect a -> Size a
rectSize r = MkSize (R.width r) (R.height r)

-- * Offset

data Offset a
    = MkOffset
    {
        dx :: a
        , dy :: a
    }
    deriving (Show)
