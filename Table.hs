module Table
where

import qualified Data.List as L

{- |
An inhabitant of this type represents a table
(something like a two-dimensional array of cells).

The table is in a row-major order.
The table is represented as a list of rows.
Each row is a list of cells.

Each row can have a different number of cells.
-}
newtype Table a
    = MkTable { _unTable :: [[a]] }
    deriving (Read, Show)

instance Functor Table where
    fmap f = MkTable . fmap (fmap f) . _unTable

-- * Conversion from and to lists

fromRowList :: [[a]] -> Table a
fromRowList = MkTable

rowsOf :: Table a -> [[a]]
rowsOf = _unTable

columnsOf :: Table a -> [[a]]
columnsOf = L.transpose . _unTable

-- * Adding rows and columns

-- | Add a row before the top of the table.
prependRow :: [a] -> Table a -> Table a
prependRow row (MkTable tab) = MkTable $ row : tab

-- | Add a column before the left border of the table.
prependColumn :: [a] -> Table a -> Table a
prependColumn col (MkTable tab) = MkTable $ zipWith (:) col tab

-- * Transpose

-- | Columns become rows. Rows become columns.
transpose :: Table a -> Table a
transpose = MkTable . L.transpose . _unTable
