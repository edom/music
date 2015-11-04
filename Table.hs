{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Table
where

import qualified Data.List as L
import qualified Test.QuickCheck as Q

{- |
An inhabitant of this type represents a table
(something like a two-dimensional array of cells).

The internal representation of the table is a list of rows.
Each row is a list of cells.

Each row can have a different number of cells.

[@m@] the internal representation of the table ('RowMajor' or 'ColMajor').
-}
newtype Table m a
    = MkTable { _unTable :: [[a]] }
    deriving (Read, Show, Eq)

instance Functor (Table m) where fmap f = MkTable . fmap (fmap f) . _unTable
instance (Q.Arbitrary a) => Q.Arbitrary (Table m a) where arbitrary = fmap MkTable Q.arbitrary

-- * Table size

{- |
The major of a table is the direction for which access is fast.
-}
majorCountOf :: Table m a -> Int
majorCountOf = length . _unTable

{- |
The minor of a table is the direction orthogonal to the major.
-}
minorCountOf :: Table m a -> Int
minorCountOf = maximum . (0 :) . map length . _unTable

sizeOf :: (Major m) => Table m a -> (Int, Int)
sizeOf t = (rowCountOf t, colCountOf t)

-- * Table access

{- |
Accessing the rows of a 'RowMajor' table is fast.

Accessing the columns of a 'ColMajor' table is fast.
-}
class Major m where
    -- | Fast for 'RowMajor' tables.
    fromRowList :: [[a]] -> Table m a
    -- | Fast for 'ColMajor' tables.
    fromColList :: [[a]] -> Table m a
    -- | Fast for 'RowMajor' tables.
    rowsOf :: Table m a -> [[a]]
    -- | Fast for 'ColMajor' tables.
    colsOf :: Table m a -> [[a]]
    -- | Fast for 'RowMajor' tables.
    rowCountOf :: Table m a -> Int
    -- | Fast for 'ColMajor' tables.
    colCountOf :: Table m a -> Int
    -- | Fast for 'RowMajor' tables.
    reverseRows :: Table m a -> Table m a

instance Major RowMajor where
    fromRowList = MkTable
    fromColList = MkTable . L.transpose
    rowsOf = _unTable
    colsOf = L.transpose . _unTable
    rowCountOf = majorCountOf
    colCountOf = minorCountOf
    reverseRows = MkTable . reverse . _unTable

instance Major ColMajor where
    fromColList = MkTable
    fromRowList = MkTable . L.transpose
    rowsOf = L.transpose . _unTable
    colsOf = _unTable
    rowCountOf = minorCountOf
    colCountOf = majorCountOf
    reverseRows = MkTable . L.transpose . reverse . L.transpose . _unTable

-- * Adding rows and columns

-- | Add a row before the top of the table.
prependRow :: [a] -> Table RowMajor a -> Table RowMajor a
prependRow row (MkTable tab) = MkTable $ row : tab

-- | Add a column before the left border of the table.
prependColumn :: [a] -> Table RowMajor a -> Table RowMajor a
prependColumn col (MkTable tab) = MkTable $ zipWith (:) col tab

-- * Joining tables

-- | Join horizontally.
juxtapose :: [Table ColMajor a] -> Table ColMajor a
juxtapose = MkTable . concatMap _unTable

-- | Join vertically.
stack :: [Table RowMajor a] -> Table RowMajor a
stack = MkTable . concatMap _unTable

-- * Transform and transpose

class Transform m n | m -> n where
    {- |
The same table but with different internal representation.

Note that this only works correctly iff the input is rectangular
because 'L.transpose' from "Data.List" assumes so.
    -}
    transform :: Table m a -> Table n a
    transform = MkTable . L.transpose . _unTable
    -- | Columns become rows. Rows become columns.
    transpose :: Table m a -> Table n a
    transpose = MkTable . _unTable

instance Transform RowMajor ColMajor
instance Transform ColMajor RowMajor

-- * Normalize

{- |
Make all rows have the same length by padding the end of short rows with empties.
-}
normalize :: a -> Table m a -> Table m a
normalize empty tab =
    MkTable . map (\ major -> major ++ replicate (minorCount - length major) empty) $ _unTable tab
    where
        minorCount = minorCountOf tab

-- isNormalized :: Table m a -> Bool
-- isNormalized table =

{- |
Append empties to short tables.
-}
equalizeMajor :: a -> [Table m a] -> [Table m a]
equalizeMajor empty tables = map (majorPad empty wantedMajorCount) tables
    where
        wantedMajorCount = maximum $ 0 : map majorCountOf tables

majorPad :: a -> Int -> Table m a -> Table m a
majorPad empty wantedMajorCount table | wantedMajorCount > thisMajorCount
    = MkTable $ _unTable table ++ padding
    where
        thisMajorCount = majorCountOf table
        thisMinorCount = minorCountOf table
        padding = replicate (wantedMajorCount - thisMajorCount) $ replicate thisMinorCount empty
majorPad _ _ table = table

-- * QuickCheck properties

-- | Run all tests in this module.
test :: IO ()
test = mapM_ Q.quickCheck
    [
        transpose_exchanges_size
        , normalized_double_transpose
        , normalized_double_transform
        , normalization_preserves_size
    ]

{- |
Transposing a non-empty table should preserve (exchange) its size.
-}
transpose_exchanges_size :: Q.Property
transpose_exchanges_size =
    Q.label "transpose_exchanges_size" prop
    where
        prop tab =
            rowCountOf tab == colCountOf tab'
            && colCountOf tab == rowCountOf tab'
            where
                tab' = transpose (tab :: Table RowMajor ())

{- |
If a table is non-empty and is already 'normalize'd,
then 'transpose'ing it twice should result in the same table.
-}
normalized_double_transpose :: Q.Property
normalized_double_transpose =
    Q.label "normalized_double_transpose" prop
    where
        prop :: Table RowMajor () -> Q.Property
        prop tab =
            -- rowCountOf tab > 0
            -- && colCountOf tab > 0
            True
            Q.==>
            nor == transpose (transpose nor)
            where
                nor = normalize () tab

{- |
If a table is non-empty and is already 'normalize'd,
then 'transform'ing it twice should result in the same table.
-}
normalized_double_transform :: Q.Property
normalized_double_transform =
    Q.label "normalized_double_transform" prop
    where
        prop tab =
            rowCountOf tab > 0
            && colCountOf tab > 0
            Q.==>
            nor == transform (transform nor)
            where
                nor :: Table RowMajor ()
                nor = normalize () tab

normalization_preserves_size :: Q.Property
normalization_preserves_size =
    Q.label "normalization_preserves_size" prop
    where
        prop :: Table RowMajor () -> Bool
        prop tab = sameSize tab $ normalize () tab

sameSize :: Table m a -> Table m b -> Bool
sameSize a b = majorCountOf a == majorCountOf b && minorCountOf a == minorCountOf b

-- * Majors

data RowMajor
data ColMajor
