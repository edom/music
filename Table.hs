module Table
where

import qualified Data.Monoid as M
import qualified Test.QuickCheck as Q

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
    deriving (Read, Show, Eq)

instance Functor Table where fmap f = MkTable . fmap (fmap f) . _unTable
instance (Q.Arbitrary a) => Q.Arbitrary (Table a) where arbitrary = fmap MkTable Q.arbitrary

-- * Conversion from and to lists

fromRowList :: [[a]] -> Table a
fromRowList = MkTable

rowsOf :: Table a -> [[a]]
rowsOf = _unTable

columnsOf :: (M.Monoid a) => Table a -> [[a]]
columnsOf = transposeList . _unTable

numRows :: Table a -> Int
numRows = length . rowsOf

numColumns :: Table a -> Int
numColumns = maximum . (0 :) . map length . rowsOf

-- * Adding rows and columns

-- | Add a row before the top of the table.
prependRow :: [a] -> Table a -> Table a
prependRow row (MkTable tab) = MkTable $ row : tab

-- | Add a column before the left border of the table.
prependColumn :: [a] -> Table a -> Table a
prependColumn col (MkTable tab) = MkTable $ zipWith (:) col tab

-- * Transpose

-- | Columns become rows. Rows become columns.
transpose :: (M.Monoid a) => Table a -> Table a
transpose = MkTable . transposeList . _unTable

transposeList :: (M.Monoid a) => [[a]] -> [[a]]
transposeList = f
    where
        f rows | all null rows = []
        f rows = map safeHead rows : f (map safeTail rows)
        safeHead [] = M.mempty
        safeHead (x : _) = x
        safeTail [] = []
        safeTail (_ : x) = x

-- * Normalize

{- |
Make all rows have the same length by padding the end of short rows with 'M.mempty's.
-}
normalize :: (M.Monoid a) => Table a -> Table a
normalize tab =
    fromRowList $ map (\ r -> r ++ replicate (n - length r) M.mempty) $ rowsOf tab
    where
        n = numColumns tab

-- * QuickCheck properties

-- | Run all tests in this module.
test :: IO ()
test = mapM_ Q.quickCheck
    [
        nonempty_double_transpose_preserves_size
        , normalized_double_transpose
        , normalization_preserves_size
    ]

{- |
Transposing a non-empty table twice should preserve its size.

The transpose from "Data.List" does not satisfy this property.
-}
nonempty_double_transpose_preserves_size :: Q.Property
nonempty_double_transpose_preserves_size =
    Q.label "nonempty_double_transpose_preserves_size" prop
    where
        prop :: Table () -> Q.Property
        prop tab =
            numRows tab > 0
            && numColumns tab > 0
            Q.==>
            sameSize tab tab'
            where
                tab' = transpose $ transpose tab

{- |
If a table is already 'normalize'd,
then transposing it twice should result in the same table.
-}
normalized_double_transpose :: Q.Property
normalized_double_transpose =
    Q.label "normalized_double_transpose" prop
    where
        prop :: Table () -> Q.Property
        prop tab =
            numRows tab > 0
            && numColumns tab > 0
            Q.==>
            nor == transpose (transpose nor)
            where
                nor = normalize tab

normalization_preserves_size :: Q.Property
normalization_preserves_size =
    Q.label "normalization_preserves_size" prop
    where
        prop :: Table () -> Bool
        prop tab = sameSize tab (normalize tab)

sameSize :: Table a -> Table b -> Bool
sameSize a b = numRows a == numRows b && numColumns a == numColumns b
