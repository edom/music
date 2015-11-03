module Lyrics
where

import qualified Draw as D

{- |
Prepare the lyrics into a row of 'D.Drawing's.

The row expects to be incorporated into a table.
-}
row :: Lyrics -> [D.Drawing]
row = map f
    where
        f (Syllable s) = D.string s
        f Space = D.HGap 16
        f Dash = D.char '-'
        f Underline = D.line 32 0

type Lyrics = [Element]

data Element
    = Syllable String
    -- | should only be used on a rest
    | Space
    -- | connects two adjacent syllables in a word
    | Dash
    -- | extends the ending of a word
    | Underline
    deriving (Read, Show)
