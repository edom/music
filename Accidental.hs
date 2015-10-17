module Accidental
where

import qualified Glyph as Gl

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp deriving (Show, Read, Eq)

glyph :: Accidental -> Char
glyph x = case x of
    DoubleFlat -> Gl.doubleFlat
    Flat -> Gl.flat
    Natural -> Gl.natural
    Sharp -> Gl.sharp
    DoubleSharp -> Gl.doubleSharp

-- how do we describe this function?
offset :: Accidental -> Int
offset x = case x of
    DoubleFlat -> -2
    Flat -> -1
    Natural -> 0
    Sharp -> 1
    DoubleSharp -> 2
