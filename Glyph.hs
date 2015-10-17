{- |
Aliases for some characters.
-}
module Glyph
where

clefF :: Char
clefF = '\x1d122'

clefG :: Char
clefG = '\x1d11e'

head1 :: Char
head1 = '\xe0a2'

head_2 :: Char
head_2 = '\x1d157'

head_4 :: Char
head_4 = '\x1d158'

doubleFlat :: Char
doubleFlat = '\x1d12b'

flat :: Char
flat = '\x266d'

natural :: Char
natural = '\x266e'

sharp :: Char
sharp = '\x266f'

doubleSharp :: Char
doubleSharp = '\x1d12a'

rest1 :: Char
rest1 = '\x1d13b'

-- | rest_2 looks identical to rest1. They also have the same baseline.
rest_2 :: Char
rest_2 = '\x1d13c'

rest_4 :: Char
rest_4 = '\x1d13d'

rest_8 :: Char
rest_8 = '\x1d13e'

rest_16 :: Char
rest_16 = '\x1d13f'

rest_32 :: Char
rest_32 = '\x1d140'

rest_64 :: Char
rest_64 = '\x1d141'

timeSig0 :: Char
timeSig0 = '\xe080'

timeSig1 :: Char
timeSig1 = '\xe081'

timeSig2 :: Char
timeSig2 = '\xe082'

timeSig3 :: Char
timeSig3 = '\xe083'

timeSig4 :: Char
timeSig4 = '\xe084'

timeSig5 :: Char
timeSig5 = '\xe085'

timeSig6 :: Char
timeSig6 = '\xe086'

timeSig7 :: Char
timeSig7 = '\xe087'

timeSig8 :: Char
timeSig8 = '\xe088'

timeSig9 :: Char
timeSig9 = '\xe089'

timeSigGlyphs :: Int -> [Char]
timeSigGlyphs x
    | x < 0 = []
    | otherwise = case x of
    0 -> [timeSig0]
    1 -> [timeSig1]
    2 -> [timeSig2]
    3 -> [timeSig3]
    4 -> [timeSig4]
    5 -> [timeSig5]
    6 -> [timeSig6]
    7 -> [timeSig7]
    8 -> [timeSig8]
    9 -> [timeSig9]
    _ ->
        let
            (q, r) = divMod x 10
        in
            timeSigGlyphs q ++ timeSigGlyphs r

bullet :: Char
bullet = '\x2022'
