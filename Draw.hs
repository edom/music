{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Draw
where

import qualified Data.Monoid as Mo

import qualified Rect as R
import qualified Table as T

-- * Description

-- | A textual drawing that consists of one character.
char :: Char -> Drawing
char x = Textual [x]

-- | A textual drawing.
string :: String -> Drawing
string = Textual

staff :: Drawing
staff = genStaff 5
    where
        genStaff :: Int -> Drawing
        genStaff n | n <= 0 = Mo.mempty
        genStaff n = HLine 1000 Mo.<> spaceDown 1 (genStaff $ n - 1)

-- * Composition

-- | A drawing that consists of all the drawings; they will be drawn using the same origin.
overlay :: [Drawing] -> Drawing
overlay [] = Empty
overlay (x : xs) = Overlay x (overlay xs)

-- | Packed horizontal layout.
juxtapose :: [Drawing] -> Drawing
juxtapose = foldr HSeq Empty
{-
-- revert back to this if slow? but meaning is different
juxtapose = loop 0
    where
        loop _ [] = Empty
        loop hpos (x : xs) =
            GetBounds x $ \ boundRect ->
                Overlay
                    (Translate (hpos - R.x0 boundRect) 0 x)
                    (loop (hpos + R.width boundRect) xs)
-}

-- | This is 'juxtapose' for two elements.
andThen :: Drawing -> Drawing -> Drawing
andThen = HSeq

{- |
The drawing in which the first drawing is right above the second drawing.

The origin of the result is the origin of the second drawing.
The first drawing is moved up.

The second drawing should be bigger than the first drawing.
-}
above :: Drawing -> MainDrawing -> Drawing
above u v =
    GetBounds u $ \ a ->
    GetBounds v $ \ b ->
    Translate 0 (R.y0 b - R.y1 a) u `Overlay` v

below :: Drawing -> MainDrawing -> Drawing
below u v =
    GetBounds u $ \ a ->
    GetBounds v $ \ b ->
    Translate 0 (R.y1 b - R.y0 a) u `Overlay` v

{-
In 'above', we want the bottom of the translated bounds_a to meet the top of bounds_b.

a.y1 + t = b.y0

In 'below', we want the top of the translated bounds_a to meet the bottom of bounds_b.

a.y0 + t = b.y1

-}

type MainDrawing = Drawing

spaceDown :: NumSpace -> Drawing -> Drawing
spaceDown n d = GetFontSize $ \ f ->
    let
        space = f / 4
    in
        Translate 0 (MkUserUnit n * space) d

spaceRight :: NumSpace -> Drawing -> Drawing
spaceRight n d = GetFontSize $ \ f ->
    let
        space = f / 4
    in
        Translate (MkUserUnit n * space) 0 d

-- * Drawing

data Drawing
    -- | an empty drawing
    = Empty
    -- | a text
    | Textual String
    -- | a horizontal line with the given width; the line extends to the right
    | HLine UserUnit
    | VLine UserUnit
    -- | an empty drawing with the given minimum width; 'HFit' changes this gap
    | HGap UserUnit
    | Gap UserUnit UserUnit
    -- | a drawing that is not drawn, but whose size is computed
    | Hidden Drawing
    -- | a juxtaposition of the given drawings;
    -- this changes 'HGap's in order to make the total width be the given width
    | HFit UserUnit [Drawing]
    -- | a drawing consisting of both drawings using the same origin
    | Overlay Drawing Drawing
    -- | @'HSeq' a b@ is a and translated b such that the right side of the bounding rectangle of a
    -- meets the left side of the bounding rectangle of b
    | HSeq MainDrawing Drawing
    -- | @'VSeq' a b@ is a and translated b such that the bottom side of the bounding rectangle of a
    -- meets the top side of the bounding rectangle of b
    | VSeq MainDrawing Drawing
    -- | table in row-major order; if a row has too few columns, empty columns will be added to the right
    | Table (T.Table Drawing)
    -- | the given drawing, but moved with respect to the parent drawing;
    -- the arguments are rightward and downward translation amount, respectively;
    -- negative argument means reverse direction
    | Translate UserUnit UserUnit Drawing
    -- | a drawing that depends on the font size used to draw it
    | GetFontSize (UserUnit -> Drawing)
    -- | a drawing that depends on the bounding rectangle of the first drawing; the first drawing is not drawn
    | GetBounds Drawing (R.Rect UserUnit -> Drawing)
    | GetCellBounds (R.Rect UserUnit -> Drawing)
    -- | an empty drawing that prints something to the standard output when rendered
    | DebugPutStr String

-- Boilerplate. Nothing interesting here. Move on.
instance Show Drawing where
    show x = case x of
        Empty -> "Empty"
        Textual s -> "Textual " ++ show s
        Hidden d -> "Hidden " ++ show d
        HLine w -> "HLine " ++ show w
        VLine w -> "VLine " ++ show w
        HGap w -> "HGap " ++ show w
        Gap w h -> "Gap " ++ show w ++ " " ++ show h
        HSeq a b -> "HSeq (" ++ show a ++ ") (" ++ show b ++ ")"
        VSeq a b -> "VSeq (" ++ show a ++ ") (" ++ show b ++ ")"
        HFit a b -> "HFit " ++ show a ++ " " ++ show b
        Table a -> "Table " ++ show a
        Translate a b d -> "Translate " ++ show a ++ " " ++ show b ++ " (" ++ show d ++ ")"
        Overlay a b -> "Overlay (" ++ show a ++ ") (" ++ show b ++ ")"
        DebugPutStr s -> "DebugPutStr " ++ show s
        GetFontSize _ -> "GetFontSize <lambda>"
        GetCellBounds _ -> "GetCellBounds <lambda>"
        GetBounds d _ -> "GetBounds (" ++ show d ++ ") <lambda>"

instance Mo.Monoid Drawing where
    mempty = Empty
    mappend = Overlay

newtype UserUnit
    = MkUserUnit Double
    deriving (Show, Read, Eq, Ord, Num, Fractional)

type NumSpace = Double

-- * Parameters

-- | The default drawing parameter for standard Western musical notation.
defDrawParm :: Param
defDrawParm = MkParam "bravura" 32

-- | The default drawing parameter for numbered musical notation.
numberedDrawParm :: Param
numberedDrawParm = MkParam "sans-serif" 16

data Param
    = MkParam
    {
        drawParmFontFace :: FontFace
        , drawParmFontSize :: FontSize
        -- TODO merge defDrawParm and numberedDrawParm?
        -- , drawParmTextFontFace :: FontFace
        -- , drawParmTextFontSize :: FontSize
    }
    deriving (Show, Read, Eq)

type FontFace = String
type FontSize = Double
