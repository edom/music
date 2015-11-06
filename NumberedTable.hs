module NumberedTable
where

import qualified Data.Maybe as Mb
import qualified Text.Printf as Pf

import qualified Graphics.UI.Threepenny as G

import qualified Accidental as A
import qualified BeatExpansion as B
import qualified Glyph as Gl
import qualified Pitch as P
import qualified Table as T
import qualified Voice as V

-- * Transforming user model into drawing model

-- | The row that is the drawing of the event without beams and dots.
eventRow :: V.Event B.Expansion (V.Type P.Doremi) -> [Content]
eventRow (V.MkEvent rhy typ) = g typ rhy
    where
        g V.Rest = B.fold [Rest] id (++)
        g (V.Note pitch) =
            replaceFirst note . B.fold [Beat] id (++)
            where
                -- TODO accidental slashes and octave dots
                note = Note $ show $ 1 + P.diatoneNum (P.classOf pitch)

upperDotsRow :: V.Event B.Expansion (V.Type P.Doremi) -> [Content]
upperDotsRow (V.MkEvent rhy typ) = f typ rhy
    where
        f V.Rest = B.fold [Empty] id (++)
        f (V.Note pitch) =
            replaceFirst dots . B.fold [Empty] id (++)
            where
                -- TODO accidental slashes and octave dots
                dots = Dots $ poz $ P.octaveOf pitch

-- XXX this is mostly a duplicate of the upperDotsRow
lowerDotsRow :: V.Event B.Expansion (V.Type P.Doremi) -> [Content]
lowerDotsRow (V.MkEvent rhy typ) = f typ rhy
    where
        f V.Rest = B.fold [Empty] id (++)
        f (V.Note pitch) =
            replaceFirst dots . B.fold [Empty] id (++)
            where
                -- TODO accidental slashes and octave dots
                dots = Dots $ poz $ negate $ P.octaveOf pitch

{- |
The columns that is the drawing of the number of beams.

See also 'B.beaming' and 'B.Expansion'.
-}
beamColumn :: Int -> [Content]
beamColumn n | n <= 0 = [empty]
beamColumn n = replicate n $ Beam minBeamWidth interbeamSpacing
    where
        minBeamWidth = 1
        interbeamSpacing = 4

eventBeamColumns :: V.Event B.Expansion (V.Type p) -> T.Table T.ColMajor Cell
eventBeamColumns event = T.fromColList $ map (map plain . beamColumn) $ B.beaming (V.durationOf event)

{- |
Combination of 'eventRow' and 'beamColumn' of the 'B.beaming' of the event.

The output is 'normalize'd.
-}
eventTable :: V.Event B.Expansion (V.Type P.Doremi) -> T.Table T.RowMajor Cell
eventTable event = case () of
    -- The column count of both tables should be the same
    -- because they come from the same rhythm.
    _ | upperColCount == lowerColCount ->
        T.stack [beamRow, upperDotsRow_, mainRow, lowerDotsRow_]
    _ -> error $ Pf.printf "eventTable: assertion failed; %d /= %d" upperColCount lowerColCount
    where
        upperColCount = T.colCountOf beamRow
        lowerColCount = T.colCountOf mainRow
        beamRow = T.transform . T.normalize empty $ eventBeamColumns event
        upperDotsRow_ = T.fromRowList [map centered $ upperDotsRow event]
        lowerDotsRow_ = T.fromRowList [map centered $ lowerDotsRow event]
        mainRow = T.fromRowList [map centered $ eventRow event]

-- * Rendering the table into Threepenny

class Render a where
    render :: a -> G.UI G.Element

instance (T.Major m, Render a) => Render (T.Table m a) where
    render table =
        G.table
        G.# G.set G.cellspacing 0
        G.# G.set G.cellpadding 0
        G.#+ map renderRow (T.rowsOf table)

instance Render Cell where
    render (MkCell align content) =
        G.td
        G.# G.set G.style (borderStyle content ++ alignStyle align ++ paddingStyle)
        G.#+ [render content]
        where
            alignStyle Center = [("text-align", "center")]
            alignStyle _ = []
            borderStyle (Beam _ _) = [("border-bottom", "1px solid #000")]
            borderStyle _ = []
            paddingStyle = [
                    ("padding-left", p)
                    , ("padding-right", p)
                ]
                where
                    p = "0.25em"

instance Render Content where
    render = f
        where
            f Empty = G.div
            f (Box w h) = G.div G.# G.set G.style [("width", show w ++ "px"), ("height", show h ++ "px")]
            f (Beam w h) = f (Box w h)
            f (Dots n) = G.string $ replicate n Gl.middleDot
            f Beat = G.string "."
            f Rest = G.string "0"
            f (Note str) = G.string str

renderRow :: (Render a) => [a] -> G.UI G.Element
renderRow row = G.tr G.#+ map render row

-- * Content construction

data Content
    = Empty -- ^ nothing at all
    | Box Width Height -- ^ empty box with the given size
    | Dots Int -- ^ octave dots
    | Beat
    | Rest
    | Beam MinWidth Height
    | Note String
    deriving (Show, Read)

-- * Cell construction

plain :: Content -> Cell
plain = MkCell NumberedTable.Left

-- * Content or cell construction

class Centered a where centered :: a -> Cell
instance Centered Content where centered = MkCell Center
instance Centered Cell where centered x = x { _cellAlign = Center }

class Empty a where empty :: a
instance Empty Content where empty = Empty
instance Empty Cell where empty = plain Empty

-- * Numbered notation as a table

type MinWidth = Double
type Width = Double
type Height = Double

data Cell
    = MkCell
    {
        _cellAlign :: Align
        , _cellContent :: Content
    }
    deriving (Show, Read)

data Align
    = Left
    | Center
    deriving (Show, Read)

-- * Aligning events into columns

{- |
Lay the voices out into columns
such that the same time corresponds to the same column.

An @Extend t@ function determines the continuation of a cell content,
should the event be split into multiple cells,
where @t@ is the cell content type.
-}
tabulate :: EmptyElem a -> Extend a -> [Voice a] -> [Column a]
tabulate empty_ extend_ = f []
    where
        f result voices =
            case col of
                [] -> reverse result
                _ -> f (col : result) rest
            where
                (col, rest) = stepTabulate empty_ extend_ voices

-- * Music model for layout

type EmptyElem a = a

type Extend a = a -> a
type Voice a = V.Voice V.BeatCount a
type Column a = [a]

-- * Total list functions

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

maybeMinimum :: (Ord a) => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum list = Just $ minimum list

-- * Internals

stepTabulate :: a -> (a -> a) -> [Voice a] -> (Column a, [Voice a])
stepTabulate empty_ extend_ voices =
    case smallestHeadDurationAmong voices of
        Nothing -> ([], voices)
        Just d -> unzip $ map (splitHead empty_ extend_ d) voices
    where
        smallestHeadDurationAmong =
            maybeMinimum . Mb.mapMaybe (fmap V.durationOf . maybeHead)

{- |
The duration must not exceed the duration of the head of the voice;
otherwise the result will be incorrect.
-}
splitHead :: a -> (a -> a) -> V.BeatCount -> Voice a -> (a, Voice a)
splitHead empty_ extend_ = f
    where
        -- p is reverse prefix
        -- r is remaining duration (split point)
        -- d is the suffix head duration
        -- e is the suffix head event
        -- s is the rest of the suffix
        f _ [] = (empty_, [])
        f r s | r <= 0 = (empty_, s)
        f r (V.MkEvent d e : s) | d <= r = (e, s)
        f r (V.MkEvent d e : s) = (e, V.MkEvent (d - r) (extend_ e) : s)

{- |
If the event is split across multiple columns,
then the first column will be the original event,
and the rest of the columns will be its extension.

See also the "NumberedBar" module.
-}
extend :: Content -> Content
extend (Beam _ h) = Beam 1 h
extend _ = Empty

-- | Positive or zero.
poz :: (Num a, Ord a) => a -> a
poz x | x >= 0 = x
poz _ = 0

replaceFirst :: a -> [a] -> [a]
replaceFirst _ [] = []
replaceFirst a (_ : b) = a : b
