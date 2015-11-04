module NumberedTable
where

import qualified Graphics.UI.Threepenny as G
import qualified Text.Printf as Pf

import qualified Accidental as A
import qualified BeatExpansion as B
import qualified Pitch as P
import qualified Table as T
import qualified Voice as V

-- * Transforming user model into drawing model

-- | The row that is the drawing of the event without beaming.
eventRow :: V.Event P.Doremi -> [Cell]
eventRow (V.MkEvent rhy_ typ) = g typ rhy_
    where
        g V.Rest = B.fold [plain Rest] id (++)
        g (V.Note (P.MkPitch doremi _ oct)) =
            replaceFirst note . B.fold [plain Beat] id (++)
            where
                note = plain $ Note (P.MkPitch doremi A.none oct)
                replaceFirst _ [] = []
                replaceFirst a (_ : b) = a : b

{- |
The columns that is the drawing of the number of beams.

See also 'B.beaming' and 'B.Expansion'.
-}
beamColumn :: Int -> [Cell]
beamColumn n | n <= 0 = [empty]
beamColumn n = replicate n $ underlined (Box minBeamWidth interbeamSpacing)
    where
        minBeamWidth = 1
        interbeamSpacing = 4

eventBeamColumns :: V.Event p -> T.Table T.ColMajor Cell
eventBeamColumns event = T.fromColList $ map beamColumn $ B.beaming (V.rhythmOf event)

{- |
Combination of 'eventRow' and 'beamColumn' of the 'B.beaming' of the event.

The output is 'normalize'd.
-}
eventTable :: V.Event P.Doremi -> T.Table T.RowMajor Cell
eventTable event = case () of
    -- The column count of both tables should be the same
    -- because they come from the same rhythm.
    _ | upperColCount == lowerColCount ->
        T.stack [upperTable, lowerTable]
    _ -> error $ Pf.printf "eventTable: assertion failed; %d /= %d" upperColCount lowerColCount
    where
        upperColCount = T.colCountOf upperTable
        lowerColCount = T.colCountOf lowerTable
        upperTable = T.transform . T.normalize empty $ eventBeamColumns event
        lowerTable = T.fromRowList [eventRow event]

-- * Rendering the table into Threepenny

renderTable :: T.Table T.RowMajor Cell -> G.UI G.Element
renderTable table =
    G.table
    G.# G.set G.cellspacing 0
    G.# G.set G.cellpadding 0
    G.#+ map renderRow (T.rowsOf table)

renderRow :: [Cell] -> G.UI G.Element
renderRow row = G.tr G.#+ map renderCell row

renderCell :: Cell -> G.UI G.Element
renderCell (MkCell align border content) =
    G.td
    G.# G.set G.style (borderStyle border ++ alignStyle align ++ paddingStyle)
    G.#+ [renderContent content]
    where
        alignStyle Center = [("text-align", "center")]
        alignStyle _ = []
        borderStyle Bottom = [("border-bottom", "1px solid #000")]
        borderStyle _ = []
        paddingStyle = [
                ("padding-left", p)
                , ("padding-right", p)
            ]
            where
                p = "0.25em"

renderContent :: Content -> G.UI G.Element
renderContent = f
    where
        f Empty = G.div
        f (Box w h) = G.div G.# G.set G.style [("width", show w ++ "px"), ("height", show h ++ "px")]
        f (Dot n) = G.string $ replicate n '.'
        f Beat = G.string "."
        f Rest = G.string "0"
        f (Note pitch) = G.string $ show $ 1 + P.diatoneNum (P.classOf pitch) -- XXX accidental and octave

-- * Content construction

data Content
    = Empty -- ^ nothing at all
    | Box Width Height -- ^ empty box with the given size
    | Dot Int -- ^ octave dots
    | Beat
    | Rest
    | Note (P.Pitch P.Doremi)
    deriving (Show, Read)

-- * Cell construction

plain :: Content -> Cell
plain = MkCell NumberedTable.Left None

-- * Content or cell construction

class Underlined a where underlined :: a -> Cell
instance Underlined Content where underlined = MkCell NumberedTable.Left Bottom
instance Underlined Cell where underlined x = x { _cellBorder = Bottom }

class Centered a where centered :: a -> Cell
instance Centered Content where centered = MkCell Center None
instance Centered Cell where centered x = x { _cellAlign = Center }

class Empty a where empty :: a
instance Empty Content where empty = Empty
instance Empty Cell where empty = plain Empty

-- * Numbered notation as a table

type Width = Double
type Height = Double

data Cell
    = MkCell
    {
        _cellAlign :: Align
        , _cellBorder :: Border
        , _cellContent :: Content
    }
    deriving (Show, Read)

data Border
    = None
    | Bottom
    deriving (Show, Read)

data Align
    = Left
    | Center
    deriving (Show, Read)

type Row = [Cell]
