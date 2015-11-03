module DrawCairo
where

import qualified Control.Monad as M
import qualified Data.Monoid as Mo

import qualified Graphics.Rendering.Cairo as Ca

import qualified Draw as D
import qualified Rect as R
import qualified Table as T
import qualified TableLayout as Tl

{- |
This maps a 'D.Drawing' to an actual drawing on the screen.

Use 'D.defDrawParm' to get a 'D.Param'.
-}
draw :: D.Param -> D.Drawing -> Ca.Render ()
draw parm drawee = do
    -- any font that conforms to SMUFL: http://www.smufl.org/fonts/
    -- http://www.smufl.org/
    -- FIXME those fonts: license?
    -- Install using the 'Install' button in font viewer in ubuntu; install to ~/.fonts.
    Ca.selectFontFace fontFace Ca.FontSlantNormal Ca.FontWeightNormal
    Ca.setFontSize fontSize
    -- Staff lines look best on screen unantialiased.
    Ca.setAntialias Ca.AntialiasNone
    Ca.moveTo 0 0
    f drawee
    where
        fontFace = D.drawParmFontFace parm
        fontSize = D.drawParmFontSize parm
        preservingCurrentPoint act = do
            (x, y) <- Ca.getCurrentPoint
            M.void act
            Ca.moveTo x y
        f D.Empty = return ()
        f (D.Hidden d) = do
            -- XXX
            boundRect <- bounds parm d
            let
                D.MkUserUnit w = R.width boundRect
                D.MkUserUnit h = R.height boundRect
            Ca.relMoveTo w h
        f (D.Basic x) = preservingCurrentPoint $ basicDraw parm x
        f (D.Lambda x) = preservingCurrentPoint $ lambdaDraw parm x
        f (D.HGap (D.MkUserUnit w)) = Ca.relMoveTo w 0
        f (D.Gap (D.MkUserUnit w) (D.MkUserUnit h)) = Ca.relMoveTo w h
        f (D.HSeq u v) = do
            a <- bounds parm u
            b <- bounds parm v
            f u
            f (D.Translate (R.x1 a - R.x0 b) 0 v)
        f (D.VSeq u v) = do
            a <- bounds parm u
            b <- bounds parm v
            f u
            f (D.Translate 0 (R.y1 a - R.y0 b) v)
        f (D.HFit wantedWidth drawings) = do
            widths <- fmap (fit wantedWidth) $ M.mapM getWidth drawings
            loop 0 widths drawings
            where
                getWidth (D.HGap x) = return $ Resizable x
                getWidth d = do
                    boundRect <- bounds parm d
                    return $ Rigid $ R.width boundRect
                loop _ [] [] = return ()
                loop _ [] _ = Ca.liftIO $ putStrLn $ "DrawCairo.draw.f.HFit.loop: impossible: widths empty before drawings"
                loop _ _ [] = Ca.liftIO $ putStrLn $ "DrawCairo.draw.f.HFit.loop: impossible: drawings empty before widths"
                loop hpos (s : ss) (d : ds) = do
                    f (D.Translate hpos 0 d)
                    loop (hpos + s) ss ds
        f (D.Table tab) = preservingCurrentPoint $ do
            let rows = T.rowsOf tab
            bounds_ <- M.mapM (M.mapM (bounds parm)) rows
            let tableRects = T.rowsOf $ Tl.table $ fmap Tl.rectSize $ T.fromRowList bounds_
            M.zipWithM_ drawRow rows tableRects
            where
                drawRow = M.zipWithM_ drawCol
                drawCol drawing boundRect =
                    preservingCurrentPoint $ do
                        Ca.relMoveTo x y
                        g drawing
                    where
                        D.MkUserUnit x = R.x0 boundRect
                        D.MkUserUnit y = R.y0 boundRect
                        g (D.Lambda (D.GetCellBounds lam)) = f $ lam boundRect
                        g u = f u
        f (D.Translate (D.MkUserUnit x) (D.MkUserUnit y) d) = preservingCurrentPoint $ do
            Ca.relMoveTo x y
            f d
        f (D.Overlay x y) = f x >> f y
        f (D.DebugPutStr x) = Ca.liftIO $ putStr x

-- | Compute the size of a drawing.
bounds :: D.Param -> D.Drawing -> Ca.Render (R.Rect D.UserUnit)
bounds parm = f
    where
        f D.Empty = return R.empty
        f (D.Basic x) = basicBounds parm x
        f (D.Lambda x) = lambdaBounds parm x
        f (D.Hidden d) = f d
        f (D.HGap w) = return $ R.xywh 0 0 w 0
        f (D.Gap w h) = return $ R.xywh 0 0 w h
        f (D.HSeq u v) = do
            a <- f u
            b <- f v
            return a
                {
                    R.x1 = R.x0 a + R.width b
                    , R.y0 = min (R.y0 a) (R.y0 b)
                    , R.y1 = max (R.y1 a) (R.y1 b)
                }
        f (D.VSeq u v) = do
            a <- f u
            b <- f v
            return a
                {
                    R.y1 = R.y0 a + R.height b
                    , R.x0 = min (R.x0 a) (R.x0 b)
                    , R.x1 = max (R.x1 a) (R.x1 b)
                }
        f (D.HFit w d) = do
            h <- fmap (R.height . Mo.mconcat) $ M.mapM f d
            return $ R.xywh 0 0 w h
        f (D.Overlay a b) = M.liftM2 Mo.mappend (f a) (f b)
        f (D.Translate x y d) = fmap (R.translate x y) $ f d
        f x = do
            Ca.liftIO $ putStrLn $ "DrawCairo.bounds.f: not implemented: " ++ show x
            return R.empty

-- * Layout and fitting

data LayoutElem size
    = Rigid size
    | Resizable size
    deriving (Show)

{- |
Resize all resizables equally so that the sum of the sizes of the elements is equal to the wanted size.
-}
fit :: (Fractional size, Ord size) => size -> [LayoutElem size] -> [size]
fit wantedTotalSize elems =
    map adjustedSize elems
    where
        adjustedSize (Rigid s) = s
        adjustedSize (Resizable m) = m + additionPerResizable
        totalMinSize = sum $ map minSize elems
        numResizables = length $ filter isResizable elems
        roomDifference = wantedTotalSize - totalMinSize
        additionPerResizable
            | numResizables == 0 = 0
            | otherwise = roomDifference / fromIntegral numResizables
        isResizable (Resizable _) = True
        isResizable _ = False
        minSize (Rigid x) = x
        minSize (Resizable x) = x

-- * Internals

basicDraw :: D.Param -> D.BasicDrawing -> Ca.Render ()
basicDraw _ = f
    where
        f (D.Textual string) = Ca.showText string
        f (D.Line (D.MkUserUnit x) (D.MkUserUnit y)) = do
            Ca.setLineWidth staffThickness
            Ca.relLineTo x y
            Ca.stroke
        staffThickness = 1

lambdaDraw :: D.Param -> D.LambdaDrawing -> Ca.Render ()
lambdaDraw parm = f
    where
        f (D.GetFontSize c) = draw parm $ c $ D.MkUserUnit fontSize
        -- deprecated?
        f (D.GetBounds d c) = bounds parm d >>= draw parm . c
        -- XXX
        f x = Ca.liftIO $ putStrLn $ "DrawCairo.lambdaDraw: not implemented: " ++ show x
        fontSize = D.drawParmFontSize parm

basicBounds :: D.Param -> D.BasicDrawing -> Ca.Render (R.Rect D.UserUnit)
basicBounds _ = f
    where
        f (D.Textual s) = fmap fromTextExtents $ Ca.textExtents s
        f (D.Line w h) = return $ R.xywh 0 0 (max 1 w) (max 1 h)
        fromTextExtents u = R.xywh x y w h
            where
                x = D.MkUserUnit $ Ca.textExtentsXbearing u
                y = D.MkUserUnit $ Ca.textExtentsYbearing u
                w = D.MkUserUnit $ Ca.textExtentsWidth u
                h = D.MkUserUnit $ Ca.textExtentsHeight u

lambdaBounds :: D.Param -> D.LambdaDrawing -> Ca.Render (R.Rect D.UserUnit)
lambdaBounds parm = f
    where
        -- f (D.GetCellBounds _) = return R.empty -- FIXME
        f (D.GetBounds d lam) = bounds parm d >>= bounds parm . lam
        f (D.GetFontSize lam) = bounds parm $ lam $ D.MkUserUnit fontSize
        f x = do
            Ca.liftIO $ putStrLn $ "DrawCairo.lambdaBounds: not implemented: " ++ show x
            return R.empty
        fontSize = D.drawParmFontSize parm
