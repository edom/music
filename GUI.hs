{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module GUI
where

import qualified Control.Concurrent as C
import qualified Control.Monad as M

import qualified Graphics.UI.Gtk as Gu
import qualified Graphics.Rendering.Cairo as Ca

import qualified Graphics.UI.Threepenny as Gt

-- * Main loop

run :: G a -> IO a
run act = do
    M.void Gu.initGUI
    result <- _unG act
    Gu.mainGUI
    return result

quit :: G ()
quit = fromIo Gu.mainQuit

newtype G a = MkG { _unG :: IO a }
instance Functor G where fmap f = MkG . fmap f . _unG
instance Monad G where
    return = MkG . return
    (>>=) m k = MkG $ _unG m >>= _unG . k

data Widget
    = Window Gu.Window
    | Button Gu.Button
    | Canvas Gu.DrawingArea

fromIo :: IO a -> G a
fromIo = MkG

-- * Creation

window :: G Widget
window = MkG $ fmap Window Gu.windowNew

button :: G Widget
button = MkG $ fmap Button Gu.buttonNew

{- |
See also 'canvasWith'.
-}
canvas :: G Widget
canvas = MkG $ fmap Canvas Gu.drawingAreaNew

type Render a = Ca.Render a

{- |
See also 'setSizeRequest'.
-}
canvasWith :: Render () -> G Widget
canvasWith render = MkG $ do
    w <- Gu.drawingAreaNew
    M.void $ Gu.on w Gu.draw render
    return $ Canvas w

-- * Parenting

-- | @add a b@ adds a to b. This returns the parent.
add :: Widget -> Widget -> G Widget
add (Window c) par@(Window p) = MkG $ do
    Gu.containerAdd p c
    return par
add (Button c) par@(Window p) = MkG $ do
    Gu.containerAdd p c
    return par
add (Canvas c) par@(Window p) = MkG $ do
    Gu.containerAdd p c
    return par
add _ par = return par

-- * Size

setSizeRequest :: Int -> Int -> Widget -> G Widget
setSizeRequest w h x = MkG $ withGtkWidget x (return x) $ \ (MkGtkWidget c) ->
    Gu.widgetSetSizeRequest c w h >> return x

-- * Properties

setLabel :: String -> Widget -> G Widget
setLabel s x@(Button w) = MkG $ do
    Gu.buttonSetLabel w s
    return x
setLabel s x@(Window w) = MkG $ do
    Gu.set w [Gu.windowTitle Gu.:= s]
    return x
setLabel _ x = return x

setTitle :: String -> Widget -> G Widget
setTitle s x@(Window w) = MkG $ do
    Gu.set w [Gu.windowTitle Gu.:= s]
    return x
setTitle _ x = return x

getTitle :: Widget -> G String
getTitle (Window w) = MkG $ Gu.get w Gu.windowTitle
getTitle _ = return ""

-- * Events

onActivate :: Widget -> G () -> G ()
onActivate w a = MkG $ f w
    where
        f (Button c) = M.void $ Gu.on c Gu.buttonActivated (_unG a)
        f _ = return ()

onDestroy :: Widget -> G () -> G ()
onDestroy w a = MkG $ f w
    where
        f (Button c) = M.void $ Gu.on c Gu.objectDestroy (_unG a)
        f (Window c) = M.void $ Gu.on c Gu.objectDestroy (_unG a)
        f (Canvas c) = M.void $ Gu.on c Gu.objectDestroy (_unG a)

-- * Showing

showAll :: Widget -> G ()
showAll = fromIo . f
    where
        f (Window c) = Gu.widgetShowAll c
        f (Button c) = Gu.widgetShowAll c
        f (Canvas c) = Gu.widgetShowAll c

-- * Destruction

destroy :: Widget -> G ()
destroy = MkG . f
    where
        f (Window c) = Gu.widgetDestroy c
        f (Button c) = Gu.widgetDestroy c
        f (Canvas c) = Gu.widgetDestroy c

-- * GTK-specific internals

data GtkWidget = forall a. (Gu.WidgetClass a) => MkGtkWidget a

asGtkWidget :: Widget -> Maybe GtkWidget
asGtkWidget (Window c) = Just $ MkGtkWidget c
asGtkWidget (Button c) = Just $ MkGtkWidget c
asGtkWidget (Canvas c) = Just $ MkGtkWidget c

withGtkWidget :: Widget -> a -> (GtkWidget -> a) -> a
withGtkWidget w x y = maybe x y $ asGtkWidget w

-- * Threepenny example

-- | To see the example, point your browser to localhost port 10000.
threepennyExample :: (Gt.Window -> Gt.UI ()) -> IO ()
threepennyExample setup = do
    t <- C.forkIO $ Gt.startGUI Gt.defaultConfig { Gt.tpPort = Just 10000 } setup
    M.void getLine
    C.killThread t
