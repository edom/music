{-# LANGUAGE CPP #-}

module GUI
where

import qualified Control.Concurrent as C
import qualified Control.Monad as M

import qualified Graphics.UI.Threepenny as Gt

-- * Threepenny example

-- | To see the example, point your browser to localhost port 10000.
threepennyExample :: (Gt.Window -> Gt.UI ()) -> IO ()
threepennyExample setup = do
    t <- C.forkIO $ Gt.startGUI (setPort 10000 Gt.defaultConfig) setup
    M.void getLine
    C.killThread t

setPort :: Int -> Gt.Config -> Gt.Config
setPort port conf =
#if MIN_VERSION_threepenny_gui(0,6,0)
    conf { Gt.jsPort = Just port }
#else
    conf { Gt.tpPort = Just port }
#endif
