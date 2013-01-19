module Layout where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Data.IORef
import Data.Time.Clock
import Structure

maxRows        = 24 :: Int
maxColumns     = 18 :: Int

initTetrisLayout :: IO LayoutInfo
initTetrisLayout = do
    initGUI
    mainWindow'   <-  windowNew
    drawingArea'  <-  drawingAreaNew
    previewArea'  <-  drawingAreaNew
    
    -- we make all buttons in a horizonal button box
    pauseB'       <- toggleButtonNewWithLabel stockMediaPause
    restartB'     <- buttonNewFromStock stockClear
    infoB'        <- buttonNewFromStock stockAbout
    quitB'        <- buttonNewFromStock stockQuit
    hButtonBox'   <- hButtonBoxNew
    containerAdd hButtonBox' pauseB'
    containerAdd hButtonBox' restartB'
    containerAdd hButtonBox' infoB'
    containerAdd hButtonBox' quitB'

    -- score labels
    strScore'     <- labelNewWithMnemonic "S C O R E"
    strLevel'     <- labelNewWithMnemonic "L E V E L"
    labelScore'   <- labelNew $ Just "0"
    labelLevel'   <- labelNew $ Just "0"
    upperPad'     <- labelNew $ Nothing

    -- we put the drawArea upon a frame
    aFrame' <- aspectFrameNew 0.5 0.5 (Just (fromIntegral maxColumns / fromIntegral maxRows))
    frameSetShadowType aFrame' ShadowNone
    containerAdd aFrame' drawingArea'

    -- we start do layout
    vBoxMain'     <- vBoxNew False 1
    vBoxSub'      <- vBoxNew False 1
    hBoxMain'     <- hBoxNew False 1

    -- preview and score labels put in the right side
    boxPackStart vBoxSub' previewArea' PackGrow    0
    boxPackStart vBoxSub' strScore'    PackNatural 0
    boxPackStart vBoxSub' labelScore'  PackNatural 0
    boxPackStart vBoxSub' strLevel'    PackNatural 0
    boxPackStart vBoxSub' labelLevel'  PackNatural 0

    -- aFrame and vBoxSub are put into a hBox
    boxPackStart hBoxMain' aFrame'     PackGrow    0
    boxPackStart hBoxMain' vBoxSub'    PackNatural 0
    boxPackStart vBoxMain' upperPad'   PackNatural 0 -- we add a upper pad.
    boxPackStart vBoxMain' hBoxMain'   PackNatural 0
    boxPackStart vBoxMain' hButtonBox' PackNatural 0

    -- we put it into window
    containerAdd mainWindow' vBoxMain'

    windowSetTitle mainWindow' "Hask-Tetris"
    windowSetDefaultSize mainWindow' 640 480

    initTime' <- getCurrentTime
    (getTimerId,setTimerId) <- getAndSet Nothing

    return LayoutInfo {
                 mainWindow  =   mainWindow'   , 
                 vBoxMain    =   vBoxMain'     , 
                 hBoxMain    =   hBoxMain'     , 
                 aFrame      =   aFrame'       , 
                 drawingArea =   drawingArea'  , 
                 vBoxSub     =   vBoxSub'      , 
                 previewArea =   previewArea'  , 
                 labelScore  =   labelScore'   , 
                 labelLevel  =   labelLevel'   , 
                 hButtonBox  =   hButtonBox'   , 
                 pauseB      =   pauseB'       , 
                 restartB    =   restartB'     , 
                 infoB       =   infoB'        , 
                 quitB       =   quitB'        ,
                 initTime    =   initTime'     ,
                 timerId     =   (getTimerId, setTimerId)
                 }   


-- closure
getAndSet :: a -> IO (IO a, a -> IO ())
getAndSet a = do
    ior <- newIORef a
    let get = readIORef ior
    let set = writeIORef ior
    return (get,set)
