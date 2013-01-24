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

    table'        <-  tableNew 2 4 False
    
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
    --containerSetBorderWidth table' 5

    -- score labels
    strScore'     <- labelNewWithMnemonic "S C O R E"
    strLevel'     <- labelNewWithMnemonic "L E V E L"
    labelScore'   <- labelNew $ Just "0"
    labelLevel'   <- labelNew $ Just "0"
    upperPad'     <- labelNew $ Just "T E T R I S"
    labelSetJustify strScore'   JustifyLeft 
    labelSetJustify strLevel'   JustifyLeft 
    labelSetJustify labelScore' JustifyLeft 
    labelSetJustify labelLevel' JustifyLeft 

    -- we start do layout
    tableAttach table' upperPad'    0 2 0 1 [Fill]   [] 0 10
    tableAttach table' drawingArea' 0 1 1 3 [Expand, Fill] [Expand, Fill] 10 0

    -- right part of vBoxSub and alignment
    vBoxSub'     <- vBoxNew False 5
    tableAttach table' vBoxSub' 1 2 1 2 [] [Fill] 0 0
    widgetSetSizeRequest strScore'    labelWidth   labelHeight
    widgetSetSizeRequest strLevel'    labelWidth   labelHeight
    widgetSetSizeRequest labelScore'  labelWidth   labelHeight
    widgetSetSizeRequest labelLevel'  labelWidth   labelHeight
    boxPackStart vBoxSub' previewArea' PackNatural 1 
    boxPackStart vBoxSub' strScore'    PackNatural 1 
    boxPackStart vBoxSub' labelScore'  PackNatural 1 
    boxPackStart vBoxSub' strLevel'    PackNatural 1 
    boxPackStart vBoxSub' labelLevel'  PackNatural 1 

    widgetSetSizeRequest previewArea'  previewWidth previewHeigh

    align' <- alignmentNew 0 0 0 0
    containerAdd align' labelLevel'
    tableAttach  table' align' 1 2 2 3 [] [Expand, Fill] 0 0    

    -- hButtonBox packing
    tableAttach  table' hButtonBox' 0 2 3 4 [Fill] [] 0 10
    -- we put it into window
    containerAdd mainWindow' table'


    windowSetTitle mainWindow' "Hask-Tetris"
    --windowSetDefaultSize mainWindow' 640 480
    widgetSetSizeRequest mainWindow'  windowWidth  windowHeight
    windowSetResizable   mainWindow'  False
    

    initTime' <- getCurrentTime
    (getTimerId,setTimerId) <- getAndSet Nothing

    return LayoutInfo {
                 mainWindow  =   mainWindow'   , 
                 drawingArea =   drawingArea'  , 
                 previewArea =   previewArea'  , 
                 labelScore  =   labelScore'   , 
                 labelLevel  =   labelLevel'   , 
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

windowWidth  = 602
windowHeight = 554

previewWidth = 95
previewHeigh = 110

labelWidth   = 110
labelHeight  = 20


{-
    -- we put the drawArea upon a frame
aFrame' <- aspectFrameNew 0.5 0.5 (Just (fromIntegral maxColumns / fromIntegral maxRows))
    frameSetShadowType aFrame' ShadowNone
    containerAdd aFrame' drawingArea'


-}