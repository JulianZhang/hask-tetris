module Layout where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Structure
import Logic(getAndSet)
import  

maxRows        = 24 :: Int
maxColumns     = 18 :: Int

cellSize       = 20 :: Int
cellBorderSize = 1  :: Int

canvasWidth  = cellSize * maxColumns
canvasHeight = cellSize * maxRows

-- get the coordinate used in 
coordinateTransform :: Position -> (Int, Int)
coordinateTransform p = ( (xp p) * cellSize, (yp p) * cellSize )

initTetrisLayout :: DrawInfo
initTetrisLayout = do

    initGui
    mainWindow'   <-  windowNew
    drawingArea'  <-  drawingAreaNew
    previewArea'  <-  drawingAreaNew
    
    -- we make all buttons in a horizonal button box
    pauseB'       <- toggleButtonNewWithLabel stockMediaPause
    restartB'     <- buttonNewFromStock stockClear
    infoB'        <- buttonNewFromStock stockAbout
    quitB'        <- buttonNewFromStock stockQuit
    hButtonBox'   <- hButtonBoxNew
    containerAdd vButtonBox' pauseB'
    containerAdd vButtonBox' restartB'
    containerAdd vButtonBox' infoB'
    containerAdd vButtonBox' quitB'

    -- score labels
    strScore     <- labelNewWithMnemonic "__S.C.O.R.E"
    strScore     <- labelNewWithMnemonic "__L.E.V.E.L"
    labelScore   <- labelNew $ Just "0"
    labelLevel   <- labelNew $ Just "0"
    upperPad     <- labelNew $ Nothing

    -- we put the drawArea upon a frame
    aFrame' <- aspectFrameNew 0.5 0.5 (Just (maxColumns / maxRows))
    frameSetShadowType aFrame' ShadowNone
    containerAdd aFrame' drawingArea'

    -- we start do layout
    vBoxMain'     <- vBoxNew False 1
    vBoxSub'      <- vBoxNew False 1
    hBoxMain'     <- hBoxNew False 1

    -- preview and score labels put in the right side
    containerAdd vBoxSub' previewArea' PackGrow    0
    containerAdd vBoxSub' strScore'    PackNatural 0
    containerAdd vBoxSub' labelScore'  PackNatural 0
    containerAdd vBoxSub' strLevel'    PackNatural 0
    containerAdd vBoxSub' labelLevel'  PackNatural 0

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
                 initTime    =   initTime'
                }   
