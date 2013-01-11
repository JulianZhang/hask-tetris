module Layout where

import Graphics.UI.Gtk
import Data.List
import Control.Monad
import Control.Concurrent
import System.IO
import System.Process

import Utils
import Logic
-- remember, draw two bricks wall in the both vertical sides.
-- and each block shape has its own color
-- after settled, the color turns to Grey. (draw field area can do this)
-- I orange | J: purple | L: blue | O: yellow| S: green | Z: pink | T: red 

initTetrisLayout :: DrawInfo
initTetrisLayout = do

    initGui
    mainWindow   <-  windowNew
    drawingArea  <-  drawingAreaNew
    previewArea  <-  drawingAreaNew
    
    -- we make all buttons in a horizonal button box
    pauseB       <- toggleButtonNewWithLabel stockMediaPause
    restartB     <- buttonNewFromStock stockClear
    infoB        <- buttonNewFromStock stockAbout
    quitB        <- buttonNewFromStock stockQuit
    hButtonBox   <- hButtonBoxNew
    containerAdd vButtonBox pauseB
    containerAdd vButtonBox restartB
    containerAdd vButtonBox infoB
    containerAdd vButtonBox quitB

    -- score labels
    strScore     <- labelNewWithMnemonic "__S.C.O.R.E"
    strScore     <- labelNewWithMnemonic "__L.E.V.E.L"
    labelScore   <- labelNew $ Just "0"
    labelLevel   <- labelNew $ Just "0"

    -- we put the drawArea upon a frame
    aFrame <- aspectFrameNew 0.5 0.5 (Just (maxColumns / maxRows))
    frameSetShadowType aFrame ShadowNone
    containerAdd aFrame drawingArea

    -- we start do layout
    vBoxMain     <- vBoxNew False 1
    vBoxSub      <- vBoxNew False 1
    hBoxMain     <- hBoxNew False 1

    -- preview and score labels put in the right side
    containerAdd vBoxSub previewArea PackGrow    0
    containerAdd vBoxSub strScore    PackNatural 0
    containerAdd vBoxSub labelScore  PackNatural 0
    containerAdd vBoxSub strLevel    PackNatural 0
    containerAdd vBoxSub labelLevel  PackNatural 0

    -- aFrame and vBoxSub are put into a hBox

    boxPackStart hBoxMain aFrame     PackGrow    0
    boxPackStart hBoxMain vBoxSub    PackNatural 0
    boxPackStart vBoxMain hBoxMain   PackNatural 0
    boxPackStart vBoxMain hButtonBox PackNatural 0

    -- we put it into window
    containerAdd mainWindow vBoxMain

    windowSetTitle mainWindow "Hask-Tetris"
    windowSetDefaultSize mainWindow 640 480

    return LayoutInfo {
                 windows     =   windows      , 
                 vBoxMain    =   vBoxMain     , 
                 hBoxMain    =   hBoxMain     , 
                 aFrame      =   aFrame       , 
                 drawingArea =   drawingArea  , 
                 vBoxSub     =   vBoxSub      , 
                 previewArea =   previewArea  , 
                 labelScore  =   labelScore   , 
                 labelLevel  =   labelLevel   , 
                 hButtonBox  =   hButtonBox   , 
                 pauseB      =   pauseB       , 
                 restartB    =   restartB     , 
                 infoB       =   infoB        , 
                 quitB       =   quitB       
                 }   

