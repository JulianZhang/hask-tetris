module Signal where

import Graphics.GUI.Gtk
import Data.IORef

import Layout
import Structure
import Logic

-- for regular timeout, we pass stepDown to the drawing part.
stepDown = (-1) :: Word32

registerSignals :: (DrawInfo, IORef Field) -> IO ()
registerSignals drawInfo refField = do

    mainWindow drawInfo `on`        objectDestroy mainQuit
    pauseB     drawInfo `onClicked` pauseButtonAffair drawInfo
    restartB   drawInfo `onClicked` reset    drawInfo refField
    infoB      drawInfo `onClicked` showInfo
    quitB      drawInfo `onClicked` ( widgetDestroy mainWindow >> mainQuit )

    widgetAddEvents (drawingArea drawInfo) [PointerMotionMask]
    widgetAddEvents (previewArea drawInfo) [PointerMotionMask]

    drawingArea drawInfo `on` exposeEvent drawMainArea     drawInfo refField stepDown
    previewArea drawInfo `on` exposeEvent drawSPreviewArea drawInfo refField

    mainWindow `on` keyPressEvent $ keyboardReact drawInfo refField

runTetris :: (DrawInfo, MVar Field) -> IO ()
runTetris (drawInfo, _ ) = do
          run drawInfo
          widgetShowAll (mainWindow drawInfo)
          mainGUI
    
-- we need MVar to be passed.
pauseButtonAffair drawInfo = do
    let pause = pauseB drawInfo
    buttonSetUseStocl pause True
    onToggled pause $ do
              isPause <- toggleButtonGetActive pause
              case isPause of
                   False -> run   drawInfo
                   True  -> pause drawInfo

--timeoutAdd IO Bool -> Int-> IO Handler
run   drawInfo = do
                 -- previewArea is drawed later for drawingMainArea will update the field
                 handler <- flip timeoutAdd 33
                            ( widgetQueueDraw drawingArea >> 
                             widgetQueueDraw previewArea >> return True )
                 tryTakeMVar >> putMVar handler 
                  
pause drawInfo = tryTakeMVar timerId >>= maybe (return ()) timeoutRemove

showInfo = do
    ad <- aboutDialogNew
    aboutDialogSetName     ad $ "Hask-Tetris"
    aboutDialogSetVersion  ad $ "1.0"
    aboutDialogSetAuthors  ad $ ["Peng Xingtao " ++ "<peng.pxt@gmail.com>"]
    aboutDialogSetComments ad $ "Heading For There"
    dialogRun              ad
    widgetDestroy          ad