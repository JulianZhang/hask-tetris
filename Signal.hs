module Signal where --(registerSignals, runTetris)

import Graphics.UI.Gtk
import Data.IORef
import Data.Word(Word32)
import Graphics.Rendering.Cairo
import Layout
import Structure
import Logic

-- for regular timeout, we pass stepDown to the drawing part.
stepDown = (-1) :: Word32

registerSignals :: (LayoutInfo, IORef Field) -> IO ()
registerSignals layoutInfo refField = do

    mainWindow layoutInfo `on`        objectDestroy mainQuit
    pauseB     layoutInfo `onClicked` pauseButtonAffair layoutInfo
    restartB   layoutInfo `onClicked` resetAll  layoutInfo refField
    infoB      layoutInfo `onClicked` showInfo
    quitB      layoutInfo `onClicked` ( widgetDestroy mainWindow >> mainQuit )

    widgetAddEvents (drawingArea layoutInfo) [PointerMotionMask]
    widgetAddEvents (previewArea layoutInfo) [PointerMotionMask]

    drawingArea layoutInfo `on` exposeEvent drawMainArea     layoutInfo refField stepDown
    previewArea layoutInfo `on` exposeEvent drawPreviewArea layoutInfo refField

    mainWindow `on` keyPressEvent $ tryEvent $ do 
                                    val <- eventKeyVal 
                                    liftIO $ drawMainArea    layoutInfo refField val
                                    liftIO $ drawPreviewArea layoutInfo refField
                                    return ()

runTetris :: (LayoutInfo, IORef Field) -> IO ()
runTetris (layoutInfo, _ ) = do
          run layoutInfo
          widgetShowAll (mainWindow layoutInfo)
          mainGUI
    
-- we need MVar to be passed.
pauseButtonAffair layoutInfo = do
    let pause = pauseB layoutInfo
    buttonSetUseStock pause True
    onToggled pause $ do
              isPause <- toggleButtonGetActive pause
              case isPause of
                   False -> run   layoutInfo
                   True  -> pause layoutInfo

--timeoutAdd IO Bool -> Int-> IO Handler
run   layoutInfo = do
                 -- previewArea is drawed later for drawingMainArea will update the field
                 handler <- flip timeoutAdd 33
                            ( widgetQueueDraw drawingArea >> 
                             widgetQueueDraw previewArea >> return True )
                 let (_, setTimerId) = timerId layoutInfo
                 setTimerId handler
                  
pause layoutInfo = do 
                 let (getTimerId, _) = timerId layoutInfo
                 getTimerId >>= maybe (return ()) timeoutRemove

showInfo = do
    ad <- aboutDialogNew
    aboutDialogSetName     ad $ "Hask-Tetris"
    aboutDialogSetVersion  ad $ "1.0"
    aboutDialogSetAuthors  ad $ ["Peng Xingtao " ++ "<peng.pxt@gmail.com>"]
    aboutDialogSetComments ad $ "Heading For There"
    dialogRun              ad
    widgetDestroy          ad