module Signal where --(registerSignals, runTetris)

import Graphics.UI.Gtk
import Data.IORef
import Data.Word(Word32)
import Graphics.Rendering.Cairo

--import Layout
import Structure
import Logic

-- for regular timeout, we pass stepDown to the drawing part.
stepDown = (-1) :: Word32

registerSignals :: (LayoutInfo, IORef Field) -> IO (LayoutInfo, IORef Field)
registerSignals (layoutInfo, refField) = do
    let window = mainWindow layoutInfo
    on  window   objectDestroy mainQuit
    (pauseB     layoutInfo) `onToggled` pauseAffair layoutInfo
    (restartB   layoutInfo) `onClicked` resetAll  layoutInfo refField
    (infoB      layoutInfo) `onClicked` showInfo
    (quitB      layoutInfo) `onClicked` (widgetDestroy window >> mainQuit )

    widgetAddEvents (drawingArea layoutInfo) [PointerMotionMask]
    widgetAddEvents (previewArea layoutInfo) [PointerMotionMask]

    on (drawingArea layoutInfo) exposeEvent $ liftIO (drawMainArea layoutInfo refField stepDown)
    on (previewArea layoutInfo) exposeEvent $ liftIO (drawPreviewArea layoutInfo refField)

    on window keyPressEvent $ tryEvent $ do 
                              val <- eventKeyVal 
                              liftIO $ drawMainArea    layoutInfo refField val
                              liftIO $ drawPreviewArea layoutInfo refField
                              return ()
    
    return (layoutInfo, refField)

runTetris :: (LayoutInfo, IORef Field) -> IO ()
runTetris (layoutInfo, _ ) = do
          runIt layoutInfo
          widgetShowAll (mainWindow layoutInfo)
          mainGUI
    

pauseAffair layoutInfo = do
    let pause = pauseB layoutInfo
    isPause <- toggleButtonGetActive pause
    case isPause of
         False -> runIt   layoutInfo 
         True  -> pauseIt layoutInfo
    return ()
                     

--timeoutAdd IO Bool -> Int-> IO Handler
runIt   layoutInfo = do
                 -- previewArea is drawed later for drawingMainArea will update the field
                 handler <- flip timeoutAdd 20
                            ( widgetQueueDraw (drawingArea layoutInfo) >> 
                              widgetQueueDraw (previewArea layoutInfo) >> return True)
                 let (_, setTimerId) = timerId layoutInfo
                 setTimerId $ Just handler
                 return ()
                  
pauseIt layoutInfo = do 
                 let (getTimerId, _) = timerId layoutInfo
                 getTimerId >>= maybe (return ()) timeoutRemove
                 return ()

showInfo = do
    ad <- aboutDialogNew
    aboutDialogSetName     ad $ "Hask-Tetris"
    aboutDialogSetVersion  ad $ "1.0"
    aboutDialogSetAuthors  ad $ ["Peng Xingtao " ++ "<peng.pxt@gmail.com>"]
    aboutDialogSetComments ad $ "Heading For There, HardWater"
    dialogRun              ad
    widgetDestroy          ad