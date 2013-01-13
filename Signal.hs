module Signal where


import Graphics.GUI.Gtk
import Layout
import Structure
import Logic

-- field connect Logic, layout and render.

-- type HandlerId = CUInt
timerId = MVar (fromInteger 0) :: MVar HandlerId

registerSignals :: DrawInfo -> Field -> IO DrawInfo
registerSignals drawInfo field = do

    mainWindow drawInfo `on` objectDestroy mainQuit
   
    pauseB   drawInfo `onClicked` pauseButtonAffair drawInfo
    restartB drawInfo `onClicked` reset    drawInfo field
    infoB    drawInfo `onClicked` showInfo
    quitB    drawInfo `onClicked` ( widgetDestroy mainWindow >> mainQuit )

    widgetAddEvents (drawingArea drawInfo) [PointerMotionMask]
    widgetAddEvents (previewArea drawInfo) [PointerMotionMask]

    drawingArea drawInfo `on` exposeEvent drawMain draw field
    previewArea drawInfo `on` exposeEvent drawSub  draw -- we just randomly take one block.

    mainWindow `on` keyPressEvent $ kbReact field

    widgetShowAll mainWindow
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
runTetris drawInfo = do
                handler <- flip timeoutAdd 33
                          (updateStatus >> widgetQueueDraw previewArea >> widgetQueueDraw drawingArea >> return True)
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



drawMain drawInfo field = do 
    (w,h) <- eventWindowSize
    dw <- eventWindow
    liftIO $ do
      jam <- getJam
      cars <- getCars
      renderWithDrawable dw $ do
                            translate (w/2) (h/2)
                            scale (w/drawSide) (h/drawSide)
                            road2render jam cars
      return True

drawMain drawInfo field = do 
    (w,h) <- eventWindowSize
    dw <- eventWindow

kbReact field = do tryEvent $ do
                   kv  <- eventKeyVal
                   liftIO $ updateStatus kv field

{-
define XK_Home			0xFF50
define XK_Left			0xFF51	/* Move left, left arrow */
define XK_Up			0xFF52	/* Move up, up arrow */
define XK_Right		0xFF53	/* Move right, right arrow */
define XK_Down			0xFF54	/* Move down, down arrow */
define XK_Prior		0xFF55	/* Prior, previous */
define XK_Page_Up		0xFF55
define XK_Next			0xFF56	/* Next */
define XK_Page_Down		0xFF56
define XK_End			0xFF57	/* EOL */
define XK_Begin		0xFF58	/* BOL */
-}