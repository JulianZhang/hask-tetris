module Signal where


import Graphics.GUI.Gtk
import Layout
import Structure
import Logic

-- type HandlerId = CUInt
timerId = MVar (fromInteger 0) :: MVar HandlerId

registerSignals :: DrawInfo -> IO DrawInfo
registerSignals drawInfo = do

    mainWindow drawInfo `on` objectDestroy mainQuit
   
    pauseButtonAffair drawInfo

    pauseB   drawInfo `onClicked` pause
    restartB drawInfo `onClicked` 
    infoB    drawInfo `onClicked` showInfo
    quitB    drawInfo `onClicked` ( do widgetDestroy mainWindow
                                       mainQuit )

          vBoxMain       :: Box
          hBoxMain       :: Box
          aFrame         :: AspectFrame
          drawingArea    :: DrawingAera
          vBoxSub        :: Box
          previewArea    :: DrawingAera
          labelScore     :: Label
          labelLevel     :: Label
          hButtonBox     :: HButtonBox
          pauseB         :: Button
          restartB       :: Button
          infoB          :: Button
          quitB          :: Button
    
    return drawInfo 


-- this belong to signaling part.
        onClicked qr $ do
            (liftM length) getCars >>= setCars . newCarList
            getCurrentTime >>= setTimeStamp
            widgetQueueDraw drawingArea

        buttonSetUseStock qp True
        onToggled qp $ do
            p <- toggleButtonGetActive qp
            case p of
                True -> pause
                False -> resume

        onClicked qq (do
                       widgetDestroy mainWindow
                       mainQuit)

    widgetAddEvents dr [PointerMotionMask]
    on dr motionNotifyEvent $ do
            (r,t) <- eventPolarCoordinates
            liftIO $ if (0.8<r && r<1.2)
                then setJam (Just t)
                else setJam Nothing
            liftIO $ widgetQueueDraw dr
            return True

        on dr leaveNotifyEvent $ liftIO $
            setJam Nothing >> return True

        on dr exposeEvent $ do
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

--|||||||||| we need more carefully design here.
run drawInfo  = do
                Just =<< flip timeoutAdd 33
                (step >> widgetQueueDraw previewArea >> widgetQueueDraw drawingArea >> return True)

pause =  do
         maybe (return ()) timeoutRemove =<< getTimeoutId
         setTimeoutId Nothing


showInfo = do
    ad <- aboutDialogNew
    aboutDialogSetName     ad $ "Hask-Tetris"
    aboutDialogSetVersion  ad $ "1.0"
    aboutDialogSetAuthors  ad $ ["Peng Xingtao " ++ "<peng.xingtao@gmail.com>"]
    aboutDialogSetComments ad $ "Practising "  ++ " Practising"
    dialogRun              ad
    widgetDestroy          ad