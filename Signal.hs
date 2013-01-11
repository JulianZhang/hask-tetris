module Utils where

import Control.Monad
import Data.List
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

    on mainWindow objectDestroy mainQuit
    widgetShowAll mainWindow
    mainGUI
