module Render where --(tetrisMainRender, tetrisPreviewRender)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad.Trans
import Structure
import Layout(maxRows,maxColumns)

-- remember, draw two bricks wall in the both vertical sides.
-- settledColor = grey = Brown = (165, 42, 42) = (0.65, 0.16, 0.16, 1.0)

-- get the coordinate used in 
coordinateTransform :: Double -> Double -> Position -> (Double, Double, Double, Double)
coordinateTransform wUnit hUnit p = ( (fromIntegral $ xp p) * wUnit, (fromIntegral $ yp p) * hUnit, wUnit, hUnit )

drawRectUnit :: (Double, Double, Double, Double) -> Render ()
drawRectUnit (x, y, w, h) =  rectangle x y w h >> fillPreserve >> stroke

backupCoorTransform :: Double -> Double -> Position -> (Double, Double, Double, Double)
backupCoorTransform w h p = ( ((fromIntegral $ xp p) - 8 ) * 20 - 4, ( fromIntegral $ yp p) * 20 + 25, 20, 20) 

tetrisPreviewRender field dw regio w h = renderWithDrawable dw $ do
       case bGameOver field of
            True  -> do
                       setSourceRGBA 0.5 0.5 0.5 1.0
                       moveTo 2 2
                       setFontSize 20.0  
                       showText "GAME OVER" 
                       
                       setSourceRGBA 0.0 0.40 0.7 1
                       setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth 3)
                       moveTo 0 0 >> lineTo w 0 >> lineTo w h >> lineTo 0 h >> lineTo 0 0 >> stroke
                       --liftIO $ drawWindowEndPaint dw
                       return ()

            False -> do
                   let block = backupBlock field
                       coors = map (backupCoorTransform w h) $ coordinate block -- we get the 4 coordinates
                       (r,g,b,a) = color block
                   --liftIO $ drawWindowBeginPaintRegion dw regio
                   setSourceRGBA r g b a
                   setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth 3)
                   --translate (0) (0)
                   mapM drawRectUnit coors
                   setSourceRGBA 0.0 0.40 0.7 1
                   setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth 3)
                   moveTo 0 0 >> lineTo w 0 >> lineTo w h >> lineTo 0 h >> lineTo 0 0 >> stroke
                   --liftIO $ drawWindowEndPaint dw
                   return ()


tetrisMainRender field layoutInfo dw regio  w h = renderWithDrawable dw $ do
                let block = currentBlock field
                    wUnit = w / (fromIntegral maxColumns)
                    hUnit = h / (fromIntegral maxRows)
                    (r,g,b,a) = color block
                    coorBlock = map (coordinateTransform wUnit hUnit) $ coordinate block
                    coors     = map (coordinateTransform wUnit hUnit) $ markField field
                --liftIO $ drawWindowBeginPaintRegion dw regio
                setSourceRGBA 0.0 0.40 0.7 1
                moveTo 0 0 >> lineTo w 0 >> lineTo w h >> lineTo 0 h >> lineTo 0 0 >> stroke
                setSourceRGBA r g b a
                setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth 4)
                mapM drawRectUnit coorBlock

                setSourceRGBA 0.65 0.16 0.16 1.0 -- set to greyish color
                setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth 0)
                mapM drawRectUnit coors
                --liftIO $ drawWindowEndPaint dw
                return ()