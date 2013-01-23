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
coordinateTransform wUnit hUnit p = ( (fromIntegral $ xp p) * wUnit, (fromIntegral $ yp p) * hUnit,
                                       wUnit, hUnit )

drawRectUnit :: (Double, Double, Double, Double) -> Render ()
drawRectUnit (x, y, w, h) =  rectangle x y w h >> fillPreserve >> stroke

tetrisPreviewRender field dw w h = renderWithDrawable dw $ do
                   let block = backupBlock field
                       wUnit = w / 5
                       hUnit = h / 5
                       coors = map (coordinateTransform wUnit hUnit) $ coordinate block -- we get the 4 coordinates
                       (r,g,b,a) = color block
                   setSourceRGBA r g b a
                   setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth (hUnit / 15))
                   translate (0) (0)
                   mapM drawRectUnit coors

tetrisMainRender field dw w h= renderWithDrawable dw $ do
                let block = currentBlock field
                    wUnit = w / (fromIntegral maxColumns)
                    hUnit = h / (fromIntegral maxRows)
                    (r,g,b,a) = color block
                    coorBlock = map (coordinateTransform wUnit hUnit) $ coordinate block
                    coors     = map (coordinateTransform wUnit hUnit) $ markField field
                setSourceRGBA r g b a
                setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth $ hUnit / 200)
                mapM drawRectUnit coorBlock

                setSourceRGBA 0.65 0.16 0.16 1.0 -- set to greyish color
                setLineCap LineCapRound >> setLineJoin LineJoinRound >> (setLineWidth $ hUnit / 180)
                mapM drawRectUnit coors