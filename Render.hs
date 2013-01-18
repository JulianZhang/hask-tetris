module Render where (tetrisMainRender, tetrisPreviewRender)

import Gtaphics.UI.Gtk.Cairo
import Structure

-- remember, draw two bricks wall in the both vertical sides.
-- settledColor = grey = Brown = (165, 42, 42) = (0.65, 0.16, 0.16, 1.0)

-- get the coordinate used in 
coordinateTransform :: Position -> Double -> Double ->(Double, Double)
coordinateTransform p vUnit hUnit= ( (xp p) * hUnit, (yp p) * yUnit )

drawRectUnit :: Double -> Double -> Double -> Double -> C.Render ()
drawRectUnit x y w h =  rectangle x y w h >> fillPreserve >> stroke

tetrisPreviewRender dw field w h = do $
             renderWithDrawable dw do $
                   let block = backupBlock field
                       vUnit = v / 6
                       hUnit = h / 8
                       units = map (coordinateTransform vUnit hUnit) $ coordinate block -- we get the 4 coordinates
                       (r,g,b,a) = color block
                   setSourceRGBA r g b a
                   setLineCap LineCapRound >> setLineJoin LineJoinRound >> setLineWidth $ hUnit / 15
                   translate (w / 4) (3 * h / 4)
                   mapM drawRectUnit units

tetrisMainRender dw field w h = do $
          renderWithDrawable dw do $
                newPath
                setSourceRGB 0 0 1
                 

data Block = Block {
         shapeV       :: ShapeV,    -- shape type and current variant
         color        :: Color
         coordinate   :: [Position] -- current postion, row and column coordinate, 4 units
         } deriving (Show)


