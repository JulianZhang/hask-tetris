module Render where (tetrisMainRender, tetrisPreviewRender)

import Gtaphics.UI.Gtk.Cairo
import Structure

-- remember, draw two bricks wall in the both vertical sides.
-- and each block shape has its own color

-- after settled, the color turns to Grey. (draw field area can do this)
-- I orange | J: purple | L: blue | O: yellow| S: green | Z: pink | T: red 


tetrisMainRender dw field w h = do $
          renderWithDrawable dw do $
                newPath
                setSourceRGB 0 0 1
                 




tetrisPreviewRender dw field w h = do $
             renderWithDrawable dw do $
                 


road2render :: Maybe Double -> [(Double,Double)] -> Render ()
road2render jam cars = do
    newPath
    setSourceRGB 0 0 1
    drawRoad
    when (isJust jam) drawJam
    setSourceRGBA 0 1 0 0.55
    let cars' = map fst cars
    let rotations = zipWith subtract (0:cars') cars'
    sequence_ $ map ((>> drawCar) . rotate) rotations
 where
    drawRoad = setLineWidth 0.02 >> setDash [2*pi/34,2*pi/34]
     (pi/34) >> arc 0.0 0.0 1.0 0.0 (2*pi) >> stroke
    drawJam = setLineWidth 0.005 >> setDash [0.03,0.02] 0.04 >>
     save >> rotate (fromJust jam) >> moveToLineTo 0.8 0 1.2
     0 >> stroke >> setDash [] 0 >> moveToLineTo 0.8 (-0.015)
     0.8 0.015 >> moveToLineTo 1.2 (-0.015) 1.2 0.015 >> stroke
     >> restore
    drawCar = arc 1 0 (carSize/2) 0 (2*pi) >> fill


-- get the coordinate used in 
coordinateTransform :: Position -> (Int, Int)
coordinateTransform p = ( (xp p) * cellSize, (yp p) * cellSize )
