module Layout where

import Graphics.UI.Gtk
import Data.List
import Control.Monad
import Control.Concurrent
import System.IO
import System.Process

import Utils
import Logic
-- remember, draw two bricks wall in the both vertical sides.
-- and each block shape has its own color
-- after settled, the color turns to Grey. (draw field area can do this)
-- I orange | J: purple | L: blue | O: yellow| S: green | Z: pink | T: red 

maxRows        = 24 :: Int
maxColumn      = 18 :: Int

cellSize       = 20 :: Int
cellBorderSize = 1  :: Int

canvasWidth  = cellSize * maxColumn
canvasHeight = cellSize * maxRows

-- get the coordinate used in 
coordinateTransform :: Position -> (Int, Int)
coordinateTransform p = ( (xp p) * cellSize, (yp p) * cellSize )

initTetris :: DrawInfo
initTetris = do
             initGui
             
             generateLayout
