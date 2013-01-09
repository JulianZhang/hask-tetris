module Layout where

import Graphics.UI.Gtk
import Data.List
import Control.Monad
import Control.Concurrent
import System.IO
import System.Process

import Utils

-- remember, draw two bricks wall in the both vertical sides.
-- and each shape has its own color
-- after settled, the color turns to Grey.
-- I orange | J: purple | L: blue | O: yellow| S: green | Z: pink | T: red 


initTetris :: DrawInfo
initTetris = do
             initGui
             
             generateLayout
