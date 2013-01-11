module Structure where

import Graphics.UI.Gtk
import Control.Concurrent
import Data.List
import qualified Data.Map as M

-- draw part should be totally separated from the logic
data LayoutInfo = LayoutInfo {
          windows        :: Window
          vBoxMain       :: Box
          hBoxMain       :: Box
          aFrame         :: AspectFrame
          drawingArea    :: DrawingAera
          vBoxSub        :: Box
          previewArea    :: DrawingAera
          labelCounter   :: Label
          labelLevel     :: Label
          hButtonBox     :: HButtonBox
          pauseB         :: Button
          restartB       :: Button
          infoB          :: Button
          quitB          :: Button
          } deriving (Show)

data Shape = I | J | L | O | S | Z | T 

-- shape variants. make it instance Show?
data ShapeV = ShapeV (Shape, Int) -- shape and their transformers, represent by Int
instance Show ShapeV where
     show I = "I"
     show J = "J"
     show L = "L"
     show S = "S"
     show Z = "Z"
     show T = "T"
     show O = "O"
     
data Position = Position {
                xp ::  Int,
                yp ::  Int
                } deriving (Show)
instance Num Position where 
     a + b = Position ( xp = xp a + xp b, yp = yp a + yp b)
     a - b = Position ( xp = xp a - xp b, yp = yp a - yp b)
     a * b = Position ( xp = xp a * xp b, yp = yp a * yp b)
     negate a = Position ( xp = negate $ xp a , yp = negate $ yp a)
     abs    a = error "abs is not implemented"
     signum a = error "signum is unimplemented"
     fromInteger a = error "fromInteger is unimplemented"

-- shape: use Int to represent
data Block = Block {
         shapeV       :: ShapeV,    -- shape type and current variant
         coordinate   :: [Position] -- current postion, row and column coordinate, 4 units
         } deriving (Show)

-- we first use List, may change to Data.Vector in future.
data Field = Field {
         fieldArea   :: (Int, Int),  -- the battle field of TETRIS' coordiante
         markField   :: [Position]   -- 24 x 20
         } deriving (Show)