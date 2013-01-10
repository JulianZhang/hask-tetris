module Structure where

import Graphics.UI.Gtk
import Control.Concurrent
import Data.List
import qualified Data.Map as M

-- draw part should be totally separated from the logic
-- which means Structure and Logic is in the same part but, the draw and 
-- the coordinate translate in the same part.

data LayoutInfo = LayoutInfo {
          windows        :: Window
          drawArea       :: DrawingAera
          vBoxMain       :: Box
          vBoxMain       :: Box
          vBoxSub        :: Box
          pauseButton    :: Button
          restartButton  :: Button
          labelTitle     :: Label
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


-- data ControlInfo
data BoundM a = Crash | Trans a

instance Monad BoundM where
     return x      =  Trans x
     Crash >>= f   =  Crash
     Trans x >>= f =  f x
     fail _        =  Crash
