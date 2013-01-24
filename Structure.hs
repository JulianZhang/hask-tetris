module Structure where

import Graphics.UI.Gtk
import Data.Time.Clock
import Data.List
import Data.IORef

-- draw part should be totally separated from the logic
data LayoutInfo = LayoutInfo {
          mainWindow     :: Window        , 
          drawingArea    :: DrawingArea   , 
          previewArea    :: DrawingArea   , 
          labelScore     :: Label         , 
          labelLevel     :: Label         , 
          pauseB         :: ToggleButton  , 
          restartB       :: Button        , 
          infoB          :: Button        , 
          quitB          :: Button        , 
          initTime       :: !UTCTime      , 
          timerId        :: !(IO (Maybe HandlerId), (Maybe HandlerId) -> IO ())
          }

-- shape: use Int to represent
data Block = Block {
         shapeV       :: !ShapeV,    -- shape type and current variant
         color        :: !(Double, Double, Double, Double),
         coordinate   :: ![Position] -- current postion, row and column coordinate, 4 units
         } deriving (Show)

-- we first use List, may change to Data.Vector in future.
data Field = Field {
         bGameOver     :: !Bool  , 
         currentBlock  :: !Block , 
         backupBlock   :: !Block , 
         markField     :: ![Position]
         } deriving (Show)


data Shape = I | J | L | O | S | Z | T 
             deriving (Eq)

-- shape variants. make it instance Show?
data ShapeV = ShapeV (Shape, Int) -- shape and their transformers, represent by Int
instance Show ShapeV where
     show ( ShapeV (I, n) ) = "I " ++ show n 
     show ( ShapeV (J, n) ) = "J " ++ show n 
     show ( ShapeV (L, n) ) = "L " ++ show n 
     show ( ShapeV (S, n) ) = "S " ++ show n 
     show ( ShapeV (Z, n) ) = "Z " ++ show n 
     show ( ShapeV (T, n) ) = "T " ++ show n 
     show ( ShapeV (O, n) ) = "O " ++ show n 
     
data Position = Position {
                xp ::  Int,
                yp ::  Int
                } deriving (Show, Eq)

instance Num Position where 
     a + b = Position { xp = xp a + xp b, yp = yp a + yp b } 
     a - b = Position { xp = xp a - xp b, yp = yp a - yp b } 
     a * b = Position { xp = xp a * xp b, yp = yp a * yp b } 
     negate a = Position { xp = negate $ xp a , yp = negate $ yp a }
     abs    a = error "abs is not implemented"
     signum a = error "signum is unimplemented"
     --fromInteger a = error "fromInteger is unimplemented"
