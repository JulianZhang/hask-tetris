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

-- shape variants
data ShapeV = ShapeV (Shape, Int) -- shape and their transformers, represent by Int

data Position = Position {
                xp ::  Int
                yp ::  Int
                } deriving (Show)

-- shape: use Int to represent
data Block = Block {
         shapeV       :: ShapeV     -- shape type and current variant
         coordinate   :: [Position] -- current postion, row and column coordinate, 4 units.
         } deriving (Show)

-- we first use List, may change to Data.Vector in future.
data Field = Field {
         fieldArear  :: (Int, Int)  -- the battle field of TETRIS' coordiante
         markField   :: [(Position, Bool)] -- 24 x 20
         }

-- what is the logic should this be?
-- we only needs to check whether it can transform , can move down

canMoveDown :: Block -> Field -> Bool
canMoveDown block field = let yy  = map ( (+1) . yp ) $ coordinate block
                              marks = map  (yp . fst) . filter ( == True . snd ) $ markField field 
                           in not . or . map (\ y -> any (elem y) marks ) $ yy

canTransform :: Block -> Field -> Bool
canTransform block field = let newP = 

getNextTransformBlock :: Block -> Block
getNextTransformBlock block = let nextVariant  = getNextVariant $ shapeV block
                                  newPositions = getNewPostions nextVariant $ coordinate block
                               in Block { shapeV     = nextVariant,
                                          coordinate = newPositions }
 
            where 
            getNextVariant :: ShapeV -> ShapeV
            getNextVariant ShapeV (shape, variant) 
               = case shape of
                   I -> Shape (shape, mod (variant + 1) 2)
                   L -> Shape (shape, mod (variant + 1) 4)
                   J -> Shape (shape, mod (variant + 1) 4)
                   O -> Shape (shape, variant)
                   S -> Shape (shape, mod (variant + 1) 2)
                   Z -> Shape (shape, mod (variant + 1) 2)
                   T -> Shape (shape, mod (variant + 1) 4)                       
            
            getNewPostions shapeV coors = 

  

getBlockPosition :: Block -> 


-- data ControlInfo
data BoundM a = Crash | Trans a

instance Monad BoundM where
     return x      =  Trans x
     Crash >>= f   =  Crash
     Trans x >>= f =  f x
     fail _        =  Crash
