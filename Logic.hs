module Logis where

import Data.List
import Structure

-- when do transform of block, the according transform of its 4 units' transform
-- relative ones, the first one is the base case.

maxRows  = 24
maxColum = 18

shapeVI = [ [(0,0), (1,0), (2,0), (3,0),], [(0,0), (0,-1), (0,-2), (0,-3)] ]
--shapeVO = NoNeed for O
shapeVS = [ [(0,0), (1,0), (1,-1), (2,-1),], [(0,0), (0,1), (1, 1), (1,2)] ]
shapeVZ = [ [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ]

shapeVL = [ [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ,
            [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ]

shapeVJ = [ [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ,
            [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ]

shapeVT = [ [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ,
            [(0,0), (1,0), (1, 1), (2, 1),], [(0,0), (0,1), (-1,1), (2,2)] ]

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

            -- this func should take Boundary into consideration
            getNewPostions shapeV coors = 


canTransform :: Block -> Occupy -> Maybe Bool
canTransform = bottomCheck >>= occupyCheck 


bottomCheck :: Block -> Occupy -> Maybe Bool
bottomCheck block occupy = undefined


boundCheck  :: Block -> Occupy -> Maybe Bool
boundCheck  block occupy = undefined 


occupyCheck :: Block -> Occupy -> Maybe Bool
occupyCheck block occupy = undefined


meltBlocks :: Occupy -> Occupy
meltBlocks occpy = undefined

isGameOver :: Occupy -> Bool
isGameOver occupy = undefined

-- how to determine one block is settled?

