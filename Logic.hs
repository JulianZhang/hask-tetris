module Logic where

import Data.List
import Structure

-- when do transform of block, the according transform of its 4 units' transform
-- relative ones, the first one is the base case.

maxRows  = 24
maxColum = 18

-- relative positio, header position
relativePO = [ [(0,0), (1,0), (0,1), (1,1)] ]
headerPO   = [(0, 0)]

relativePI = [ [(0,0), (1,0), (2,0),  (3,0) ], [(0,0), (0,-1), (0,-2), (0,-3)] ]
headerPI   = [ (-3,3), (3, -3)]

relativePL = [ [(0,0), (0,1),  (-1,1) , (-2,1)], [(0,0), (1,0), (1,1), (1,2)   ] ,
               [(0,0), (0,-1), (1,-1), (2, -1)], [(0,0), (-1,0),(-1,-1),(-1,-2)] ]
headerPL   = [ (1,-1), (-2,-1), (-1,-2), (2,0) ]

relativePJ = [ [(0,0), (0,1),  (1,1),  (2,1)  ], [(0,0), (1,0) , (1,-1), (1,-2) ] ,
               [(0,0), (0,-1), (-1,-1),(-2,-1)], [(0,0), (-1,0), (-1,1), (-1,2) ] ]
headerPJ   = [ (-2,-1), (0,1), (2,0), (0,-2) ]

relativePS = [ [(0,0), (1,0), (,-1), (2,11)], [(0,0), (0,1), (1, 1), (1,2)] ]
headerPS   = [ (-1,2), (0,-2)]

relativePZ = [ [(0,0), (1,0), (1,1), (2,1)], [(0,0), (0,1), (-1,1), (-1,2)] ]
headerPZ   = [ (-2,1), (1, -1)]


relativePT = [ [(0,0), (1,0), (2,0), (1,1)  ], [(0,0), (0,-1),(0,-2),(1,-1)] ,
               [(0,0), (-1,0),(-2,0),(-1,-1)], [(0,0), (0,1), (0,2), (-1,1)] ]
headerPT   = [ (-2,1), (1,1), (2,0), (0,2) ]


relativeP = [ relativePO, relativePI, relativePL,  relativePJ, relativePS, relativePZ,  relativePT]
headerP   = [ headerPO, headerPI, headerPL, headerPJ, headerPS, headerPZ,  headerPT]


getNextTransformBlock :: Block -> Block
getNextTransformBlock block = let nextVariant  = getNextVariant $ shapeV block
                                  newPositions = getNewPostions nextVariant $ coordinate block
                               in Block { shapeV     = nextVariant,
                                          coordinate = newPositions }
 
{-| it take the current positions and expected variant NO.
    return new positions of the block.
    find the header postion first, should take Boundary into consideration.
-}
shapeVList = [ (O,1), (I,2), (L,4), (J,4), (S,2), (Z,2), (T,4) ]

getNextTransformBlock :: Block -> Block
getNextTransformBlock    block
    = let (s, v)         = shapeV     block
          ps             = coordinate block
          ((_, n), m)   = filter ( == s . fst . fst) $ zip shapeVist [0..]     
          v'             = mod (v + 1) n
          ps'            = transform ps v' m
       in Block { shapeV = {s, v'}, coordinate = ps' }

coorPlus  (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

transform ps n m  = let (x1, y1) = head ps
                        (xh, yh) = (headerP !! m) !! n
                        x1' = | x1 + xh <  0        = 0
                              | x1 + xh >= maxColum = maxColum -1
                    in map $ coorPluse (x1', yh) $ (relativeP !! m) !! n


-- | real check function that return Bool

-- move down
-- first check whther reach bottom
isReachBottom :: Block -> Bool
isReachBottom block = let yps = map yp $ coordinate block
                       in or . map ( == (maxRows -1)) $ yps

canMoveDown :: Block -> Field -> (Block, Bool)
canMoveDown block field = let newP  = map (coorPlus (0,1) ) $ coordinate block
                              newBlock = block {coordinate = newP}
                              markP = map fst . filter ( == True . snd) $ markField field
                           in (newBlock, not . or . map (\p -> any (elem p) markP ) $ newP)

-- transform
canTransform :: Block -> Field -> (Block, Bool)
canTransform block field = let newBlock =  getNextTransformBlock block
                               newp  = coordinat newBlock
                               markP = map fst . filter ( == True . snd) $ markField field
                            in (newBlock, not . or . map (\p -> any (elem p) markP ) $ newP)


-- update field 
-- we first use List, may change to Data.Vector in future.
data Field = Field {
         fieldArear  :: (Int, Int)  -- the battle field of TETRIS' coordiante
         markField   :: [(Position, Bool)] -- 24 x 20
         }





meltBlocks :: Occupy -> Occupy
meltBlocks occpy = undefined

isGameOver :: Occupy -> Bool
isGameOver occupy = undefined

-- how to determine one block is settled?

