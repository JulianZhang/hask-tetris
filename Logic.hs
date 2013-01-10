module Logic where

import Data.List
import Structure

-- when do transform of block, the according transform of its 4 units' transform
-- relative ones, the first one is the base case.

maxRows   = 24 :: Int
maxColumn = 18 :: Int

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
canMoveDown block field = let newP     = map (coorPlus (0,1) ) $ coordinate block
                              newBlock = block {coordinate = newP}
                              markP    = markField field
                           in (newBlock, not . or . map (\p -> any (elem p) markP ) $ newP)

-- transform
canTransform :: Block -> Field -> (Block, Bool)
canTransform block field = let newBlock =  getNextTransformBlock block
                               newp     = coordinat newBlock
                               markP    = markField field
                            in (newBlock, not . or . map (\p -> any (elem p) markP ) $ newP)


-- | update field 

-- when cannot move down or isReachBottom, we update this.
addToField :: Block -> Field -> Field
addToField block field = let ps = coordinate block
                          in filed { markField = union (markField field) ps }

-- we just check one row after another and update the field accordingly
-- call this after addToField
rows      = [0..(maxRows-1)] :: [Int]
columnSum = foldl (+) 0 [0..(maxColumn-1)] :: Int


meltBlocks :: Field -> Field
meltBlocks field = let markP = markField field
                       sortY =  sort . flip filter rows $ 
                                \ y -> (sum . filter (== y . yp) $ markP) == columnSum
                    -- less than y's rwo, move down first and update field
                    in case length sortY == 0 of
                             True  -> field
                             False -> field { markField =  foldl step markP sortY }

                    where step ps y = let ps' = filter ( /= y . yp ) ps
                                       in map (ltYPlusOne y) ps'
                          ltYPlusOne y p = case yp p < y of
                                                True  -> p { yp = yp p + 1}
                                                Flase -> p

-- we first use List, may change to Data.Vector in future.
data Field = Field {
         fieldArea   :: (Int, Int) -- 24 x 18 the battle field of TETRIS' coordiante
         markField   :: [Position] -- 
         }






isGameOver :: Occupy -> Bool
isGameOver occupy = undefined

-- how to determine one block is settled?

