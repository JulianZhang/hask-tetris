module Logic where (initFieldData, drawMainArea, drawPreviewArea, keyboardReact, getAndSet)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import System.Random
import Data.List
import Data.IORef

import Structure

-- | this is the interface
initFieldData :: DrawInfo -> IO (DrawInfo, IORef Field)
initFieldData drawInfo = do
          let field = Field { currentBlock = blockEmpty, backupBlock = blockEmpty, markField = []}
              backupBlock  = getNewBackupBlock drawInfo field
              currentBlock = getNewBackupBlock drawInfo field
              field {currentBlock = currentBlock, backupBlock = backupBlock}
          refField <- newIORef field
          return (drawInfo, refField)

getAndSet :: a -> IO (IO a, a -> IO ())
getAndSet a = do
    ior <- newIORef a
    let get = readIORef ior
    let set = writeIORef ior
    return (get,set)

maxRows   = 24 :: Int
maxColumn = 18 :: Int

-- relative positio, header position
relativePO = [ [(0,0), (1,0), (0,1), (1,1)] ]
headerPO   = [ (0, 0) ]

relativePI = [ [(0,0), (1,0), (2,0),  (3,0) ], [(0,0), (0,-1), (0,-2), (0,-3)] ]
headerPI   = [ (-3,3), (3, -3) ]

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
headerP   = [ headerPO,   headerPI,   headerPL,    headerPJ,   headerPS,   headerPZ,    headerPT]

shapeVList   = [ (O,1), (I,2), (L,4), (J,4), (S,2), (Z,2), (T,4) ]
                  -- O                         -- I                            
initPosition = [ [(9,0),(10,0),(9,1),(10,1)],  [(8,0),(9,0),(10,0),(11,0),], 
                  -- L                         -- J
                 [(11,0),(11,1),(10,1),(9,1)], [(9,0),(9,0),(10,0),(11,0),]   
                  -- S                         -- Z
                 [(9,1),(10,1),(10,0),(11,0)], [(9,0),(10,0),(10,1),(11,1)], 
                  -- T
                 [(9,0),(10,0),(11,0),(10,1)]                 ]

colorList    = [ ] -- all kinds of colors!!

{-| it take the current positions and expected refiant NO.
    return new positions of the block.
    find the header postion first, should take Boundary into consideration.
-}

getNextTransformBlock :: Block -> Block
getNextTransformBlock    block
    = let (s, v)         = shapeV     block
          ps             = coordinate block
          ((_, n), m)    = filter ( == s . fst . fst) $ zip shapeVist [0..]     
          v'             = mod (v + 1) n
          ps'            = transform ps v' m
       in Block { shapeV = {s, v'}, coordinate = ps' }

transform ps n m  = let (x1, y1) = head ps
                        (xh, yh) = (headerP !! m) !! n
                        x1' = | x1 + xh <  0        = 0
                              | x1 + xh >= maxColum = maxColum -1
                    in map $ coorPlus (x1', yh) $ (relativeP !! m) !! n
                    where 
                    coorPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | real check function that return Bool

-- move down
-- first check whther reach bottom
down  = Position { xp = 0,  yp = 1) } 
left  = Position { xp =-1,  yp = 0) } 
right = Position { xp = 1,  yp = 0) } 
up    = Position { xp =-10, yp =-10) } 

isReachBottom :: Block -> Bool
isReachBottom block = let yps = map yp $ coordinate block
                       in or . map ( == (maxRows -1)) $ yps

moveAround :: Position -> Block -> Field -> (Block, Bool)
moveAround around block field = 
              let newP = map (around +) $ coordinate block
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
rows      = [0 .. (maxRows-1)]             :: [Int]
columnSum = foldl (+) 0 [0..(maxColumn-1)] ::  Int

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

-- when yp = 0, this is called after meltBlocks
blockEmpty = Block { shapeV = head shapeVList, color = head colorList, markField = [] }

isGameOver :: Field -> Bool
isGameOver filed = (length . filter ( (== 0) . yp ) $ markField filed) > 0

getNewBackupBlock :: (DrawInfo, Field) -> IO (DrawInfo, Field)
getNewBackupBlock drawInfo field = do
         time' <- getCurrentTime
         let timeSeed = truncat . (* 1000000) . diffTime $ time' $ initTime drawInfo :: Int
             stdGen   = mkStdGen timeSeed
             (si, newGen) = randomR (0,6) stdGen -- shapeIndex
             (ci, _)      = randomR (0,?) newGen -- colorIndex
             positions    = positionsFromList (initPositon !! si)
         return $(drawInfo, Field { backupBlock = Block { shapeV  = shapeVList !! si, 
                                                          color   = colorList  !! ci,
                                                          coordinate = positions    }  }
         where positionsFromList []     = []
               positionsFromList (x:xs) = Position {xp = fst x, yp = snd x} : positionsFromList xs
               diffTime :: UTCTime -> UTCTime -> Double
               diffTime = (realToFrac .) . diffUTCTime


drawMainArea drawInfo field val = do 
    -- first update field status    
    field <- readIORef refField                   
    field <- updateStatus refField val
    
    (w,h) <- eventWindowSize
    dw <- eventWindow
    liftIO $ do
      jam <- getJam
      cars <- getCars
      renderWithDrawable dw $ do
                            translate (w/2) (h/2)
                            scale (w/drawSide) (h/drawSide)
                            road2render jam cars
      return True

drawPreviewArea drawInfo refField = do 
                field <- readIORef refField                   
                (w,h) <- eventWindowSize
                dw <- eventWindow
                -- reander preview area using backupBlock


--tryEvent :: EventM any () -> EventM any Bool
--Execute an event handler and assume it handled the event unless it threw a pattern match exception. 

keyboardReact drawInfo refField = 
                      do tryEvent $ do
                      kv  <- eventKeyVal
                      liftIO $ drawMainArea    drawInfo refField kv
                      liftIO $ drawPreviewArea drawInfo refField
                      
-- updateStatus : key function , also update the 'level & score'
-- val
updateStatus :: Word32 -> IORef Field -> IORef Field
updateStatus val refField = 





-- resetAll 



{-
define XK_Home			0xFF50
define XK_Left			0xFF51	/* Move left, left arrow */
define XK_Up			0xFF52	/* Move up, up arrow */
define XK_Right		0xFF53	/* Move right, right arrow */
define XK_Down			0xFF54	/* Move down, down arrow */
define XK_Prior		0xFF55	/* Prior, previous */
define XK_Page_Up		0xFF55
define XK_Next			0xFF56	/* Next */
define XK_Page_Down		0xFF56
define XK_End			0xFF57	/* EOL */
define XK_Begin		0xFF58	/* BOL */
-}
