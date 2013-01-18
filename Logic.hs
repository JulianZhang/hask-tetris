module Logic where (initFieldData, drawMainArea, drawPreviewArea, keyboardReact, getAndSet)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import System.Random
import Data.List
import Data.IORef

import Structure
import Render

-- | this is the interface
initFieldData :: DrawInfo -> IO (DrawInfo, IORef Field)
initFieldData drawInfo = do
          backupBlock  <- getNewBackupBlock drawInfo
          currentBlock <- getNewBackupBlock drawInfo
          refField <- newIORef $ Field {bGameOver = False, currentBlock = currentBlock, 
                                        backupBlock = backupBlock,  markField = []     }
          return (drawInfo, refField)

-- | resetAll the tetris
resetAll :: :: DrawInfo -> IORef Field -> IO ()
resetAll drawInfo refField = do
         backupBlock  <- getNewBackupBlock drawInfo
         currentBlock <- getNewBackupBlock drawInfo
         let field = Field {bGameOver = False, currentBlock = currentBlock, 
                                        backupBlock = backupBlock,  markField = [] }
         writeIORef refField


-- | some constants
maxRows   = 24 :: Int
maxColumn = 18 :: Int

-- | relative positio, header position
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


-- after settled, the color turns to Grey. (draw field area can do this)
-- 10 colors: Crimson, Indigo, Yellow, Orange, red, HotPink, Blue, DeepSkyBlue, DarkGreen
colorList = [(0.86, 0.08, 0.24,0.8), (0.3, 0.0,  0.51,0.8), (1.0,  1.0,  0.0, 0.8), (1.0,  0.65, 0,   0.8),
             (1.0,  0.0,  0.0, 0.8), (1.0, 0.41, 0.71,0.8), (0.0,  0.0,  1.0, 0.8), (0.86, 0.08, 0.24,0.8),
             (0.0,  0.75, 1.0, 0.8), (0.0, 0.40, 0.0, 0.8), ] -- all kinds of colors!!

down  = Position { xp = 0,  yp = 1) } 
left  = Position { xp =-1,  yp = 0) } 
right = Position { xp = 1,  yp = 0) } 
up    = Position { xp =-10, yp =-10) } 


{-| it take the current positions and expected variant NO.
    return new positions of the block.
    find the header postion first, should take Boundary into consideration.
-}


-- | the function to move down, left, right or do transform.
moveAround :: Position -> Block -> Field -> (Block, Bool)
moveAround around block field = 
         | around == up   = canTransform block field
         | around == down = let (block', canMove) = checkMove around block field
                             in (block', canMove && (not . isReachBottom $ block'))
         | otherwise = checkMove around block field
         where 
         isReachBottom :: Block -> Bool
         isReachBottom block = let yps = map yp $ coordinate block
                                in or . map ( == (maxRows -1)) $ yps
         checkMove around block field =
                       let newP = map (around +) $ coordinate block
                           newBlock = block {coordinate = newP}
                           markP    = markField field
                       in (newBlock, not . or . map (\p -> any (elem p) markP ) $ newP)

               

-- | transform is special case of moveAround up
canTransform :: Block -> Field -> (Block, Bool)
canTransform block field = let newBlock =  getNextTransformBlock block
                               newp     = coordinat newBlock
                               markP    = markField field
                            in (newBlock, not . or . map (\p -> any (elem p) markP ) $ newP)
        where
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
                          
        coorPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- | update field 

-- when cannot move down and isReachBottom, we update this.
addToField :: Block -> Field -> Field
addToField block field = let ps = coordinate block
                          in filed { markField = union (markField field) ps }

-- we just check one row after another and update the field accordingly
-- call this after addToField

meltBlocks :: Field -> Field
meltBlocks field = let markP = markField field
                       sortY =  sort . flip filter rows $ 
                                \ y -> (sum . filter (== y . yp) $ markP) == columnSum
                    -- less than y's rwo, move down first and update field
                    in case length sortY == 0 of
                             True  -> Field
                             False -> Just $ field { markField =  foldl step markP sortY }

                    where step ps y = let ps' = filter ( /= y . yp ) ps
                                       in map (ltYPlusOne y) ps'
                          ltYPlusOne y p = case yp p < y of
                                                True  -> p { yp = yp p + 1}
                                                Flase -> p

                          rows      = [0 .. (maxRows-1)]             :: [Int]
                          columnSum = foldl (+) 0 [0..(maxColumn-1)] ::  Int

-- when yp = 0, this is called after meltBlocks
isGameOver :: Field -> Bool
isGameOver filed = (length . filter ( (== 0) . yp ) $ markField filed) > 0

getNewBackupBlock :: DrawInfo -> IO Block
getNewBackupBlock drawInfo = do
         time' <- getCurrentTime
         let timeSeed     = truncat . (* 1000000) . diffTime $ time' $ initTime drawInfo :: Int
             stdGen       = mkStdGen timeSeed
             (si, newGen) = randomR (0,6) stdGen -- shapeIndex
             (ci, _)      = randomR (0,9) newGen -- colorIndex
             positions    = positionsFromList (initPositon !! si)
          return Block { shapeV  = shapeVList !! si, 
                         color   = colorList  !! ci,
                         coordinate = positions     }
         where positionsFromList []     = []
               positionsFromList (x:xs) = Position {xp = fst x, yp = snd x} : positionsFromList xs
               diffTime :: UTCTime -> UTCTime -> Double
               diffTime = (realToFrac .) . diffUTCTime



-- | redraw the area acoording to the key press
keyboardReact drawInfo refField = 
                      do tryEvent $ do --tryEvent :: EventM any () -> EventM any Bool
                      kv  <- eventKeyVal
                      -- must do main first, for it will refresh the field
                      drawMainArea    drawInfo refField kv
                      drawPreviewArea drawInfo refField
                      
-- | updateStatus : key function , also update the 'level & score' in future
updateStatus :: DrawInfo -> Field ->  Word32 -> IO (Maybe Field)
updateStatus drawInfo field val = do
       let  maybeDir = ketToDirection val 
       case maybeDir of
            Nothing  -> return Nothing
            Just dir -> do 
                 let (block', canMove) = moveAround dir (currentBlock field)
                 case canMove of
                   Flase -> case dir == down of
                              False -> return Nothing -- not downward cannot move
                              -- downward cannot move, so we add this to markField
                              True  -> do
                                    field' <- blockTransact drawInfo field
                                    let field'' = meltBlocks field'
                                    case isGameOver field'' of
                                         True  -> return $ Just field'' {bGameOver = True}
                                         False -> return $ Just field''
                                                   
                   -- can move or transform     
                   True  -> return $ field' = field { currentBlock = block' }

    where blockTransact drawInfo field = do
               let field' = addToField (currentBlock field) field
               backupBlock' <- getNewBackupBlock drawInfo
               return $ field' {currentBlock = (backupBlock field), backupBlock = backupBlock'}

          keyToDirection :: Word32 -> Maybe Position
          ketTODirection val = case val of
                          0xFF51         -> Just left
                          0xFF52         -> Just up
                          0xFF53         -> Just right
                          0xFF54         -> Just down 
                          (-1) :: Word32 -> Just down
                          _              -> Nothing 


-- | the draw functions, periodically draw the image or react to keypress
drawMainArea drawInfo refField val = do 
    -- first update field status    
    field      <- readIORef refField
    maybeField <- updateStatus drawInfo field val
    case maybeField of
         Nothing     -> return ()
         Just field' -> do
                        writeIORef refField field'
                        realMainRender drawInfo field'
                        return ()
    where realMainRender drawInfo field = do
              case bGameOver field of
                   True  -> renderGameOver field
                   False -> do
                            renderField field
                            (w,h) <- eventWindowSize
                            dw <- eventWindow
                            liftIO $ do tetrisMainRender dw field w h


drawPreviewArea drawInfo refField = do 
                field <- readIORef refField                   
                (w,h) <- eventWindowSize
                dw <- eventWindow
                -- reander preview area using backupBlock
                liftIO $ do tetrisPreviewRender dw field w h                