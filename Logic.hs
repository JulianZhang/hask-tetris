{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns #-}

module Logic where --(initFieldData, drawMainArea, drawPreviewArea, keyboardReact)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import System.Random
import Data.List
import Data.IORef
import Data.Word(Word32)
import Data.Time.Clock
import Debug.Trace

import Structure
import Render

debug = flip trace
--debugShow = flip traceShow
{-
I've often also found the variation debug = (flip trace) False useful. If I have a guards-based function definition, I simply insert a (or multiple) | debug ("Interesting value is " ++ show whatever) = undefined as the first guard(s).
 -}

-- | this is the interface
initFieldData :: LayoutInfo -> IO (LayoutInfo, IORef Field)
initFieldData layoutInfo = do
          backupBlock  <- getNewBackupBlock layoutInfo
          currentBlock <- getNewBackupBlock layoutInfo
          refField <- newIORef $ Field {bGameOver = False, currentBlock = currentBlock, 
                                        backupBlock = backupBlock,  markField = []     }
          return (layoutInfo, refField)

-- | resetAll the tetris
resetAll :: LayoutInfo -> IORef Field -> IO ()
resetAll layoutInfo refField = do
         backupBlock  <- getNewBackupBlock layoutInfo
         currentBlock <- getNewBackupBlock layoutInfo
         let field = Field {bGameOver = False, currentBlock = currentBlock, 
                                        backupBlock = backupBlock,  markField = [] }
         writeIORef refField field


-- | some constants
maxRows    = 24 :: Int
maxColumns = 18 :: Int

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

relativePS = [ [(0,0), (1,0), (1,-1), (2,-1)], [(0,0), (0,1), (1, 1), (1,2)] ]
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
initPosition = [ [(9,0),(10,0),(9,1),(10,1)],  [(8,0),(9,0),(10,0),(11,0)], 
                  -- L                         -- J
                 [(11,0),(11,1),(10,1),(9,1)], [(9,0),(9,0),(10,0),(11,0)],   
                  -- S                         -- Z
                 [(9,1),(10,1),(10,0),(11,0)], [(9,0),(10,0),(10,1),(11,1)], 
                  -- T
                 [(9,0),(10,0),(11,0),(10,1)]  ]


-- after settled, the color turns to Grey. (draw field area can do this)
-- 10 colors: Crimson, Indigo, Yellow, Orange, red, HotPink, Blue, DeepSkyBlue, DarkGreen
colorList = [(0.86, 0.08, 0.24,0.8), (0.3, 0.0,  0.51,0.8), (1.0,  1.0,  0.0, 0.8), (1.0,  0.65, 0,   0.8),
             (1.0,  0.0,  0.0, 0.8), (1.0, 0.41, 0.71,0.8), (0.0,  0.0,  1.0, 0.8), (0.86, 0.08, 0.24,0.8),
             (0.0,  0.75, 1.0, 0.8), (0.0, 0.40, 0.0, 0.8) ] -- all kinds of colors!!

down  = Position { xp = 0,    yp = 1   } 
left  = Position { xp =(-1),  yp = 0   } 
right = Position { xp = 1,    yp = 0   } 
up    = Position { xp =(-10), yp =(-10)} 


{-| it take the current positions and expected variant NO.
    return new positions of the block.
    find the header postion first, should take Boundary into consideration.
-}


-- | the function to move down, left, right or do transform.
moveAround :: Position -> Block -> Field -> (Block, Bool)
moveAround around block field 
           | around == up   = canTransform block field
           | around == down = let (block', canMove) = checkMove down block field
                               in (block', canMove && (not . isReachBottom $ block'))
           | otherwise      = checkMove around block field
           where 
           isReachBottom :: Block -> Bool
           isReachBottom block = let yps = map yp $ coordinate block
                                  in or . map ( == (maxRows -1)) $ yps
           checkMove around block field =
               let newP     = map (around +) $ coordinate block
                   newBlock = block {coordinate = newP}
                   markP    = markField field
                in (newBlock, not . or . map (\p -> elem p markP ) $ newP)

              
-- | transform is special case of moveAround up
canTransform :: Block -> Field -> (Block, Bool)
canTransform block field = let newBlock = getNextTransformBlock block
                               newP     = coordinate newBlock
                               markP    = markField  field
                            in (newBlock, not . or . map (\p -> elem p markP ) $ newP)
        where
        getNextTransformBlock :: Block -> Block
        getNextTransformBlock    block
            = let ShapeV (s, v)  = shapeV     block
                  ps             = coordinate block
                  ((_, n), m)    = head . filter ( (== s) . fst . fst) $ zip shapeVList [0..]     
                  v'             = mod (v + 1) n
                  ps'            = transform ps v' m
               in block {shapeV = ShapeV (s, v'), coordinate = ps' }

        transform ps n m  = let Position {xp=x1, yp=y1} = head ps
                                (xh, yh) = (headerP !! m) !! n
                                x1'  | x1 + xh <  0          = 0
                                     | x1 + xh >= maxColumns = maxColumns -1
                                     | otherwise             = x1 + xh
                             in map ( toPosition . coorPlus (x1', yh)) $ (relativeP !! m) !! n
                          
        coorPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
        toPosition (x, y) = Position {xp = x, yp =y}

-- | update field 

-- when cannot move down and isReachBottom, we update this.
addToField :: Block -> Field -> Field
addToField block field = let ps = coordinate block
                          in field { markField = union (markField field) ps }

-- we just check one row after another and update the field accordingly
-- call this after addToField

meltBlocks :: Field -> Field
meltBlocks field = let markP = markField field
                       sortY =  sort . flip filter rows $ 
                                \ y -> (yp . sum . filter ( (== y) . yp) $ markP) == columnSum
                    -- less than y's rwo, move down first and update field
                    in case length sortY == 0 of
                             True  -> field
                             False -> field { markField =  foldl step markP sortY }

                    where step ps y = let ps' = filter ( (/= y) . yp ) ps
                                       in map (ltYPlusOne y) ps'
                          ltYPlusOne y p = case yp p < y of
                                                True  -> p { yp = yp p + 1}
                                                False -> p

                          rows      = [0 .. (maxRows-1)]              :: [Int]
                          columnSum = foldl (+) 0 [0..(maxColumns-1)] ::  Int

-- when yp = 0, this is called after meltBlocks
isGameOver :: Field -> Bool
isGameOver filed = (length . filter ( (== 0) . yp ) $ markField filed) > 0

getNewBackupBlock :: LayoutInfo -> IO Block
getNewBackupBlock layoutInfo = do
         time' <- getCurrentTime
         let timeSeed     = truncate . (* 1000000) . diffTime time' $ initTime layoutInfo :: Int
             stdGen       = mkStdGen timeSeed
             (si, newGen) = randomR (0,6) stdGen -- shapeIndex
             (ci, _)      = randomR (0,9) newGen -- colorIndex
             positions    = positionsFromList (initPosition !! si)
         return $ Block { shapeV  = ShapeV (shapeVList !! si), 
                          color   = colorList  !! ci,
                          coordinate = positions     }
         where positionsFromList []     = []
               positionsFromList (x:xs) = Position {xp = fst x, yp = snd x} : positionsFromList xs
               diffTime :: UTCTime -> UTCTime -> Double
               diffTime = (realToFrac .) . diffUTCTime


                      
-- | updateStatus : key function , also update the 'level & score' in future
updateStatus :: LayoutInfo -> Field ->  Word32 -> IO (Maybe Field)
updateStatus layoutInfo field val = do
       let  maybeDir = keyToDirection val
       --print $ show maybeDir 
       case maybeDir of
            Nothing  -> return Nothing
            Just dir -> do 
                 let (block', canMove) = moveAround dir (currentBlock field) field
                 print $ "before " ++ show (currentBlock field)
                 print $ show canMove ++ " and after " ++ show dir ++ show block'
                 case canMove of
                   False -> case dir == down of
                              False -> return Nothing -- not downward cannot move
                              -- downward cannot move, so we add this to markField
                              True  -> do
                                    field' <- blockTransact layoutInfo field
                                    let field'' = meltBlocks field'
                                    case isGameOver field'' of
                                         True  -> return $ Just field'' {bGameOver = True}
                                         False -> return $ Just field''
                                                   
                   -- can move or transform     
                   True  -> return $ Just $ field { currentBlock = block' }

         where blockTransact layoutInfo field = do
                    let field' = addToField (currentBlock field) field
                    backupBlock' <- getNewBackupBlock layoutInfo
                    return $ field' {currentBlock = (backupBlock field), backupBlock = backupBlock'}

               keyToDirection :: Word32 -> Maybe Position
               keyToDirection val = case val of
                                         0xFF51  -> Just left  -- `debug` "left"
                                         0xFF52  -> Just up    -- `debug` "up"
                                         0xFF53  -> Just right -- `debug` "right"
                                         0xFF54  -> Just down  -- `debug` "down"
                                         ((-1) :: Word32) -> Just down -- `debug` "down"
                                         _       -> Nothing -- `debug` "not support key"


-- | the draw functions, periodically draw the image or react to keypress
drawMainArea layoutInfo refField val = do 
    -- first update field status    
    -- print $ "draw main" ++ show val
    field      <- readIORef refField
    maybeField <- updateStatus layoutInfo field val
    case maybeField of
         Nothing     -> return ()
         Just field' -> do
                        -- print field'
                        writeIORef refField field'
                        print $ markField field
                        realMainRender layoutInfo field'
                        return ()
    return True
    where realMainRender layoutInfo field = do
              case bGameOver field of
                   True  -> return () --renderGameOver field
                   False -> do
                            dr <- widgetGetDrawWindow $ drawingArea layoutInfo
                            (w, h) <- widgetGetSize (aFrame layoutInfo) 
                            --print $ show w  ++ " " ++ show h
                            tetrisMainRender field dr (fromIntegral w) (fromIntegral h)
                            return ()

drawPreviewArea layoutInfo refField = do 
                field  <- readIORef refField                   
                dr     <- widgetGetDrawWindow $ previewArea layoutInfo
                (w, h) <- drawWindowGetOrigin dr
                --print $ show w  ++ " " ++ show h
                -- render preview area using backupBlock
                tetrisPreviewRender field dr (fromIntegral w) (fromIntegral h)
                return True