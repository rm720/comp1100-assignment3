{-# Language OverloadedStrings #-}
{-|
Module      : Dragons.GUI
Description : GUI interface for the Othello game
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3

This is currently a bit of a mess and of a lower quality than the rest of
the code, but it handles the drawing of the GUI using blank-canvas, which
is a very un-Haskell-like library.

-}
module Dragons.GUI (GUIAction (..), runGUI, drawGame) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Game
import GameState
import Graphics.Blank hiding (fillStyle)
import Graphics.Blank.Style (fillStyle)

-- | The different kinds of GUI actions.
data GUIAction = DrawTable
               | DrawBoard
               | DrawPiece Player Int Int
               | DrawScores Int Int
               | DrawTurn (Maybe Player)
               | Save
               | Restore
               | BadMove Int Int
               | Fail String
  deriving (Eq, Show)

-- | Run the GUI. One instance of the program for each open window. Runs
-- a function which reads from a channel of input clicks on the board
-- and writes to a channel of GUI actions.
runGUI :: (TChan (Int,Int) -> TChan GUIAction -> IO ()) ->  IO ()
runGUI run = blankCanvas (3000 {events = ["mousedown"]}) $ \context -> do
  send context (eval "var canvasStack = []")
  actions <- newTChanIO
  clicks <- newTChanIO
  void $ forkIO $ forever (atomically (readTChan (eventQueue context) >>=
    maybe (return ()) (writeTChan clicks) . toClickLocation context))
  void $ forkIO $ drawLoop context actions
  run clicks actions
  where
    -- Convert a click on the screen to a location on the board
    toClickLocation context Event{ePageXY = Just(x,y)}
      | 0 <= finalX && finalX <= 7 && 0 <= finalY && finalY <= 7
      = Just (finalX, finalY)
      where
        -- Just do the drawing translations backwards, divide by size of
        -- the board, multiply by 8 and floor
        (h,w,c) = (height context, width context, min h w)
        (xOffset, yOffset) = ((w-c)/2+c*0.05,(h-c)/2+c*0.05)
        clamp a = floor $ a/0.9/c*8
        (finalX, finalY) = (clamp (x-xOffset), clamp (y-yOffset))

    toClickLocation _ _ = Nothing


-- | The main drawing loop. We cache the woodTexture and feltTexture
-- because they're computationally intensive to make
drawLoop :: DeviceContext -> TChan GUIAction -> IO ()
drawLoop context eventChan = do
  woodTexture <- send context wood
  feltTexture <- send context felt
  forever $ do
    event <- atomically $ readTChan eventChan
    handleGUIAction context event woodTexture feltTexture


-- | Generate the actions to draw a game. In most use cases this will be called
-- exactly once to draw the board.
drawGame :: Game -> [GUIAction]
drawGame (Game t board) =
  [ DrawTable, DrawBoard
   , DrawScores (currentScore Dark board) (currentScore Light board)
   , DrawTurn t ] ++
  [DrawPiece p x y | (row,y) <- zip board [0..]
                   , (Just p, x) <- zip row [0..]]

-- Handle a given GUI action.
handleGUIAction :: DeviceContext -> GUIAction -> CanvasPattern
                -> CanvasPattern -> IO ()
handleGUIAction context action woodTexture feltTexture = send context $
  withDefaultTransform $ case action of
    Save -> eval $ pack $ "canvasStack.push("
                            ++ canvasJS ++ ".getImageData(0,0,"
                            ++ show w ++ "," ++ show h ++ "))"
    Restore -> eval $ pack
                    $ canvasJS ++ ".putImageData(canvasStack.pop(),0,0)"
    Fail s -> eval $ pack $ "alert(\"A fatal error has occurred:\\n\" + "
                            ++ show s ++ ")"
    DrawTable -> do
      rect (0,0,width context,height context)
      fillStyle woodTexture
      fill ()
    DrawBoard -> do
      moveToBoard
      -- Draw the bounding box of the board.
      rect (0,0,c,c)
      fillStyle feltTexture
      fill ()
      thickBlackStroke
      -- Draw the rest of the grid using 14 squares
      let drawBox b = do
            beginPath ()
            rect (0,0,b,b)
            thickBlackStroke
            beginPath ()
            rect (c, c, -b, -b)
            thickBlackStroke
      mapM_ drawBox steps
    DrawPiece p col row -> do
      let startColour = case p of
                    Dark -> "gray"
                    Light -> "#dddddd"
          stopColour = case p of
                    Dark -> "black"
                    Light -> "gray"
          x = fromIntegral col * cellSize + (cellSize/2)
          y = fromIntegral row * cellSize + (cellSize/2)
      moveToBoard
      arc (x,y,r,0,2*pi, False)
      grad <- createRadialGradient (x,y,0,x,y,r)
      addColorStop(0,startColour) grad
      addColorStop(1,stopColour) grad
      fillStyle grad
      fill ()
      thickBlackStroke
    BadMove col row -> do
      let x = fromIntegral col * cellSize + (cellSize/2)
          y = fromIntegral row * cellSize + (cellSize/2)
      moveToBoard
      grad <- createRadialGradient (x,y,0,x,y,r)
      arc (x,y,r,0,2*pi, False)
      addColorStop(0,"orange") grad
      addColorStop(1,"red") grad
      fillStyle grad
      fill()
    DrawScores darkScore lightScore -> do
      let darkText = pack $ "Dark: " ++ show darkScore
          lightText = pack $ "Light: " ++ show lightScore
      -- Cover the old score
      coverWithWood (w - (w-h)/2) 0 ((w-h)/2) (height context) woodTexture
      -- Write the score
      beginPath ()
      fillStyle ("black" :: Text)
      translate(w-(w-h)/4,h/4)
      (TextMetrics dWidth) <- measureText darkText
      (TextMetrics lWidth) <- measureText lightText
      let tWidth = max lWidth dWidth
      scale (((w-h)/3)/tWidth,((w-h)/3)/tWidth)
      textAlign center
      fillText (darkText,0,0)
      fillText (lightText,0,20)
    DrawTurn s -> do
      let turnText = case s of
            Just Dark -> "Turn: Dark"
            Just Light -> "Turn: Light"
            Nothing -> "Game Over"
      -- Cover the old turn
      coverWithWood 0 0 ((w-h)/2) (height context) woodTexture
      -- Write the turn state
      beginPath ()
      fillStyle ("black" :: Text)
      translate((w-h)/4,h/4)
      (TextMetrics tWidth) <- measureText turnText
      scale (((w-h)/3)/tWidth,((w-h)/3)/tWidth)
      textAlign center
      fillText (turnText,0,0)
  where
    h = height context
    w = width context
    c = min h w
    steps = map (*(c/8)) [1..7]
    r = 0.4 * cellSize
    cellSize = c / 8
    withDefaultTransform f = do
      setTransform (1,0,0,1,0,0)
      beginPath ()
      res <- f
      setTransform (1,0,0,1,0,0)
      return res
    moveToBoard = do
      translate((w-c)/2+c*0.05,(h-c)/2+c*0.05)
      scale (0.9,0.9)
    thickBlackStroke = do
      strokeStyle "black"
      lineWidth 3
      stroke ()
    canvasJS = unpack $ toStrict $ toLazyText
                      $ showbJS $ deviceCanvasContext context

-- Cover a section of the screen with wood (used for refreshing the score
-- and turn indicators
coverWithWood :: Double -> Double -> Double -> Double
              -> CanvasPattern -> Canvas ()
coverWithWood x y w h woodTexture = do
  beginPath ()
  rect (x,y,w,h)
  fillStyle woodTexture
  fill ()

-- Create a pattern from a simple linear gradient from the size of the
-- "texture" and the gradient stops.
linearGradientPattern :: Int -> [(Double, Text)] -> Canvas CanvasPattern
linearGradientPattern inSize stops = do
   ctx <- newCanvas (size,size)
   with ctx $ do
       grad <- createLinearGradient(0,0,0,size)
       mapM_ (`addColorStop` grad) stops
       rect (0,0,size,size)
       fillStyle grad
       fill ()
       image <- toDataURL () >>= newImage
       createPattern (image, Repeat)
   where
     size :: Num a => a
     size = fromIntegral inSize

-- Taken from https://codepen.io/darrentorpey/pen/oGnLj
wood :: Canvas CanvasPattern
wood = linearGradientPattern 120
  [ (0   , "rgb(233,223,196)")
  , (0.1 , "rgb(233,223,196)")
  , (0.2 , "rgb(237,227,200)")
  , (0.24, "rgb(237,227,200)")
  , (0.25, "rgb(235,221,195)")
  , (0.48, "rgb(233,223,196)")
  , (0.49, "rgb(235,221,195)")
  , (0.52, "rgb(230,216,189)")
  , (0.53, "rgb(230,216,189)")
  , (0.54, "rgb(233,219,192)")
  , (0.55, "rgb(230,216,189)")
  , (0.56, "rgb(230,216,189)")
  , (0.57, "rgb(233,219,192)")
  , (0.58, "rgb(230,216,189)")
  , (0.73, "rgb(230,216,189)")
  , (0.74, "rgb(233,219,192)")
  , (0.98, "rgb(233,219,192)")
  , (1   , "rgb(235,221,195)")
  ]


-- It doesn't really look like felt, but that was what I was aiming for
felt :: Canvas CanvasPattern
felt = linearGradientPattern 317 -- It seemed like a good number
  [ (0  , "#009550")
  , (0.5, "#0a6c03")
  , (1  , "#009550")
  ]
