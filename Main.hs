module Main where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

--------- Type aliases --------------------------------------

-- | type aliases for context variables
type Radius = Float
type Position = (Float, Float)

--------- Constant functions aka. constant Settings ---------

-- | width of screen
width :: Int 
width = 300

-- | background color
background :: Color
background = black 

-- | Number of frames to show per second.
fps :: Int
fps = 60

--------- Game States ---------------------------------------

-- | data structure for describing the game state
data PongGame = 
    Game
    { 
    paused :: Bool, -- ^ state of a running or paused game
    keyHold :: Bool, -- ^ state of folding a key down
    ballLocation :: (Float, Float), -- ^ location of the ball
    ballVelocity :: (Float, Float), -- ^ velocity of the ball
    playerOne :: Float,  -- ^ height of playerOnes Paddle
    playerTwo :: Float   -- ^ height of playerTwos Paddle
    } deriving Show 

-- | starting state for a game of pong.
initialState :: PongGame
initialState = Game{paused = False,
                    keyHold = False,
                    ballLocation = (-80,  -40),
                    ballVelocity = (-20, -5),
                    playerOne = 40,
                    playerTwo = -80
                    }


----------- main function ------------------------------------

main :: IO()
main = play makeWindow background fps initialState render handleKeys update
    where
        frame :: Float -> Picture 
        frame seconds = render $ moveBall seconds initialState


----------- window and game board creation -------------------

makeWindow :: Display
makeWindow = InWindow "Window" (width,300) (100,100)

----------- input handling -----------------------------------

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 's') _ _ _) game = game {ballLocation = (0, 0)} -- if s is pressed reset ball to center
handleKeys (EventKey (Char 'p') _ _ _) game = game {paused = not $ paused game}
handleKeys (EventKey (Char 'w') _ _ _) game = moveLeftPaddle 10 game
handleKeys (EventKey (Char 'a') _ _ _) game = moveLeftPaddle (-10) game
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = moveRightPaddle 10 game
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = moveRightPaddle (-10) game
handleKeys _ game = game -- game state does not change on any other input

-- paddle control

----------- rendering of game states -------------------------

-- | converting a game state into a picture
render :: PongGame -> Picture 
render game = pictures [ball, 
                        walls, 
                        mkPaddle rose 120 $ playerOne game,
                        mkPaddle orange (-120) $ playerTwo game] 
              where
                ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid 10 
                ballColor = dark red

                  -- the bottom and top walls
                wall :: Float -> Picture 
                wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
                wallColor = greyN 0.5
                walls = pictures [wall 150, wall (-150)]

                mkPaddle :: Color -> Float -> Float -> Picture 
                mkPaddle col x y = pictures[translate x y $ color col $ rectangleSolid 26 86, translate x y $ color paddleColor $ rectangleSolid 20 80]
                paddleColor = light (light blue)

------------ updating of game state ---------------------------

-- | Update the game by moving the ball.
update :: Float -> PongGame -> PongGame
update seconds game 
                    | paused game = game
                    | otherwise = if fst (ballLocation game) > 130
                                    then error "player one wins"
                                    else if fst (ballLocation game) < -130
                                        then error "player two wins"
                                        else (paddleBounce . wallBounce . moveBall seconds) game 

                        
------------ changes in game state through collision ----------

-- | Detect a collision with a paddle and change velocity after collision
paddleBounce :: PongGame -> PongGame
paddleBounce game =  game { ballVelocity = (vx', vy')}
    where 
        -- Radius
        radius = 10
        -- The old velocities
        (vx, vy) = ballVelocity game 

        (vx', vy') = if paddleCollision (ballLocation game) (playerOne game) (playerTwo game) radius
                     then
                        -- Update the velocity.
                        (-vx, vy)
                     else
                        -- Do nothing. Return the old velocity.
                        (vx, vy)

-- | Detect a collision with a wall and change velocity after collision
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVelocity = (vx, vy')}
    where
        -- Radius
        radius = 10
        -- The old velocities
        (vx, vy) = ballVelocity game 

        vy' = if wallCollision (ballLocation game) radius
              then
                  -- Update the velocity.
                  -vy
              else
                  -- Do nothing. Return the old velocity.
                  vy

------------ collision detection -----------------------------

-- | Given position and radius of the ball, return whether a collision occured.
paddleCollision :: Position -> Float -> Float -> Radius -> Bool
paddleCollision (x, y) poy pty radius = leftPaddle || rightPaddle
    where
        -- position of the paddle
        rightPaddle = (y <= poy + 43 && y >= poy - 43) && (x + radius >= 120)
        leftPaddle =  (y <= pty + 43 && y >= pty - 43) && (x - radius <= -120) 

-- | Given position and radius of the ball, return whether a collision occured.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision = y - radius <= -fromIntegral width/2
        bottomCollision = y + radius >= fromIntegral width/2

------------ movement ----------------------------------------

-- | Update the ball position using its current velocity
moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game {ballLocation = (x', y')}
  where
      -- Old locations and velocities.
      (x, y) = ballLocation game
      (vx, vy) = ballVelocity game

      -- New locations.
      x' = x + vx * seconds
      y' = y + vy * seconds

moveLeftPaddle :: Float -> PongGame -> PongGame
moveLeftPaddle offset game 
                        | playerTwo game + offset > 100 = game
                        | playerTwo game + offset < -100 = game
                        | otherwise = game {playerTwo = playerTwo game + offset}

moveRightPaddle :: Float -> PongGame -> PongGame
moveRightPaddle offset game 
                        | playerOne game + offset > 100 = game
                        | playerOne game + offset < -100 = game
                        | otherwise = game {playerOne = playerOne game + offset}

