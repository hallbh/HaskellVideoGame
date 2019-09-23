module Game where

import Entity

import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game
getDecimal :: RealFrac a => a -> a
getDecimal = snd . properFraction

-- | Data describing the state of the game
data GameState = Game
  { player :: GameObject -- ^ The player
  , enemies :: [GameObject] -- ^ A list of the enemies
  , bullets :: [Bullet] -- ^ A list of bullets on screen
  , timeElapsed :: Float -- ^ The total elapsed time of the game
  , textShown :: Maybe String -- ^ Text displayed on screen
  , textScale :: Float
  , isPaused :: Bool
  , playerAlive :: Bool
  , gameStep :: GameState -> GameState
  , currentLevel :: GameState -> GameState
  }

initialState :: GameState
initialState = Game
  { player = initialPlayer
  , enemies = []
  , bullets = []
  , timeElapsed = 0
  , textShown = Nothing
  , textScale = 0
  , isPaused = False
  , playerAlive = True
  , gameStep = tutorial_0
  , currentLevel = tutorial_0
  }
----------------------------------------------------------------------------------------
-- | collision order matters -- collide_enemies will remove enemies and collide_bullets will remove bullets
update :: Float -> GameState -> GameState
update seconds = collide_bullets . collide_enemies . collide_player . run_game seconds
----------------------------------------------------------------------------------------
-- | GameState modifying functions
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
  where
    x' = x2 - x1
    y' = y2 - y1
-- | function to check if any of our hero's bullets have hit an enemy 
collide_enemies :: GameState -> GameState
collide_enemies game = if playerAlive game then game { enemies = enemies'' } else game
  where
     -- If an enemy is too close to a bullet, apply damage (same order as bullets')
    enemies' = 
      foldr 
        (\enemy -> (\r ->
          [enemy { curHealth = (curHealth enemy) 
            - (sum 
                [damage b | b <- (filter (\bullet -> (distance (position enemy) (bullet_pos bullet)) < (hit_radius enemy) + (bullet_hit_radius bullet))
                  (bullets game))]) }] ++ r )) [] (enemies game)
    enemies'' =
      foldr 
        (\enemy -> (\r ->
          if (distance (position enemy) (position (player game))) < (hit_radius enemy) + (hit_radius (player game))
          then r else [enemy] ++ r )) [] enemies'
    -- If an enemy hits a player, remove it
-- | function to check if any bullets need to be cleaned up after hitting an enemy
collide_bullets :: GameState -> GameState
collide_bullets game = game { bullets = bullets' }
  where
    -- If a bullet hit an enemy, remove it (Order n*m where n is num bullets and m is num enemies)
    bullets' = 
      foldr (\bullet -> (\r ->
              if any 
                (\enemy -> (distance (position enemy) (bullet_pos bullet)) < (hit_radius enemy) + (bullet_hit_radius bullet)) 
                (enemies game)
              then r else [bullet] ++ r)) [] (bullets game)
-- | function for colliding the hero
collide_player :: GameState -> GameState
collide_player game = if playerAlive game then game { player = player' } else game
  where -- | if there is an enemy that hits our player, lose health and move to origin
    prev = curHealth (player game)
    hitEnemy = length (filter (\enemy -> 
                                (distance (position enemy) (position (player game)) < 
                                ((hit_radius enemy) + (hit_radius (player game)))))
                              (enemies game)) > 0
    curHealth' = if hitEnemy then prev - 1 else prev
    position' = if hitEnemy then (0, 0) else position (player game)
    player' = (player game) { curHealth = curHealth', position = position' } 

----------------------------------------------------------------------------------------
-- Game running functions
----------------------------------------------------------------------------------------
-- | Basic "run the game" function
run_game :: Float -> GameState -> GameState -- This ghastly statement is because movestep might change gamestep. Everything inside is linear time
run_game seconds game = (gameStep (move_step seconds game)) (move_step seconds game)
---------------------------------------------------------------------------------------
-- | Basic gameplay move everything function
move_step :: Float -> GameState -> GameState
move_step seconds game =
  if (isPaused game) then game else
  game { player = player'
       , enemies = enemies'
       , bullets = bullets'
       , timeElapsed = timeElapsed'
       , playerAlive = playerAlive'
       , gameStep = gameStep'
       }
    where  
      [player'] = if (playerAlive game) then move_player seconds (player game) else [(player game)]
      enemies' = foldr (\e -> (\r -> (step e) e ++ r)) [] (foldr (\e -> (\r -> move_enemy seconds e ++ r)) [] (enemies game))
      bullets' = foldr (\b -> (\r -> (bullet_step b) b ++ r)) [] 
                  (foldr (\b -> (\r -> move_bullets seconds b ++ r)) [] 
                    (bullets game))
      timeElapsed' = timeElapsed game + seconds
      playerAlive' = if (curHealth (player game)) <= 0 then False else True
      gameStep' = if playerAlive' then (gameStep game) else game_over (Just "Press r to retry")

spawn_enemies :: [SpawnInfo] -> (GameState -> GameState) -> (GameState -> GameState)
spawn_enemies si func
  | si == [] = \game -> game { gameStep = func }
  | True = \game ->
    game { enemies = (enemies game) ++ if st == Zoomer 
                                       then [generateZoomer loc f]
                                       else if st == Seeker
                                       then [generateSeeker loc f]
                                       else if st == Carrier
                                       then [generateCarrier loc f] 
                                       else []
         , gameStep = spawn_enemies (tail si) func }
      where 
        (SI loc f st) = head si
-- | Runs the game as normal for a given amount of time before the next function
-- Shamelessly stolen from instructor's example
run_for :: Float -> (GameState -> GameState) -> (GameState -> GameState)
run_for ticks f2 = \g ->
  if ticks == 0 then
    g { gameStep = f2 }
  else
    g { gameStep = run_for (ticks - 1) f2 }

show_text :: String -> (GameState -> GameState) -> (GameState -> GameState)
show_text text f = \g ->
  g { textShown = Just text , textScale = 1.0, gameStep = f }

-- | A function that displays a countdown to the game start | Assumes the game is running perfectly at 60 fps
countdown :: Int -> (GameState -> GameState) -> (GameState -> GameState)
countdown ticks f = \g ->
  if ticks == 0 then
    g { gameStep = f , textShown = Nothing }
  else 
    g { gameStep = countdown (ticks - 1) f , textShown = textShown' , textScale = 1 - (getDecimal (timeElapsed g)) }
  where 
    textShown' = Just (show (1 + quot ticks 60))

wait_clear :: (GameState -> GameState) -> (GameState -> GameState)
wait_clear f = \g ->
  if length (enemies g) == 0 then g { gameStep = f } else g 

reset_player :: (GameState -> GameState) -> (GameState -> GameState)
reset_player f = \g ->
  g { player = (player g) { curHealth = (maxHealth (player g)) , position = (0, 0) } 
    , gameStep = f 
    , playerAlive = True}

levelup :: (GameState -> GameState) -> (GameState -> GameState)
levelup level_x = \g ->
  g { gameStep = level_x , currentLevel = level_x }

game_over :: Maybe String -> (GameState -> GameState)
game_over text = \g ->
  g { playerAlive = False, textShown = text, textScale = 1.0 }

tutorial_0 :: GameState -> GameState
tutorial_0 = reset_player
           $ show_text "WASD to move"
           $ run_for 180
           $ show_text "Click to shoot"
           $ run_for 180
           $ show_text "Kill the zoomer"
           $ spawn_enemies tutorial_0_spawn
           $ wait_clear
           $ show_text "Good"
           $ reset_player
           $ run_for 90
           $ levelup tutorial_1

tutorial_1 :: GameState -> GameState
tutorial_1 = show_text "Press 2"
           $ run_for 180
           $ show_text "Starburst"
           $ run_for 90
           $ countdown 180
           $ spawn_enemies tutorial_1_spawn
           $ wait_clear
           $ show_text "Good"
           $ reset_player
           $ run_for 90
           $ levelup tutorial_2

tutorial_2 :: GameState -> GameState
tutorial_2 = show_text "Press 3"
           $ run_for 90
           $ show_text "Boomerang"
           $ run_for 90
           $ countdown 180
           $ spawn_enemies tutorial_1_spawn
           $ wait_clear
           $ show_text "Hit it backwards"
           $ run_for 90
           $ countdown 180
           $ spawn_enemies tutorial_1_spawn
           $ wait_clear
           $ show_text "Good"
           $ reset_player
           $ run_for 90
           $ levelup level_0

level_0 :: GameState -> GameState
level_0 = show_text "Good Luck"
        $ run_for 60
        $ countdown 180
        $ spawn_enemies level_0_spawn
        $ wait_clear
        $ reset_player
        $ show_text "Good"
        $ run_for 60
        $ levelup level_1

level_1 :: GameState -> GameState
level_1 = show_text "Dodge This"
        $ run_for 60
        $ countdown 180
        $ spawn_enemies level_1_spawn
        $ wait_clear
        $ show_text "Good"
        $ reset_player
        $ run_for 90
        $ levelup level_2

level_2 :: GameState -> GameState
level_2 = show_text "Again"
        $ run_for 60
        $ countdown 180
        $ spawn_enemies level_2_spawn
        $ wait_clear
        $ show_text "Hey nice"
        $ run_for 120
        $ show_text "That's all (ESC)"
        $ (\g -> g)
