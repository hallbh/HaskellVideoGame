module Keyboard where

import Game
import Entity

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle

-- | Enum I made to help keep track of wasd inputs
data MoveDir = UP | DOWN | LEFT | RIGHT | NOUP | NODOWN | NOLEFT | NORIGHT
  deriving (Enum, Eq)

----------------------------------------------------------------------------------------
-- Response functions
----------------------------------------------------------------------------------------
setPlayerVel :: MoveDir -> GameState -> GameState
setPlayerVel direction game 
  | direction == UP      = game { player = (player game) { move_direction = (x, y + 1) } }
  | direction == DOWN    = game { player = (player game) { move_direction = (x, y - 1) } }
  | direction == LEFT    = game { player = (player game) { move_direction = (x - 1, y) } }
  | direction == RIGHT   = game { player = (player game) { move_direction = (x + 1, y) } }
  | direction == NOUP    = game { player = (player game) { move_direction = (x, y - 1) } }
  | direction == NODOWN  = game { player = (player game) { move_direction = (x, y + 1) } }
  | direction == NOLEFT  = game { player = (player game) { move_direction = (x + 1, y) } }
  | direction == NORIGHT = game { player = (player game) { move_direction = (x - 1, y) } }
  where 
    (x, y) = move_direction (player game)

setPlayerWeapon :: Weapon -> GameState -> GameState
setPlayerWeapon weap game
  | weap == BASIC     = game { player = (player game) { weapon_type = BASIC , shoot_cooldown = 0 } }
  | weap == STARBURST = game { player = (player game) { weapon_type = STARBURST , shoot_cooldown = 0.5 }} -- half the cooldown so people can't just spam 2 to spam shoot
  | weap == BOOMERANG = game { player = (player game) { weapon_type = BOOMERANG , shoot_cooldown = 0.25 }}

playerTurnAndShoot :: (Float, Float) -> GameState -> GameState
playerTurnAndShoot (mouse_x, mouse_y) game = 
  game { player = (player game) { facing = degrees , shoot_cooldown = shoot_cooldown'} 
       , bullets = bullets' 
       }
  where
    p = player game
    (player_x, player_y) = position p
    dx = mouse_x - player_x 
    dy = mouse_y - player_y
    degrees = if dy > 0 then radToDeg( atan (dx/dy) ) else 180 + radToDeg (atan (dx/dy))
    bullets' = if (shoot_cooldown p) == 0 then 
                (bullets game) ++ 
                       if (weapon_type p) == BASIC     then generateBasicBullet degrees (player_x, player_y)
                  else if (weapon_type p) == STARBURST then generateStarburst   degrees (player_x, player_y) 
                  else if (weapon_type p) == BOOMERANG then generateBoomerang   degrees (player_x, player_y)
                  else []
               else (bullets game)
    shoot_cooldown' = if (shoot_cooldown p) == 0 then
                        if (weapon_type p) == BASIC then 0 else
                        if (weapon_type p) == STARBURST then 1 else
                        if (weapon_type p) == BOOMERANG then 0.5 else 0
                      else (shoot_cooldown p)

-- | Key listener function
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey key keyState _ mouse) game
-- Player movement
  | key == Char 'w' && keyState == Down = setPlayerVel UP      game
  | key == Char 'w' && keyState == Up   = setPlayerVel NOUP    game
  | key == Char 'a' && keyState == Down = setPlayerVel LEFT    game
  | key == Char 'a' && keyState == Up   = setPlayerVel NOLEFT  game
  | key == Char 's' && keyState == Down = setPlayerVel DOWN    game
  | key == Char 's' && keyState == Up   = setPlayerVel NODOWN  game
  | key == Char 'd' && keyState == Down = setPlayerVel RIGHT   game
  | key == Char 'd' && keyState == Up   = setPlayerVel NORIGHT game
-- Mouse click reacting
  | key == MouseButton LeftButton && keyState == Down && playerAlive game = playerTurnAndShoot mouse game
-- Pause
  | key == Char 'p' && keyState == Down && playerAlive game = game { isPaused = not (isPaused game) }
-- New Game
  | key == Char 'n' && keyState == Down = initialState
-- Retry
  | key == Char 'r' && keyState == Down = game { player = (player game) { curHealth = (maxHealth (player game))
                                                                        , position = (0, 0) }
                                               , gameStep = (currentLevel game)
                                               , enemies = []
                                               , textShown = Nothing }
-- Change weapons 1 - basic, 2 - starburst, 3 - TBD
  | key == Char '1' && keyState == Down = setPlayerWeapon BASIC     game
  | key == Char '2' && keyState == Down = setPlayerWeapon STARBURST game
  | key == Char '3' && keyState == Down = setPlayerWeapon BOOMERANG game

handleKeys _ game = game