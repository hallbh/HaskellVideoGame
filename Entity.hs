module Entity where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle

-- | Data type for spawning large groups of enemies
data SpawnInfo = SI Point Float EnemyType
  deriving (Eq)
type EnemyGroup = [SpawnInfo]

data Weapon = BASIC | STARBURST | BOOMERANG | ENEMY
  deriving (Enum, Eq)

data EnemyType = Zoomer | Seeker | Carrier
  deriving (Enum, Eq)

-- | Bullets. They hit enemies.
data Bullet = Bullet 
  { lifespan :: Float -- ^ How long it fly
  , damage :: Float   -- ^ How hard it hit
  , bullet_pos :: Point -- ^ Where it be
  , bullet_vel :: Float -- ^ How fast it go
  , bullet_rot :: Float -- ^ How much it spin
  , bullet_dir :: Float -- ^ direction in degrees because I can't make up my mind
  , bullet_fac :: Float -- ^ Facing of the bullet for the purposes of drawing
  , bullet_step :: (Bullet -> [Bullet]) -- ^ The bullet behavior function
  , bullet_pic :: Picture -- ^ What the bullet looks like (Fancy composed pictures because we're special)
  , bullet_hit_radius :: Float -- ^ Because everything hits stuff 
  }
-- | Entities in the game - enemies, player, etc
data GameObject = GO
  { maxHealth :: Float     -- ^ The max health of the entity 
  , curHealth :: Float     -- ^ the current health of the entity
  , position :: Point -- ^ The position of the entity
  , velocity :: Float -- ^ How fast it goes
  , step :: (GameObject -> [GameObject]) -- ^ The update function of the object
  , shoot :: (Float -> Point -> [Bullet]) -- ^ The shoot function of the object 
  , shoot_cooldown :: Float -- ^ The cooldown between shots, in seconds.
  , weapon_type :: Weapon -- ^ The weapon being shot. Directly affects shoot_cooldown
  , model :: Path -- ^ The outline of the entity (using lineLoop)
  , entityColor :: Color -- ^ The color of the entity
  , facing :: Float -- ^ The direction of the front of the entity in degrees because the people that made gloss are heathens
  , move_direction :: Point -- ^ The direction of the motion as a vector
  , hit_radius :: Float -- ^ The radius of the hitbox (circle) of this game object
  }

-- | The player's ship. Pointy triangle boi
playerModel :: Path
playerModel = [(0, 15), (-7.5, -15), (0, -7.5), (7.5, -15)]
-- | A zoomer. Looks like a derpy plane
zoomer :: Path
zoomer = [(0, 20), (-5, 10), (-5, 0), (-30, -15), (-5, -15), (-5, -20), 
          (5, -20), (5, -15), (30, -15), (5, 0), (5, 10)]
-- | A seeker. Looks like a mean zoomer
seeker :: Path
seeker = [(0, 20), (-15, 10), (-5, 10), (-5, 0), (-30, -15), (-5, -15), (-5, -20), 
          (5, -20), (5, -15), (30, -15), (5, 0), (5, 10), (15, 10)]
-- | A carrier. Looks cool but its healthbar is derpy
carrier :: Path
carrier = [(0, 32), (-24, 20), (-4, 20), (-4, -24), (-16, -24), (-20, -32), 
           (20, -32), (16, -24), (4, -24), (4, 20), (24, 20)]
-- | Carrier
-- | A bullet. Looks like a rectangle
basicBullet :: Path
basicBullet = [(-1, -4), (-1, 4), (1, 4), (1, -4)]

starburst_helper :: Float -> Picture
starburst_helper b = rotate (b * 45)
             $ translate 0 6
             $ color (light yellow) (polygon basicBullet)

starburst_pic :: Picture
starburst_pic = pictures (basics ++ [body])
  where
    basics = [ starburst_helper 0
             , starburst_helper 1
             , starburst_helper 2
             , starburst_helper 3
             , starburst_helper 4
             , starburst_helper 5
             , starburst_helper 6
             , starburst_helper 7
             ]
    body = color (dark red) (circleSolid 4)

boomerang_pic :: Picture
boomerang_pic = pictures [l, r]
  where
    l = color (light yellow) (polygon [(0, 3), (-1, 2), (4, -3), (5, -2)])
    r = color (light yellow) (polygon [(0, 3), (-5, -2), (-4, -3), (1, 2)])

tutorial_0_spawn :: EnemyGroup
tutorial_0_spawn = [SI (0, 500) 180 Zoomer]

tutorial_1_spawn :: EnemyGroup
tutorial_1_spawn = [ SI (0, 500) 135 Zoomer
                   , SI (-500, 0) 135 Zoomer
                   , SI (0, -500) 135 Zoomer
                   , SI (500, 0) 135 Zoomer
                   ]

level_0_spawn :: EnemyGroup
level_0_spawn = [SI (-250, 250)  90  Zoomer
                ,SI (250, 250)   180 Zoomer
                ,SI (250, -250)  270 Zoomer
                ,SI (-250, -250) 0   Zoomer
                ,SI (-250, 0)    45  Zoomer
                ,SI (250, 0)     225 Zoomer
                ,SI (0, -250)    315 Zoomer
                ,SI (0, 250)     135 Zoomer
                ]

level_1_spawn :: EnemyGroup
level_1_spawn = [SI (-250, 250)  90  Zoomer
                ,SI (250, 250)   180 Zoomer
                ,SI (250, -250)  270 Zoomer
                ,SI (-250, -250) 0   Zoomer
                ,SI (-250, 0)    45  Seeker
                ,SI (250, 0)     225 Seeker
                ,SI (0, -250)    315 Seeker
                ,SI (0, 250)     135 Seeker
                ]

level_2_spawn :: EnemyGroup
level_2_spawn = [SI (-250, 250)  90  Carrier
                ,SI (250, 250)   180 Carrier
                ,SI (-250, 0)    45  Seeker
                ,SI (250, 0)     225 Seeker
                ,SI (0, -250)    315 Seeker
                ,SI (0, 250)     135 Seeker
                ]

-- | The initial state of the player
initialPlayer :: GameObject
initialPlayer = GO
  { maxHealth = 3
  , curHealth = 3
  , position = (0, 0)
  , velocity = 500
  , step = (\a -> [a]) -- no effect, player does what they want (for now)
  , shoot = generateBasicBullet
  , shoot_cooldown = 0
  , weapon_type = BASIC
  , model = playerModel
  , entityColor = light blue
  , facing = 180
  , move_direction = (0, 0)
  , hit_radius = 10
  }
-- | A function that spits out a zoomer
generateZoomer :: Point -> Float -> GameObject
generateZoomer pos face = GO 
  { maxHealth = 2
  , curHealth = 2
  , position = pos
  , velocity = 525
  , step = (\a -> [a]) -- no effect
  , shoot = no_shoot
  , shoot_cooldown = 0
  , weapon_type = ENEMY
  , model = zoomer
  , entityColor = light red
  , facing = face
  , move_direction = (0, 0)
  , hit_radius = 20
  }

generateSeeker :: Point -> Float -> GameObject
generateSeeker pos face = GO
  { maxHealth = 3
  , curHealth = 3
  , position = pos
  , velocity = 0
  , step = seeker_step
  , shoot = no_shoot
  , shoot_cooldown = 0
  , weapon_type = ENEMY
  , model = seeker
  , entityColor = light magenta
  , facing = face
  , move_direction = (0, 0)
  , hit_radius = 20
  }

generateCarrier :: Point -> Float -> GameObject
generateCarrier pos face = GO
  { maxHealth = 10
  , curHealth = 10
  , position = pos
  , velocity = 450
  , step = carrier_step
  , shoot = no_shoot
  , shoot_cooldown = 0
  , weapon_type = ENEMY
  , model = carrier
  , entityColor = light orange
  , facing = face
  , move_direction = (0, 0)
  , hit_radius = 32
  }
----------------------------------------------------------------------------------------
-- Bullet building blocks
----------------------------------------------------------------------------------------
-- | Some enemies don't shoot
no_shoot :: Float -> Point -> [Bullet]
no_shoot _ _ = []
-- | Pew pew
generateBasicBullet :: Float -> Point -> [Bullet]
generateBasicBullet dir pos = 
  [Bullet 
  { lifespan = 10
  , damage = 1
  , bullet_pos = pos
  , bullet_vel = 800
  , bullet_dir = dir
  , bullet_rot = 0
  , bullet_fac = dir
  , bullet_step = (\b -> [b])
  , bullet_pic = color (light yellow) (polygon basicBullet)
  , bullet_hit_radius = 0 -- Trust me on this one it really doesn't matter
  }]
-- | Pew ... pewpewpwepwepwpewpewpewpepwepw
generateStarburst :: Float -> Point -> [Bullet]
generateStarburst dir pos = 
  [Bullet
  { lifespan = 10
  , damage = 2
  , bullet_pos = pos
  , bullet_vel = 600
  , bullet_dir = dir
  , bullet_rot = 6
  , bullet_fac = dir
  , bullet_step = startburst_step
  , bullet_pic = starburst_pic
  , bullet_hit_radius = 8
  }]
-- | Pew ... weeeeeeeeeeep
generateBoomerang :: Float -> Point -> [Bullet]
generateBoomerang dir pos =
  [Bullet
  { lifespan = 10
  , damage = 1
  , bullet_pos = pos
  , bullet_vel = 600
  , bullet_dir = dir
  , bullet_rot = 6
  , bullet_fac = dir
  , bullet_step = boomerang_step
  , bullet_pic = boomerang_pic
  , bullet_hit_radius = 4
  }]
----------------------------------------------------------------------------------------
-- Movement basic functions
----------------------------------------------------------------------------------------
-- | Bullet basic move function, NEVER modifies bullet_step. I made that mistake already with game over, might fix it later
move_bullets :: Float -> Bullet -> [Bullet]
move_bullets seconds b 
  | lifespan b - seconds <= 0 = [] -- remove the bullet if it's somehow still around after its lifespan
  | (x' > 750) || (x' < -750) || (y' > 500) || (y' < -500) = [] -- remove the bullet if it goes off screen
  | lifespan b - seconds > 0  = [b { lifespan = lifespan', bullet_pos = position' , bullet_fac = bullet_fac' }]
    where
      (x, y) = bullet_pos b
      v = bullet_vel b
      dir = bullet_dir b
      x' = x + v * sin(degToRad(dir)) * seconds
      y' = y + v * cos(degToRad(dir)) * seconds
      lifespan' = lifespan b - seconds
      position' = (x', y')
      bullet_fac' = (bullet_fac b) + (bullet_rot b)
-- | Player basic move function
move_player :: Float -> GameObject -> [GameObject]
move_player seconds go = [go { position = position' , shoot_cooldown = shoot_cooldown'} ]
  where
    v = velocity go
    (x, y) = position go
    (theta_x, theta_y) = move_direction go
    hyp = sqrt ((theta_x*theta_x)+(theta_y*theta_y)) -- for normalization of the move direction vector
    x' = if hyp > 0 then x + v * seconds * (theta_x / hyp) else x
    y' = if hyp > 0 then y + v * seconds * (theta_y / hyp) else y
    position' = if x' > 730    then (730, y')    else
                if x' < (-730) then ((-730), y') else
                if y' > 480    then (x', 480)    else
                if y' < (-480) then (x', (-480)) else (x', y')
    shoot_cooldown' = if (shoot_cooldown go) > 0 then max 0 ((shoot_cooldown go) - seconds) else 0
-- | zoomer basic move function, also removes the enemy if it's dead
move_enemy :: Float -> GameObject -> [GameObject]
move_enemy seconds go 
  | curHealth go <= 0 = []
  | True = [go { position = position' }]
      where
        v = velocity go
        (x, y) = position go
        dir = facing go
        x' = x + v * seconds * sin(degToRad(dir))
        y' = y + v * seconds * cos(degToRad(dir))
        position' = if x' > 730.0 then ((-730.0), y') else
                    if x' < (-730.0) then (730.0, y') else
                    if y' > 480.0 then (x', (-480.0)) else
                    if y' < (-480.0) then (x', 480.0) else (x', y')
-----------------------------------------------------------------------------------------
-- Bullet building blocks
-----------------------------------------------------------------------------------------
slowToZero :: (Bullet -> [Bullet]) -> (Bullet -> [Bullet])
slowToZero f = \b ->
  if bullet_vel b == 0 then [ b { bullet_step = f } ]
  else [ b { bullet_vel = max ((bullet_vel b) - 10) 0 } ]

spinUpTo :: Float -> (Bullet -> [Bullet]) -> (Bullet -> [Bullet])
spinUpTo total f = \b ->
  if (bullet_rot b) >= total then [ b { bullet_step = f } ]
  else [ b { bullet_rot = (bullet_rot b) + 0.25 } ]

burst :: (Bullet -> [Bullet]) -> (Bullet -> [Bullet])
burst f = \b -> generateBasicBullet (bullet_fac b + 0)     (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 45)    (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 90)    (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 135)   (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 180)   (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 225)   (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 270)   (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 315)   (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 22.5)  (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 67.5)  (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 112.5) (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 157.5) (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 200.5) (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 247.5) (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 292.5) (bullet_pos b)
             ++ generateBasicBullet (bullet_fac b + 337.5) (bullet_pos b)

startburst_step :: Bullet -> [Bullet]
startburst_step = slowToZero
                $ spinUpTo 12
                $ burst
                $ (\b -> [b])

tripleDamage :: (Bullet -> [Bullet]) -> (Bullet -> [Bullet])
tripleDamage f = \b ->
  [ b { bullet_step = f , damage = (damage b) * 3 } ]

accelerateTo_b :: Float -> (Bullet -> [Bullet]) -> (Bullet -> [Bullet])
accelerateTo_b speed f = \b ->
  if (bullet_vel b) == speed then [ b { bullet_step = f } ]
  else [ b { bullet_vel = min (bullet_vel b + 30) speed } ]

reverseDirection :: (Bullet -> [Bullet]) -> (Bullet -> [Bullet])
reverseDirection f = \b ->
  [ b { bullet_fac = bullet_fac b + 180 , bullet_dir = bullet_dir b + 180 , bullet_step = f } ]

boomerang_step :: Bullet -> [Bullet]
boomerang_step = slowToZero
               $ reverseDirection
               $ tripleDamage
               $ accelerateTo_b 1200
               $ (\b -> [b])
---------------------------------------------------------------------------------------
-- Enemy Building Blocks. Fun stuff here
---------------------------------------------------------------------------------------
accelerateTo_go :: Float -> (GameObject -> [GameObject]) -> (GameObject -> [GameObject])
accelerateTo_go speed f = \go ->
  if (velocity go) == speed then [ go { step = f } ]
  else [ go { velocity = min (velocity go + 30) speed } ]
-- Once again, this assumes the game runs perfectly at 60 fps. probably bad juice.
waitFor :: Float -> (GameObject -> [GameObject]) -> (GameObject -> [GameObject])
waitFor time f = \go ->
  if time == 0 then
    [go { step = f }]
  else 
    [go { step = waitFor (time - 1) f }]

seekerTurn :: (GameObject -> [GameObject]) -> (GameObject -> [GameObject])
seekerTurn f = \go ->
  [go { facing = facing go + 137.5 , step = f }]

seekerStop :: (GameObject -> [GameObject]) -> (GameObject -> [GameObject])
seekerStop f = \go ->
  [go { step = f , velocity = 0 }]

seeker_step :: GameObject -> [GameObject]
seeker_step = seekerTurn
            $ waitFor 120
            $ accelerateTo_go 1200
            $ waitFor 120
            $ seekerStop
            $ seeker_step

carrierContinue :: Float -> (GameObject -> [GameObject]) -> (GameObject -> [GameObject])
carrierContinue time f = \go ->
  if time == 0 then
    [go { step = f } ]
  else
    [go { step = carrierContinue (time - 1) f , facing = facing go + 1 }]

releaseZoomer :: (GameObject -> [GameObject]) -> (GameObject -> [GameObject])
releaseZoomer f = \go ->
  [go { step = f }, generateZoomer (position go) ((facing go) + 90)]

carrier_step :: GameObject -> [GameObject]
carrier_step = carrierContinue 300
             $ releaseZoomer
             $ carrier_step