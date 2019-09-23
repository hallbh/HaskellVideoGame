module Render where

import Game
import Entity

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

window :: Display
window = FullScreen

background :: Color
background = black

fps :: Int
fps = 60

-- | Derpy helper function for drawGameObject
hpRatio :: GameObject -> Float
hpRatio go = (curHealth go) / (maxHealth go)
showText :: Maybe String -> Float -> [Picture]
showText textToShow textScale
  | (isNothing textToShow) = []
  | True = if (isJust textToShow) 
            then ([ scale textScale textScale 
                 $ translate (-center) 0
                 $ color white 
                 $ text (fromJust textToShow)])
            else []
              where 
                center = (fromIntegral (length (fromJust textToShow))) * (40 * textScale)

-- | Draws game objects
drawGameObject :: GameObject -> Picture
drawGameObject go = 
  pictures [(uncurry translate (position go)
           $ rotate (facing go)
           $ color (entityColor go)
           $ lineLoop (model go)) 
           ,(uncurry translate (position go)
           $ color hpColor 
           $ polygon [(-15, -20), (-15, -22), (hpRemain, -22), (hpRemain, -20)])]
    where
      hpColor = 
        if (hpRatio go > 2.1/3.0) then green
        else if (hpRatio go > 1.1/3.0) then yellow
        else red
      hpRemain = (-15.0) + (30.0 * (hpRatio go))

-- | Draws Bullets
drawBullet :: Bullet -> Picture
drawBullet b = uncurry translate (bullet_pos b)
             $ rotate (bullet_fac b)
             $ (bullet_pic b)

drawBorders :: Picture
drawBorders = color red (rectangleWire 1500 1000)

-- | Function to turn the gamestate into a picture
render :: GameState -> Picture
render game =
  pictures ([playerPicture] ++ enemyPictures ++ bulletPictures ++ textPicture ++ [borders])
  where
    playerPicture = drawGameObject (player game)
    enemyPictures = map drawGameObject (enemies game)
    bulletPictures = map drawBullet (bullets game)
    textPicture = showText (textShown game) (textScale game)
    borders = drawBorders