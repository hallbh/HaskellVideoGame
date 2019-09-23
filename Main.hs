module Main where

import Graphics.Gloss.Interface.Pure.Game

import Game
import Entity
import Render
import Keyboard

main :: IO ()
main = play window background fps initialState render handleKeys update