module Main where

import Cards qualified
import Klondike qualified

data Game = Game
    deriving (Show)

main :: IO ()
main = print Cards.regularDeck
