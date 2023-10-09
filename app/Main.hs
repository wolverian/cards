module Main where

import System.Random qualified as Random

import Cards qualified
import Klondike qualified

data Game = Game
    deriving (Show)

main :: IO ()
main = do
    print Cards.regularDeck
    deck <- Cards.shuffledDeck <$> Random.initStdGen
    print $ Klondike.newGame deck
