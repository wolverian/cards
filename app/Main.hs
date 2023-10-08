module Main where

import Cards qualified
import Klondike qualified
import System.Random qualified as Random

data Game = Game
    deriving (Show)

main :: IO ()
main = do
    print Cards.regularDeck
    g <- Random.initStdGen
    print $ Cards.shuffledDeck g
    print $ Klondike.initial g
