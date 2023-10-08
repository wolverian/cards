module Klondike where

import Cards
import System.Random (RandomGen (split), uniform, uniformR)

type Pile = [Card]

newtype Tableau = Tableau Pile
    deriving (Show)

newtype Stock = Stock Pile
    deriving (Show)

class FromDeck f where
    fromDeck :: Deck -> f

instance FromDeck Stock where
    fromDeck :: Deck -> Stock
    fromDeck (Deck cards) = Stock cards

newtype Waste = Waste Pile
    deriving (Show)

newtype Foundations = Foundations [Pile]
    deriving (Show)

data Game g = Game
    { tableau :: Tableau
    , stock :: Stock
    , waste :: Waste
    , foundations :: Foundations
    , randomGen :: g
    }
    deriving (Show)

initial :: (RandomGen g) => g -> Game g
initial g =
    let (g', g'') = split g
        deck = shuffledDeck g''
     in Game
            { waste = Waste []
            , foundations = Foundations [[], [], [], []]
            , stock = fromDeck (shuffledDeck g)
            , tableau = genTableau deck
            , randomGen = g'
            }

genTableau :: Deck -> Tableau
genTableau g =
    let counts = [1 .. 7]
     in (Tableau _, g)
