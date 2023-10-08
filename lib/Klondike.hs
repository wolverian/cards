module Klondike where

import Cards
import System.Random (RandomGen)

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
    Game
        { waste = Waste []
        , foundations = Foundations [[], [], [], []]
        , stock = fromDeck (shuffledDeck g)
        , tableau = genTableau g
        , randomGen = g
        }

genTableau :: (RandomGen g) => g -> Tableau
genTableau = _
