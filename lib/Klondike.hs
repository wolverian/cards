module Klondike
    ( Pile
    , Tableau (..)
    , Stock (..)
    , Waste (..)
    , Foundations (..)
    , Game (..)
    , newGame
    ) where

import Data.Vector qualified as Unsized
import Data.Vector.Sized qualified as Sized

import Cards (Card, Deck (..))

type Pile = Unsized.Vector Card

newtype Tableau = Tableau (Sized.Vector 7 Pile)
    deriving (Show)

newtype Stock = Stock Pile
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

newtype Waste = Waste Pile
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

newtype Foundations = Foundations (Sized.Vector 4 Pile)
    deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

data Game = Game
    { tableau :: Tableau
    , stock :: Stock
    , waste :: Waste
    , foundations :: Foundations
    }
    deriving (Show)

newGame :: Deck -> Game
newGame deck =
    let (stock, tableau) = genTableau deck
     in Game
            { waste = mempty
            , foundations = mempty
            , stock
            , tableau
            }

genTableau :: Deck -> (Stock, Tableau)
genTableau (Deck cards) =
    let cards' = Sized.fromSized cards
        tableau = Sized.generate \n ->
            let withoutPrevCols = Unsized.drop (sum [0 .. fromIntegral n]) cards'
             in Unsized.take (fromIntegral n + 1) withoutPrevCols
        stock :: Sized.Vector 24 Card = Sized.drop cards
     in ( Stock $ Sized.fromSized stock
        , Tableau tableau
        )
