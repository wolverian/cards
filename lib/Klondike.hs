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

type Pile n = Sized.Vector n Card
type UnsizedPile = Unsized.Vector Card

newtype Tableau = Tableau (Sized.Vector 7 UnsizedPile)
    deriving (Show)

newtype Stock n = Stock (Pile n)
    deriving (Show)

newtype Waste n = Waste (Pile n)
    deriving (Show)

newtype Foundations = Foundations (Sized.Vector 4 UnsizedPile)
    deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

data Game s w = Game
    { tableau :: Tableau
    , stock :: Stock s
    , waste :: Waste w
    , foundations :: Foundations
    }
    deriving (Show)

newGame :: Deck -> Game 24 0
newGame deck =
    let (stock, tableau) = genTableau deck
     in Game
            { waste = Waste Sized.empty
            , foundations = mempty
            , stock
            , tableau
            }

genTableau :: Deck -> (Stock 24, Tableau)
genTableau (Deck cards) =
    let cards' = Sized.fromSized cards
     in ( Stock $ Sized.drop cards
        , Tableau $ Sized.generate \n ->
            let withoutPrevCols = Unsized.drop (sum [0 .. fromIntegral n]) cards'
             in Unsized.take (fromIntegral n + 1) withoutPrevCols
        )
