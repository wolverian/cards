module Klondike
    ( Pile
    , Tableau (..)
    , Stock (..)
    , Waste (..)
    , Foundations (..)
    , Game (..)
    , newGame
    ) where

import Data.Maybe (fromJust)
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
    let counts = [1 .. 7]
        (rest, columns) =
            foldr
                ( \n (cards', columns') ->
                    let (column, rest') = splitAt n cards'
                     in (rest', column : columns')
                )
                (Sized.toList cards, mempty)
                counts
        -- fixme: figure out how to do this with sized vectors
        trustMeBro = fromJust $ Sized.fromList columns
        tableau = Unsized.fromList <$> trustMeBro
        stock = Unsized.fromList rest
     in (Stock stock, Tableau tableau)
