module Klondike
    ( Pile
    , Tableau (..)
    , Stock (..)
    , Waste (..)
    , Foundations (..)
    , Game (..)
    , initial
    ) where

import Data.Maybe (fromJust)
import Data.Vector qualified as Unsized
import Data.Vector.Sized qualified as Sized
import System.Random (RandomGen)

import Cards (Card, Deck (..), shuffledDeck)

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

-- todo: maybe it would be better to just take in the shuffled deck as a parameter? do games have different shuffling rules?
initial :: (RandomGen g) => g -> Game
initial g =
    let deck = shuffledDeck g
        (stock, tableau) = genTableau deck
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
        trustMeBro = fromJust $ Sized.fromList columns
        tableau = Unsized.fromList <$> trustMeBro
        stock = Unsized.fromList rest
     in (Stock stock, Tableau tableau)
