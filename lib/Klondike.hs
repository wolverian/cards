module Klondike
    ( Pile
    , Tableau (..)
    , Stock (..)
    , FromDeck (..)
    , Waste (..)
    , Foundations (..)
    , Game (..)
    , initial
    , genTableau
    ) where

import Prelude hiding (replicate)

import Data.Maybe (fromJust)
import Data.Vector qualified as Unsized
import Data.Vector.Sized qualified as Sized
import System.Random (RandomGen (split))

import Cards (Card, Deck (..), shuffledDeck)

type Pile = Unsized.Vector Card

newtype Tableau = Tableau (Sized.Vector 7 Pile)
    deriving (Show)

newtype Stock = Stock Pile
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

class FromDeck f where
    fromDeck :: Deck -> f

instance FromDeck Stock where
    fromDeck :: Deck -> Stock
    fromDeck (Deck cards) = Stock (Sized.fromSized cards)

newtype Waste = Waste Pile
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

newtype Foundations = Foundations (Sized.Vector 4 Pile)
    deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

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
            { waste = Waste mempty
            , foundations = mempty
            , stock = fromDeck (shuffledDeck g)
            , tableau = genTableau deck
            , randomGen = g'
            }

genTableau :: Deck -> Tableau
genTableau (Deck cards) =
    let counts = [1 .. 7] :: [Int]
        ([], columns) =
            foldr
                ( \n (cards', columns') ->
                    let (column, rest) = splitAt n cards' in (rest, column : columns')
                )
                (Sized.toList cards, mempty)
                counts
     in Tableau (Unsized.fromList <$> fromJust (Sized.fromList columns))
