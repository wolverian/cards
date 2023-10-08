module Klondike where

import Cards (Card, Deck (..), shuffledDeck)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromJust)
import Data.Vector qualified as Unsized
import Data.Vector.Sized (Vector, enumFromN', fromSized, take', toList, toSized, unfoldrN)
import Data.Vector.Sized qualified as Sized
import GHC.TypeLits (KnownNat, SNat, type (+))
import System.Random (RandomGen (split))
import Prelude hiding (replicate)

type Pile = Unsized.Vector Card

newtype Tableau = Tableau (Vector 7 Pile)
    deriving (Show)

newtype Stock = Stock Pile
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

class FromDeck f where
    fromDeck :: Deck -> f

instance FromDeck Stock where
    fromDeck :: Deck -> Stock
    fromDeck (Deck cards) = Stock (fromSized cards)

newtype Waste = Waste Pile
    deriving (Show)
    deriving newtype (Semigroup, Monoid)

newtype Foundations = Foundations (Vector 4 Pile)
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
                ( \n (cards', columns) ->
                    let (column, rest) = splitAt n cards' in (rest, column : columns)
                )
                (toList cards, mempty)
                counts
     in Tableau (Unsized.fromList <$> fromJust (Sized.fromList columns))
