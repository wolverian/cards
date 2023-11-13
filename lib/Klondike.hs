module Klondike
    ( Tableau (..)
    , Stock (..)
    , Waste (..)
    , Foundations (..)
    , Klondike (..)
    , newGame
    ) where

import Data.Vector qualified as Unsized
import Data.Vector.Sized qualified as Sized

import Cards (Deck (..), Pile, UnsizedPile)
import Data.Function ((&))

newtype Tableau = Tableau (Sized.Vector 7 UnsizedPile)
    deriving (Show)

newtype Stock n = Stock (Pile n)
    deriving (Show)

newtype Waste n = Waste (Pile n)
    deriving (Show)

newtype Foundations = Foundations (Sized.Vector 4 UnsizedPile)
    deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

-- | @t'Klondike' s w@ is a game of Klondike where @s@ is the current size of the stock and @w@ is the current size of the waste.
data Klondike s w = Klondike
    { tableau :: Tableau
    , stock :: Stock s
    , waste :: Waste w
    , foundations :: Foundations
    }
    deriving (Show)

newGame :: Deck -> Klondike 24 0
newGame deck =
    let (stock, tableau) = genTableau deck
     in Klondike
            { waste = Waste Sized.empty
            , foundations = mempty
            , stock
            , tableau
            }

genTableau :: Deck -> (Stock 24, Tableau)
genTableau (Deck cards) =
    ( Stock $ Sized.drop cards
    , Tableau $ Sized.generate \(fromIntegral -> n) ->
        cards
            |> Sized.fromSized
            |> Unsized.drop (sum [0 .. n])
            |> Unsized.take (n + 1)
    )

(|>) :: a -> (a -> b) -> b
(|>) = (&)
