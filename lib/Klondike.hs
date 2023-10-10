module Klondike
    ( Tableau (..)
    , Stock (..)
    , Waste (..)
    , Foundations (..)
    , Klondike (..)
    , newGame
    , turn
    ) where

import Data.Vector qualified as Unsized
import Data.Vector.Sized qualified as Sized

import Cards (Deck (..), Pile, UnsizedPile)

newtype Tableau = Tableau (Sized.Vector 7 UnsizedPile)
    deriving (Show)

newtype Stock n = Stock (Pile n)
    deriving (Show)

newtype Waste n = Waste (Pile n)
    deriving (Show)

newtype Foundations = Foundations (Sized.Vector 4 UnsizedPile)
    deriving stock (Show)
    deriving newtype (Semigroup, Monoid)

data Move
    = TurnStock
    | MoveWasteTableau
    | MoveWasteFoundation
    | MoveTableauFoundation
    | MoveFoundationTableau
    | MoveTableauTableau
    deriving (Show)

turn :: Klondike s w -> Move -> Klondike s' w'
turn k = \case
    TurnStock ->
        _turnStock
    MoveWasteTableau ->
        _moveWasteTableau
    MoveWasteFoundation ->
        _moveWasteFoundation
    MoveTableauFoundation ->
        _moveTableauFoundation
    MoveFoundationTableau ->
        _moveFoundationTableau
    MoveTableauTableau ->
        _moveTableauTableau

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
    let cards' = Sized.fromSized cards
     in ( Stock $ Sized.drop cards
        , Tableau $ Sized.generate \n ->
            let withoutPrevCols = Unsized.drop (sum [0 .. fromIntegral n]) cards'
             in Unsized.take (fromIntegral n + 1) withoutPrevCols
        )
