module Cards
    ( Suit (..)
    , Rank (..)
    , value
    , Card (..)
    , Deck (..)
    , regularDeck
    , Hand (..)
    , shuffledDeck
    ) where

import Data.Bifunctor (Bifunctor (bimap, first))
import Data.List (unfoldr)
import System.Random (RandomGen)
import System.Random.Shuffle qualified as Random

data Suit = Diamond | Clubs | Hearts | Spades deriving (Read, Eq, Enum)

instance Show Suit where
    show :: Suit -> String
    show = \case
        Diamond -> "♦"
        Clubs -> "♣"
        Hearts -> "♥"
        Spades -> "♠"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Read, Eq, Ord, Enum)

instance Show Rank where
    show :: Rank -> String
    show = show . value

value :: Rank -> Int
value = (+ 1) . fromEnum

data Card = Card Suit Rank
    deriving (Read, Eq)

instance Show Card where
    show :: Card -> String
    show (Card suit rank) = show suit ++ " " ++ show rank

newtype Deck = Deck [Card]
    deriving (Show, Read, Eq)

regularDeck :: Deck
regularDeck = Deck [Card suit rank | suit <- [Diamond .. Spades], rank <- [Ace .. King]]

newtype Hand = Hand [Card]
    deriving (Show, Read, Eq)

shuffledDeck :: (RandomGen g) => g -> (Deck, g)
shuffledDeck =
    let Deck cards = regularDeck
     in first Deck . shuffle cards

shuffle :: (RandomGen g) => [Card] -> g -> ([Card], g)
shuffle cards g =
    let (idx, g') = genIndices [0 .. 51] g
        cards' = Random.shuffle cards idx
     in (cards', g')
  where
    genIndices :: [Int] -> g -> ([Int], g)
    genIndices l g = _
