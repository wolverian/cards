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

shuffledDeck :: (RandomGen g) => g -> Deck
shuffledDeck = shuffle regularDeck

shuffle :: (RandomGen g) => Deck -> g -> Deck
shuffle (Deck cards) g = Deck $ Random.shuffle' cards 52 g
