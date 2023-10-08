module Cards
    ( Suit (..)
    , Rank (..)
    , value
    , Card (..)
    , Deck (..)
    , regularDeck
    , Hand (..)
    , shuffledDeck
    , shuffle
    ) where

import Data.Maybe (fromJust)
import Data.Vector.Sized qualified as Sized
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import System.Random (RandomGen)
import System.Random.Shuffle qualified as Random

data Suit = Diamond | Clubs | Hearts | Spades
    deriving (Read, Eq, Enum, Generic)

instance Show Suit where
    show :: Suit -> String
    show = \case
        Diamond -> "♦"
        Clubs -> "♣"
        Hearts -> "♥"
        Spades -> "♠"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Read, Eq, Ord, Enum, Generic)

instance Show Rank where
    show :: Rank -> String
    show = show . value

value :: Rank -> Int
value = (+ 1) . fromEnum

data Card = Card Suit Rank
    deriving (Read, Eq, Generic)

instance Show Card where
    show :: Card -> String
    show (Card suit rank) = show suit ++ " " ++ show rank

newtype Deck = Deck (Sized.Vector 52 Card)
    deriving (Show, Read, Eq, Generic)

newtype Pile n = Pile (Sized.Vector n Card)
    deriving (Show, Read, Eq, Generic)

regularDeck :: Deck
regularDeck =
    Deck
        . fromJust
        $ Sized.fromListN [Card suit rank | suit <- [Diamond .. Spades], rank <- [Ace .. King]] -- consider writing this in terms of sized vectors

newtype Hand n = Hand (Sized.Vector n Card)
    deriving (Show, Read, Eq)

shuffledDeck :: (RandomGen g) => g -> Deck
shuffledDeck g =
    let Deck cards = regularDeck
        Pile cards' = shuffle (Pile cards) g
     in Deck cards'

shuffle :: (RandomGen g, KnownNat n) => Pile n -> g -> Pile n
shuffle (Pile cards) =
    -- todo: write a shuffle for sized vectors to get rid of this mess and random-shuffle
    Pile
        . fromJust
        . Sized.fromList
        . Random.shuffle' (Sized.toList cards) (length cards)
