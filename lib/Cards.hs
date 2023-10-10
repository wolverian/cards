module Cards
    ( Suit (..)
    , suits
    , Rank (..)
    , ranks
    , value
    , Card (..)
    , Deck (..)
    , Pile
    , UnsizedPile
    , regularDeck
    , Hand (..)
    , shuffledDeck
    , shuffle
    , cardsOfRank
    , cardsOfSuit
    ) where

import Data.Maybe (fromJust)
import Data.Vector qualified as Unsized
import Data.Vector.Sized qualified as Sized
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import System.Random (RandomGen)
import System.Random.Shuffle qualified as Random

data Suit = Diamond | Clubs | Hearts | Spades
    deriving (Read, Eq, Enum, Bounded, Generic)

instance Show Suit where
    show :: Suit -> String
    show = \case
        Diamond -> "♦️"
        Clubs -> "♣️"
        Hearts -> "♥️"
        Spades -> "♠️"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Read, Eq, Ord, Enum, Bounded, Generic)

instance Show Rank where
    show :: Rank -> String
    show = show . value

-- fixme: this should be game specific
value :: Rank -> Int
value = succ . fromEnum

data Card = Card Suit Rank
    deriving (Read, Eq, Generic)

instance Show Card where
    show :: Card -> String
    show (Card suit rank) = show suit ++ " " ++ show rank

newtype Deck = Deck (Pile 52)
    deriving (Show, Read, Eq, Generic)

type Pile n = Sized.Vector n Card
type UnsizedPile = Unsized.Vector Card

regularDeck :: Deck
regularDeck = Deck $ Sized.concatMap cardsOfSuit suits

cardsOfSuit :: Suit -> Sized.Vector 13 Card
cardsOfSuit suit = Sized.map (Card suit) ranks

cardsOfRank :: Rank -> Sized.Vector 4 Card
cardsOfRank rank = Sized.map (`Card` rank) suits

suits :: Sized.Vector 4 Suit
suits = Sized.iterateN succ minBound

ranks :: Sized.Vector 13 Rank
ranks = Sized.iterateN succ minBound

newtype Hand n = Hand (Sized.Vector n Card)
    deriving (Show, Read, Eq)

shuffledDeck :: (RandomGen g) => g -> Deck
shuffledDeck g =
    let Deck cards = regularDeck
        cards' = shuffle cards g
     in Deck cards'

shuffle :: (RandomGen g, KnownNat n) => Pile n -> g -> Pile n
shuffle cards =
    -- todo: write a shuffle for sized vectors to get rid of this mess and random-shuffle
    fromJust
        . Sized.fromList
        . Random.shuffle' (Sized.toList cards) (length cards)
