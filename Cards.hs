module Cards where

import Data.Char (chr)
import qualified Data.Text as T
import System.Random

data Suit = Club
          | Diamond
          | Heart
          | Spade
            deriving (Eq, Enum, Show, Bounded)

data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
            deriving (Eq, Enum, Show, Bounded)

data Card = Card Rank Suit
            deriving (Eq, Show)

-- |Get the suit of a 'Card'
getSuit :: Card -> Suit
getSuit (Card _ s) = s

-- |Get the rank of a 'Card'
getRank :: Card -> Rank
getRank (Card r _) = r

-- Printing Utilities
--

-- |Get the unicode offset for the beginning (Ace) of each suit.
unicodeSuitOffset :: Suit -> Int
unicodeSuitOffset Club = 127185
unicodeSuitOffset Diamond = 127169
unicodeSuitOffset Heart = 127153
unicodeSuitOffset Spade = 127137

-- |Get the unicode offset of a rank. Add this to the suit unicode offset to obtain
-- the proper card.
unicodeRankOffset :: Rank -> Int
unicodeRankOffset rank
  | rank == Queen || rank == King = fromEnum rank + 1 -- Skip the weird 'Knight' unicode card
  | otherwise = fromEnum rank

-- |Get the unicode string for a playing card.
showCard :: Card -> String
showCard (Card rank suit) =
  -- Append blank so that cards show properly on a terminal. They tend to run together without this.
  (++" ") . T.unpack . T.singleton . chr $ (unicodeSuitOffset suit) + (unicodeRankOffset rank)

-- |Get the unicode string for a list of playing cards.
showCards :: [Card] -> String
showCards [] = ((++" ") . T.unpack . T.singleton) '\127136'
showCards cards = concatMap showCard cards

-- |Create an unshuffled deck of cards
deck :: [Card]
deck = [Card rank suit | suit <- [Club ..], rank <- [Ace ..]]

-- Code taken from https://github.com/fffej/haskellprojects/blob/master/cards/Klondike.hs
shuffle' :: (RandomGen g) => g -> [Card]
shuffle' g = fst (mix deck (randomRs (True, False) g))
  where mix [ ] r0 = ([ ], r0)
        mix [x] r0 = ([x], r0)
        mix  xs r0 = let (ys, zs, r1)  = cut xs r0 [] []
                         (cs,     r2)  = mix ys r1
                         (ds,     r3)  = mix zs r2
                     in (cs++ds, r3)
        cut [] rs ys zs = (ys, zs, rs)
        cut (x:xs) (r:rs) ys zs = if r then cut xs rs (x:ys) zs
                                  else cut xs rs ys (x:zs)
