module IdiotsDelight where

import Cards
import System.Random

{-|
  A game of Idiot's Delight is modeled as the current state of
  the pile (undrawn cards) and the cards in the player's hand.
-}
data Game = Game
            { pile :: [Card]
            , hand :: [Card]
            }

-- |Create a new game from a list of 'Card's
newGame :: [Card] -> Game
newGame cards = Game cards []

-- |Given a game state, return a new game state where a card has been
-- drawn from the pile and placed in the player's hand.
pullFromPile :: Game -> Game
pullFromPile (Game p h) = Game (drop 1 p) ((head p):h)

-- |Given a game state, return a new game state where the player's
-- hand has been reduced once. If the player's hand cannot be reduced,
-- then a card is pulled from the pile.
reduceHand :: Game -> Game
reduceHand game@(Game p (c1:_:_:c4:cs))
  | getRank c1 == getRank c4 = Game p cs
  | getSuit c1 == getSuit c4 = Game p (c1:c4:cs)
  | otherwise = pullFromPile game

-- |Play one turn of the game. Either the hand will be reduced or a
-- card will be pulled from the pile.
doTurn :: Game -> Game
doTurn game@(Game _ h)
  | length h < 4 = pullFromPile game
  | otherwise = reduceHand game

-- |Play a game of Idiot's Delight until a win/loss state is achieved.
playGame :: Game -> IO ()
playGame (Game [ ] [ ]) = putStrLn "You Win!!!"
playGame (Game [ ]  _ ) = putStrLn "You Lose :-("
playGame game@(Game _ h) = do
  putStrLn . showCards . reverse $ h
  getChar
  playGame (doTurn game)

main :: IO ()
main = do
  putStrLn "Welcome to Idiot's Delight! Good Luck!"
  putStrLn "Press <Return> to play a turn\n"
  g <- getStdGen
  playGame . newGame . shuffle' $ g
