module IdiotsDelight where

import Cards
import System.Random

data Game = Game
            { pile :: [Card]
            , hand :: [Card]
            }

newGame :: [Card] -> Game
newGame cards = Game cards []

pullFromPile :: Game -> Game
pullFromPile (Game p h) = Game (drop 1 p) ((head p):h)

reduceHand :: Game -> Game
reduceHand game@(Game p (c1:_:_:c4:cs))
  | getRank c1 == getRank c4 = Game p cs
  | getSuit c1 == getSuit c4 = Game p (c1:c4:cs)
  | otherwise = pullFromPile game

doTurn :: Game -> Game
doTurn game@(Game _ h)
  | length h < 4 = pullFromPile game
  | otherwise = reduceHand game

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
