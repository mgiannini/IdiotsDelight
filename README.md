# Introduction
The most popular solitaire game in my family was always "Idiot's Delight". Or, at least that's what we called it. Apparently the rules we played by (and implemented in this code) are different than those I found online.

# Rules
Here are the rules for my version of Idiot's Delight. The game begins with a shuffled deck of cards. You draw one card from the **back** of the deck and place it in your *hand*. Every time you draw a card, apply the *reduction* rules as outlined below until you can no longer reduce your hand, then draw another card.

**Always** put the most recently drawn card from the deck on *top* of your hand. Your hand is essentially a stack.

## Reduction Rules
Reduction rules are **always** applied starting with the most recently drawn cards, working backwards as your reduce (top to bottom).

1. If you have less than four cards in your hand, you cannot reduce anything. Draw another card from the deck.

2. If the card on top of your hand has the same *rank* as the fourth card from the top of your hand, then remove the top four cards. For example:

 1. &#127137; &#127173; &#127153; &#127190; &#127138; &#127185; (*top*)
 2. The first and fourth cards from the top are both aces, so remove the top four cards.
 3. &#127137; &#127173; (*top*)

3. If the top card in your hand is the same suit as the fourth card from the top, then remove the second and third cards from the top. Apply the reduction rules again if possible.

 1. &#127137; &#127173; &#127138; &#127190; &#127138; &#127149; (*top*)
 2. The first and fourth cards are both spades, so remove the six of clubs and two of spades.
 3. &#127137; &#127173; &#127138; &#127149; (*top*)
 4. The first and fourth cards from the top are still both spades, so re-apply the rule.
 5. &#127137; &#127149; (*top*)

## Winning
You win if you have an empty hand after drawing the last card from the deck and applying the reduction rules. If you have any cards left in your hand, you lose.
