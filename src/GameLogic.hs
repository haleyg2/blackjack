module GameLogic where

import System.Random (randomRIO)
import Data.List (sortBy)

--Main data Types besides Players
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq, Read)

data Rank = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Read)

data Card = Card Rank Suit
  deriving (Show, Eq, Read)



type Deck = [Card]
type Hand = [Card]

--52-card deck
fullDeck :: Deck
fullDeck = [Card rank suit | suit <- [Clubs, Diamonds, Hearts, Spades],
                              rank <- [Two .. Ace]]

--The value of each card, 
--since Ace can have two values its defualted to 11 for now
cardValue :: Card -> Int
cardValue (Card rank _) = case rank of
  Two   -> 2
  Three -> 3
  Four  -> 4
  Five  -> 5
  Six   -> 6
  Seven -> 7
  Eight -> 8
  Nine  -> 9
  Ten   -> 10
  Jack  -> 10
  Queen -> 10
  King  -> 10
  Ace   -> 11

--The Score of Given Hand
handScore :: Hand -> Int
handScore hand =
  let total = sum (map cardValue hand)
      aces  = length (filter isAce hand)
  in reduceAces total aces
  where
    isAce (Card Ace _) = True
    isAce _            = False
    reduceAces total 0    = total
    reduceAces total aces
      | total > 21 = reduceAces (total - 10) (aces - 1)
      | otherwise  = total

--Checks for bust
isBust :: Hand -> Bool
isBust hand = handScore hand > 21
--checks for Black Jack
isBlackjack :: Hand -> Bool
isBlackjack hand = length hand == 2 && handScore hand == 21

--Shuffles the Deck of cards
shuffle :: Deck -> IO Deck
shuffle [] = return []
shuffle deck = do
  i <- randomRIO (0, length deck - 1)
  let card = deck !! i
      rest = take i deck ++ drop (i + 1) deck
  shuffled <- shuffle rest
  return (card : shuffled)

--Deals one card off the top
deal :: Deck -> (Card, Deck)
deal (x:xs) = (x, xs)
deal []     = error "Empty deck!"
-------------GAMESTATE--------------------
-- Who is currently playing data type
data Player = PlayerOne | PlayerTwo | Dealer
  deriving (Show, Eq, Read)
-- The outcomes of the game
data Outcome = PlayerWins | DealerWins | Push
  deriving (Show, Eq, Read)

-- The full game state
data GameState = GameState
  { deck          :: Deck
  , playerOneHand :: Hand
  , playerTwoHand :: Hand
  , dealerHand    :: Hand
  , currentTurn   :: Player
  , outcome       :: Maybe (Outcome, Outcome)  -- (P1 result, P2 result)
  } deriving (Show, Read)

-- Deal an initial card to a hand, returning updated hand and deck
drawCard :: Hand -> Deck -> (Hand, Deck)
drawCard hand d =
  let (card, rest) = deal d
  in (hand ++ [card], rest)

-- Set up a fresh game: shuffle and deal 2 cards each
initGame :: IO GameState
initGame = do
  shuffled <- shuffle fullDeck
  let (p1H,  d1) = drawCard [] shuffled
      (p2H,  d2) = drawCard [] d1
      (dH,   d3) = drawCard [] d2
      (p1H2, d4) = drawCard p1H d3
      (p2H2, d5) = drawCard p2H d4
      (dH2,  d6) = drawCard dH  d5
  return GameState
    { deck          = d6
    , playerOneHand = p1H2
    , playerTwoHand = p2H2
    , dealerHand    = dH2
    , currentTurn   = PlayerOne
    , outcome       = Nothing
    }

-- Player hits: draw a card, check for bust
playerHit :: Player -> GameState -> GameState
playerHit PlayerOne gs =
  let (newHand, newDeck) = drawCard (playerOneHand gs) (deck gs)
      bust = isBust newHand
  in gs { playerOneHand = newHand
        , deck          = newDeck
        , currentTurn   = if bust then PlayerTwo else PlayerOne
        }
playerHit PlayerTwo gs =
  let (newHand, newDeck) = drawCard (playerTwoHand gs) (deck gs)
      bust = isBust newHand
  in gs { playerTwoHand = newHand
        , deck          = newDeck
        , currentTurn   = if bust then Dealer else PlayerTwo
        }
playerHit Dealer gs = gs  -- shouldn't happen, dealer is automated

-- Dealer plays: must hit until score is 17 or more
dealerPlay :: GameState -> GameState
dealerPlay gs
  | handScore (dealerHand gs) >= 17 = determineOutcome gs
  | otherwise =
      let (newHand, newDeck) = drawCard (dealerHand gs) (deck gs)
      in dealerPlay gs { dealerHand = newHand, deck = newDeck }

-- Figure out who won
determineOutcome :: GameState -> GameState
determineOutcome gs =
  let p1Result = calcResult (playerOneHand gs)
      p2Result = calcResult (playerTwoHand gs)
  in gs { outcome = Just (p1Result, p2Result), currentTurn = Dealer }
  where
    calcResult pHand
      | isBust pHand           = DealerWins
      | isBust (dealerHand gs) = PlayerWins
      | handScore pHand > handScore (dealerHand gs) = PlayerWins
      | handScore pHand < handScore (dealerHand gs) = DealerWins
      | otherwise              = Push

  -- Player stands: hand the turn over to the dealer
playerStand :: Player -> GameState -> GameState
playerStand PlayerOne gs = gs { currentTurn = PlayerTwo }
playerStand PlayerTwo gs = dealerPlay gs { currentTurn = Dealer }
playerStand Dealer    gs = gs