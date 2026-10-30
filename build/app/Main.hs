module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center       (hCenter)
import Control.Monad.IO.Class     (liftIO)
import qualified Graphics.Vty as V

import GameLogic

-------App state ────────────────────────────────────────────────────────────────

-- Wraps GameState with a status message for the UI
data AppState = AppState
  { appGame    :: GameState
  , appMessage :: String
  }

type Name = ()

-- ── String helpers ────────────────────────────────────────────────────────────

rankStr :: Rank -> String
rankStr Two   = "2";  rankStr Three = "3";  rankStr Four  = "4"
rankStr Five  = "5";  rankStr Six   = "6";  rankStr Seven = "7"
rankStr Eight = "8";  rankStr Nine  = "9";  rankStr Ten   = "10"
rankStr Jack  = "J";  rankStr Queen = "Q";  rankStr King  = "K"
rankStr Ace   = "A"

suitChar :: Suit -> String
suitChar Clubs    = "\9827"  -- ♣
suitChar Diamonds = "\9830"  -- ♦
suitChar Hearts   = "\9829"  -- ♥
suitChar Spades   = "\9824"  -- ♠

-- ── Card widgets ─────────────────────────────────────────────────────────────

-- A playing card rendered as a small bordered box:
--  ┌───┐
--  │A  │
--  │ ♠ │
--  │  A│
--  └───┘
cardWidget :: Card -> Widget Name
cardWidget (Card rank suit) =
  withBorderStyle unicodeBold $
  border $
  vBox [ str (r ++ replicate (3 - length r) ' ')
       , str (" " ++ suitChar suit ++ " ")
       , str (replicate (3 - length r) ' ' ++ r)
       ]
  where r = rankStr rank


hiddenCard :: Widget Name
hiddenCard =
  withBorderStyle unicodeBold $
  border $
  vBox [str "?  ", str " ? ", str "  ?"]


renderHand :: Bool -> Hand -> Widget Name
renderHand _       []     = str "(no cards)"
renderHand showAll (c:cs) =
  hBox $ cardWidget c : map (\x -> if showAll then cardWidget x else hiddenCard) cs

-- ── Drawing ─────────────────────────────────────────────────────────────────────

drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    gs       = appGame st
    gameOver = case outcome gs of { Just _ -> True; Nothing -> False }

    ui = vBox
      [ titleBar
      , hBorder
      , dealerSection
      , hBorder
      , playerSection PlayerOne
      , hBorder
      , playerSection PlayerTwo
      , hBorder
      , padLeftRight 1 $ withAttr msgAttr  $ str (appMessage st)
      , padLeftRight 1 $ withAttr ctrlAttr $ str "[H] Hit   [S] Stand   [N] New Game   [Q] Quit"
      ]

    titleBar = withAttr titleAttr $ hCenter $ str " \9824  BLACKJACK  \9824 "

    dealerSection =
      padLeft (Pad 1) $ vBox
        [ withAttr headerAttr $ str "DEALER"
        , renderHand gameOver (dealerHand gs)
        , if gameOver
            then str $ "Score: " ++ show (handScore (dealerHand gs))
            else str   "Score: ?"
        ]

    playerSection p =
      let hand   = if p == PlayerOne then playerOneHand gs else playerTwoHand gs
          label  = if p == PlayerOne then "PLAYER 1" else "PLAYER 2"
          isTurn = currentTurn gs == p && not gameOver
      in padLeft (Pad 1) $ vBox
           [ if isTurn
               then withAttr activeAttr $ str (label ++ "  \8592 your turn")
               else withAttr headerAttr $ str label
           , hBox (map cardWidget hand)
           , str $ "Score: " ++ show (handScore hand)
           ]

-- ── Attributes ───────────────────────────────────────────────────────────────

titleAttr, headerAttr, activeAttr, msgAttr, ctrlAttr :: AttrName
titleAttr  = attrName "title"
headerAttr = attrName "header"
activeAttr = attrName "active"
msgAttr    = attrName "msg"
ctrlAttr   = attrName "ctrl"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (titleAttr,  V.white `on` V.blue)
  , (headerAttr, fg V.yellow)
  , (activeAttr, fg V.green)
  , (msgAttr,    fg V.cyan)
  , (ctrlAttr,   fg V.white)
  ]

-- ── Event handling ────────────────────────────────────────────────────────────

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey key _)) = do
  st <- get
  let gs     = appGame st
      isOver = case outcome gs of { Just _ -> True; Nothing -> False }
      turn   = currentTurn gs
  case key of
    V.KChar 'q' -> halt
    V.KChar 'Q' -> halt
    V.KChar 'n' -> liftIO initGame >>= \g -> put (AppState g (statusMsg g))
    V.KChar 'N' -> liftIO initGame >>= \g -> put (AppState g (statusMsg g))
    V.KChar c | c `elem` "hH" && not isOver && turn /= Dealer ->
      applyAction (playerHit turn) st
    V.KChar c | c `elem` "sS" && not isOver && turn /= Dealer ->
      applyAction (playerStand turn) st
    _ -> return ()
handleEvent _ = return ()

-- Apply a game action, then auto-run the dealer if it becomes the dealer's turn
applyAction :: (GameState -> GameState) -> AppState -> EventM Name AppState ()
applyAction f st = do
  let gs' = f (appGame st)
      -- If P2 busted, currentTurn flips to Dealer but outcome is still Nothing
      -- playerStand handles this internally, but playerHit doesn't — catch it here
      gs'' = case (currentTurn gs', outcome gs') of
               (Dealer, Nothing) -> dealerPlay gs'
               _                 -> gs'
  put $ AppState gs'' (statusMsg gs'')

statusMsg :: GameState -> String
statusMsg gs = case outcome gs of
  Just (p1, p2) ->
    "Game over!  P1: " ++ fmt p1 ++ "   |   P2: " ++ fmt p2 ++
    "   -- Press N for a new game."
  Nothing -> case currentTurn gs of
    PlayerOne -> "Player 1 -- Hit [H] or Stand [S]?"
    PlayerTwo -> "Player 2 -- Hit [H] or Stand [S]?"
    Dealer    -> "Dealer playing..."
  where
    fmt PlayerWins = "WIN!"
    fmt DealerWins = "lose"
    fmt Push       = "push"

-- ── App wiring ────────────────────────────────────────────────

app :: App AppState () Name
app = App
  { appDraw         = drawUI
  , appHandleEvent  = handleEvent
  , appAttrMap      = const theMap
  , appStartEvent   = return ()
  , appChooseCursor = neverShowCursor
  }

main :: IO ()
main = do
  gs <- initGame
  _ <- defaultMain app (AppState gs (statusMsg gs))
  return ()