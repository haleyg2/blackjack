-- Server.hs
-- Run this on the host machine (Player 1).

module Main where

import Network.Socket
import System.IO
import Data.List (intercalate)
import GameLogic

port :: Int
port = 8080

-- ── Display helpers ───────────────────────────────────────────────────────────

showCard :: Card -> String
showCard (Card rank suit) = show rank ++ " of " ++ show suit

showHand :: Hand -> String
showHand []   = "(empty)"
showHand [c]  = showCard c ++ "  (Score: " ++ show (handScore [c]) ++ ")"
showHand hand = intercalate ", " (map showCard (init hand))
             ++ " and " ++ showCard (last hand)
             ++ "  (Score: " ++ show (handScore hand) ++ ")"

displayState :: Bool -> GameState -> IO ()
displayState final gs = do
  putStrLn "\n================================"
  if final
    then putStrLn $ "Dealer  : " ++ showHand (dealerHand gs)
    else putStrLn $ "Dealer  : " ++ showCard (head (dealerHand gs)) ++ "  [hidden]"
  putStrLn $ "You (P1): " ++ showHand (playerOneHand gs)
  putStrLn $ "Player 2: " ++ showHand (playerTwoHand gs)
  putStrLn "================================"

displayOutcome :: GameState -> IO ()
displayOutcome gs = case outcome gs of
  Nothing       -> return ()
  Just (p1, p2) -> do
    putStrLn "\n=== RESULTS ==="
    putStrLn $ "Player 1 (you): " ++ showResult p1
    putStrLn $ "Player 2:       " ++ showResult p2
  where
    showResult PlayerWins = "WIN!"
    showResult DealerWins = "Dealer wins."
    showResult Push       = "Push (tie)."

-- ── Networking setup ──────────────────────────────────────────────────────────

main :: IO ()
main = withSocketsDo $ do
  putStrLn "=== BLACKJACK SERVER ==="
  putStrLn $ "Waiting for Player 2 on port " ++ show port ++ "..."
  sock <- listenOn port
  (conn, _) <- accept sock
  putStrLn "Player 2 connected!\n"
  h <- socketToHandle conn ReadWriteMode
  hSetBuffering h LineBuffering
  hSetBuffering stdout LineBuffering
  gameLoop h
  hClose h
  close sock

listenOn :: Int -> IO Socket
listenOn p = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) Nothing (Just (show p))
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 5
  return sock

-- ── Game loop ─────────────────────────────────────────────────────────────────

gameLoop :: Handle -> IO ()
gameLoop h = do
  gs <- initGame
  displayState False gs
  hPutStrLn h (show gs)
  gs1 <- p1Turn h gs
  gs2 <- p2Turn h gs1
  displayState True gs2
  displayOutcome gs2
  putStr "\nPlay again? [Y/N] > "
  answer <- getLine
  hPutStrLn h answer
  case answer of
    "Y" -> gameLoop h
    _   -> putStrLn "Thanks for playing!"

-- ── Player turns ─────────────────────────────────────────────────────────────

p1Turn :: Handle -> GameState -> IO GameState
p1Turn h gs = do
  putStrLn "\n--- Your turn (Player 1) ---"
  putStrLn $ "Your hand: " ++ showHand (playerOneHand gs)
  putStr "[H] Hit  [S] Stand > "
  raw <- getLine
  let input = filter (/= '\r') raw
  case input of
    "H" -> do
      let gs' = playerHit PlayerOne gs
      hPutStrLn h (show gs')
      putStrLn $ "Drew: " ++ showCard (last (playerOneHand gs'))
      if isBust (playerOneHand gs')
        then do putStrLn "You busted!"; return gs'
        else p1Turn h gs'
    "S" -> do
      let gs' = playerStand PlayerOne gs
      hPutStrLn h (show gs')
      putStrLn "You stand."
      return gs'
    _ -> do putStrLn "Enter H or S."; p1Turn h gs

p2Turn :: Handle -> GameState -> IO GameState
p2Turn h gs = do
  putStrLn "\nWaiting for Player 2..."
  raw <- hGetLine h
  let action = filter (/= '\r') raw
  case action of
    "HIT" -> do
      let gs' = playerHit PlayerTwo gs
      putStrLn $ "Player 2 drew: " ++ showCard (last (playerTwoHand gs'))
      if isBust (playerTwoHand gs')
        then do
          putStrLn "Player 2 busted!"
          let gs'' = dealerPlay gs'
          hPutStrLn h (show gs'')
          return gs''
        else do
          hPutStrLn h (show gs')
          p2Turn h gs'
    "STAND" -> do
      putStrLn "Player 2 stands."
      let gs' = playerStand PlayerTwo gs
      hPutStrLn h (show gs')
      return gs'
    _ -> p2Turn h gs