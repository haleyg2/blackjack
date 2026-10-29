-- Client.hs
-- Before running, open a terminal and set up the SSH tunnel:
--   ssh -N -L 8080:localhost:8080 haley@<server-ip>
-- Then run this and it connects via 127.0.0.1 automatically.

module Main where

import Network.Socket
import System.IO
import Data.List (intercalate)
import GameLogic

port :: String
port = "8080"

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
  putStrLn $ "Player 1: " ++ showHand (playerOneHand gs)
  putStrLn $ "You (P2): " ++ showHand (playerTwoHand gs)
  putStrLn "================================"

displayOutcome :: GameState -> IO ()
displayOutcome gs = case outcome gs of
  Nothing       -> return ()
  Just (p1, p2) -> do
    putStrLn "\n=== RESULTS ==="
    putStrLn $ "Player 1:       " ++ showResult p1
    putStrLn $ "Player 2 (you): " ++ showResult p2
  where
    showResult PlayerWins = "WIN!"
    showResult DealerWins = "Dealer wins."
    showResult Push       = "Push (tie)."

-- ── Networking setup ──────────────────────────────────────────────────────────

main :: IO ()
main = withSocketsDo $ do
  putStrLn "=== BLACKJACK CLIENT ==="
  putStrLn "Connecting to 127.0.0.1:8080..."
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
  sock <- socket (addrFamily addr) Stream defaultProtocol
  connect sock (addrAddress addr)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  hSetBuffering stdout LineBuffering
  putStrLn "Connected!\n"
  gameLoop h
  hClose h

-- ── Game loop ─────────────────────────────────────────────────────────────────

gameLoop :: Handle -> IO ()
gameLoop h = do
  clientRound h
  answer <- hGetLine h
  case answer of
    "Y" -> do putStrLn "\nNew game starting..."; gameLoop h
    _   -> putStrLn "Thanks for playing!"

clientRound :: Handle -> IO ()
clientRound h = do
  line <- hGetLine h
  let gs = read line :: GameState
  case outcome gs of
    Just _ -> do
      displayState True gs
      displayOutcome gs
    Nothing -> do
      displayState False gs
      case currentTurn gs of
        PlayerTwo -> do
          action <- getAction
          hPutStrLn h action
          clientRound h
        _ -> clientRound h

getAction :: IO String
getAction = do
  putStrLn "\n--- Your turn (Player 2) ---"
  putStr "[H] Hit  [S] Stand > "
  raw <- getLine
  let input = filter (/= '\r') raw
  case input of
    "H" -> return "HIT"
    "S" -> return "STAND"
    _   -> do putStrLn "Enter H or S."; getAction