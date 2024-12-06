module Main where

import Gambit
import System.IO
import System.Environment
import Data.Maybe
import System.Console.GetOpt

-- Default depth for move calculation
depth :: Int
depth = 4

-- Define the options
data Flag = Help | Winner | Depth Int | Move String | Verbose | Interactive | Unknown
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "Display this help message",
    Option ['w'] ["winner"] (NoArg Winner) "Show the best move using exhaustive search",
    Option ['d'] ["depth"] (ReqArg (Depth . read) "<num>") "Specify a cutoff depth for move calculation",
    Option ['m'] ["move"] (ReqArg Move "<move>") "Make a move and display the resulting board",
    Option ['v'] ["verbose"] (NoArg Verbose) "Display move quality (win, lose, tie, or rating)",
    Option ['i'] ["interactive"] (NoArg Interactive) "Play a new game interactively"
  ]

-- Simplified usage function
usage :: String -> IO ()
usage prog = putStrLn $ usageInfo ("Usage: " ++ prog ++ " [options]") options

-- Main function
main :: IO ()
main = do
  args <- getArgs
  let (opts, _, errs) = getOpt RequireOrder options args
  if not (null errs)
    then do
      mapM_ putStrLn errs
      usage "program" -- Replace "program" with the actual name
    else if Help `elem` opts
      then usage "program" -- Replace "program" with the actual name
      else handleFlags opts

-- Handle different flags
handleFlags :: [Flag] -> IO ()
handleFlags opts
  | Winner `elem` opts = do
      putStrLn "Enter the game file name for analysis: "
      hFlush stdout
      gameFile <- getLine
      game <- loadGame gameFile
      putBestMove game
  | otherwise = putStrLn $ "Flags provided: " ++ show opts-- Other functions to load the game and output the best move
writeGame :: Game -> FilePath -> IO ()
writeGame game file = do
    writeFile file (showGame game)

loadGame :: FilePath -> IO Game
loadGame file = do
    gameStr <- readFile file
    return (readGame gameStr)

putBestMove :: Game -> IO ()
putBestMove game = do --print the outcome of whoWillWin here too
    case bestMove game of -- This int is the depth, change as needed
        Just move -> putStrLn (showMove move ++ ". The expected outcome is " ++ case whoWillWin game of
            (Win White) -> "a win for white."
            (Win Black) -> "a win for black."
            Tie -> "a tie." )
        Nothing -> putStrLn "There is no best move here."

{-
maxDepth = 4

readMove :: Game -> String -> Maybe Game
readMove currentGame input = let
    args = words input
    pos1 = strToPos (head args)
    pos2 = strToPos (head (tail args))
    promoPiece = if length args == 3 && isJust (strToPiece (last args)) then Just (fromJust (strToPiece (last args))) else Nothing 

    in if length args > 3 || length args < 2 || isNothing pos1 || isNothing pos2 then Nothing else
        if length args == 3 && isJust promoPiece then Just (promotePiece currentGame (fromJust pos1) (fromJust pos2) (fromJust promoPiece))
        else Just (quickMove currentGame (fromJust pos1) (fromJust pos2))


startGame = do
    whiteMain maxDepth initialGame


whiteMain :: Integer -> Game -> IO ()
whiteMain num currentGame = do
    displayBoard currentGame White
    --flush the output buffer so the board prints before we try to read input
    putStrLn "What is your move? (Format: pos, pos OR pos, pos, pieceType if promoting)"
    hFlush stdout
    move <- getLine
    let newGame = readMove currentGame move in
        case newGame of 
            Nothing -> do 
                putStrLn "Invalid move. Try again."
                whiteMain num currentGame
            Just game ->
        --check if you won on this turn. If not, then the bot plays
                case printWinner (getWinner game) of 
                    --Nobody has won yet, the bot will play
                    Nothing -> let 
                        aiMove = bestMove ((1 + fst currentGame), Black, getThd game, getFrth game)
                        --blackGame is the game state AFTER black moves
                        blackGame = makeMove ((1 + fst currentGame), Black, getThd game, getFrth game) aiMove
                        maybeWinner = getWinner blackGame
                        in if isNothing maybeWinner 
                            then putStrLn (printWinner maybeWinner)
                            else whiteMain ((1 + fst blackGame), White, getThd blackGame, getFrth blackGame)
                        --have the ai make a move, check if anyone has won, if not, then call whiteMain
                    --Somebody won, or it's a tie
                    Just _ -> putStrLn _-}


    


    --main (num - 1)
    


printWinner :: Maybe Winner -> Maybe String
printWinner Nothing = Nothing
printWinner (Just (Win Black)) = Just "Black wins!"
printWinner (Just (Win White)) = Just "White wins!"
printWinner (Just Tie) = Just "It's a tie."
    

{-
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣤⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⠿⠿⣿⣿⣿⣿⣿⢿⣿⣿⣿⣶⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣶⣿⣿⢿⣯⣟⡿⣾⡿⠋⠁⣿⣿⠀⢀⣿⣇⠈⠻⣿⣿⡾⣽⣻⣟⣿⣿⣦⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣾⣿⣿⢯⣟⡿⣾⣽⣿⡟⠁⠀⣼⣿⣿⣿⣿⣿⣿⣆⠀⠈⣿⣿⣟⣷⣻⢾⣽⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣿⣿⡿⣽⣻⣞⣿⣳⣿⣿⡆⠀⣼⣿⡏⠙⠿⠿⠀⢻⣿⣧⠀⣼⣿⣿⢾⡽⣟⣾⡽⣿⣿⣷⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣿⢯⡿⣽⣳⡿⣾⣽⣻⣿⣷⡈⢿⣿⠁⣀⠀⢀⣄⠀⣿⣿⣸⣿⣿⣟⣯⡿⣯⡷⣟⣿⣻⣿⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣯⢿⣽⣻⣷⣿⣷⣿⡽⣿⣿⣿⣶⣷⣴⣿⣷⣶⣿⣷⣿⣿⣿⣿⣿⣿⣾⣿⣷⣿⣻⣞⡷⣿⣿⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣻⣞⣯⡷⣟⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣯⣿⣽⣻⣿⣿⣿⣽⣻⢿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣟⣧⢿⣻⢷⣻⣧⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣿⣟⣿⣾⣿⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⡿⣞⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣯⡐⣉⠛⣿⣍⠋⡍⣻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣽⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣯⣿⣿⣿⣿⣿⣿⣿⣿⠿⣿⣿⣿⣿⣧⠄⠒⣈⠣⠘⢸⣿⣿⣿⣿⣿⠿⣿⣿⣿⣿⣿⣿⣿⣽⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⡿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣿⠈⠐⠀⠂⠁⠚⣿⣿⣿⣟⢻⣿⣭⠉⠁⠌⠸⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣼⣿⣿⣿⣿⣿⡇⠀⠀⠀⠠⠏⠁⢠⣿⣿⣿⣿⠀⠀⠀⠀⠀⣼⣿⡿⣿⣿⣦⠹⣿⡆⠀⠀⢀⣿⣿⣿⣿⣿⣿⣯⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣦⣤⣾⡿⢉⡁⠀⠈⣿⣿⣇⠀⠀⠀⠈⠀⠀⣿⣿⣧⣼⣿⠀⠀⠀⠀⠰⢿⣿⣦⣿⣿⣿⠀⣿⡇⠀⠀⢪⣿⣿⡟⠋⢀⣈⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣾⣿⣿⣿⣿⣷⣾⣿⠷⠒⣿⣿⣿⡆⠀⠀⠀⠀⠀⢹⣿⣿⠟⠋⠀⠀⠀⠀⠀⠈⠙⠿⣿⣿⣯⠀⣿⠇⠀⠀⢸⣿⣿⣧⠐⡨⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣿⣿⠏⠀⣠⣤⡀⠀⠀⠈⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠲⠘⠛⠃⠀⠀⢀⣶⣴⣿⣿⣿⣿⣷⡄⢻⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣤⣶⣄⣠⣿⣿⣿⣿⣿⣿⣾⣯⠛⣿⣿⣿⠀⢈⣿⣿⠀⢰⣿⣿⣷⣶⣥⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣤⣴⣿⣿⣿⣽⣿⣿⣿⣿⡇⣼⣿⡿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣶⣿⣿⣿⣿⣿⣿⣟⣭⣽⣿⡿⠟⠁⢀⣾⣿⠃⠀⠀⠉⠁⠀⢻⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⡟⠉⠛⣿⣿⣷⣿⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⣀⣴⣾⣿⣿⣿⣿⣿⣾⣿⣿⣿⡿⠉⠀⠀⣠⣾⡟⠁⣾⣶⣤⡀⠀⠀⠈⠻⣿⣿⣿⣿⣿⣷⣆⡀⠀⠀⠀⠀⠀⠀⠀⠀⢐⣴⣿⣿⣿⣿⣿⣿⡟⠀⠀⢀⣿⣿⣿⡿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⠀⠀⣠⣾⡿⠋⠀⣾⣿⣿⣿⣿⣧⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣦⣤⣤⣤⣠⣤⣤⣴⣿⣿⣿⣿⣿⠟⠁⠀⠀⠀⠀⣼⣿⡿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⢀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣤⠋⠀⠀⢀⣼⣿⠏⠀⣀⣠⣿⣿⣦⣀⡈⢿⣧⡄⠀⠀⠀⠀⠈⠙⠛⠋⢿⣿⣿⣿⡿⣿⣿⣿⣿⠿⢹⡟⠉⠁⠀⠀⠀⡀⣤⣿⡟⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⣰⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⣀⣰⣿⡿⠃⠀⠀⣿⣿⣿⣿⣯⡽⢿⣦⢿⣿⣦⣄⠀⠀⠀⠀⠀⠀⠀⠀⠉⢻⣖⠉⠛⡉⠂⠀⠀⠀⠈⢠⠀⣀⣥⣾⡿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡄⠉⢿⣿⣿⣿⣿⣿⡿⠁⠀⠀⣸⣿⣿⣿⣿⣿⣿⣄⡈⢉⠿⣿⣿⣿⣤⣀⠄⠀⠀⠀⠀⠉⠀⠿⠏⠀⠀⠁⠀⠀⠁⡀⣀⣧⣿⣿⣏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⣿⣿⣿⣿⣟⣿⣿⣿⣿⣿⣿⣧⡀⢿⣿⣿⣿⡏⠀⠀⠀⠀⣺⣿⣻⣿⣿⣿⣿⣿⣿⣼⢾⣿⣿⣿⣿⣿⣿⣤⣄⣀⠀⠀⠀⠀⠀⠐⣀⠠⣀⣶⣼⣿⣿⣿⣿⣿⣿⣷⣦⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢾⣿⣿⣟⣼⣿⣿⣿⣿⣿⣿⣿⣷⠈⣿⣿⣿⡇⠀⢠⣴⣿⣿⢛⣿⣿⣿⣿⣿⣿⣿⣿⣦⣀⢹⣿⣿⣯⣿⣿⣿⣿⣿⣿⣷⣿⣾⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣻⣿⣿⣷⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠠⣿⣿⣿⣿⡾⣿⣿⣿⣿⣿⣿⣿⣿⣼⣿⣿⣿⠇⠀⠹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡃⠈⠉⠁⠙⢿⣿⣯⢿⣽⣻⣟⡿⣿⢿⡿⣿⣻⣿⣿⣿⣿⣿⣿⣿⣿⣷⢯⣟⣿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢿⣿⣿⣿⣷⣙⠿⣿⣿⣿⣿⣟⣡⣿⣿⣿⣿⠃⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠛⠋⠀⠀⠀⠀⠀⢘⣿⣿⣿⣾⣷⣯⣿⣽⣿⣿⣿⣿⣿⣿⣿⢿⢿⣿⣿⣿⣿⣿⣻⢾⡽⣿⣿⣷⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⢹⣿⣿⣿⣿⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⠛⠀⣰⣿⡟⠻⢻⣿⣿⣿⣿⣿⣿⣿⠁⠀⠀⠀⠀⠀⠀⡀⣾⣿⣿⣛⡟⣿⢻⣟⡿⣻⢟⡿⣽⣻⣿⣯⠀⠀⠀⠛⣿⣿⣿⣿⢯⣿⣳⢿⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠈⠻⣿⣿⣿⣿⣿⡿⢿⣿⣿⣿⣿⣿⠁⠀⣼⣿⠉⠀⠠⠘⠷⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⢀⡁⣹⣿⣿⣳⡝⣾⡱⢯⣞⡽⣳⢯⣛⣷⣻⣿⣿⡀⠀⠀⠀⣸⣿⣿⣿⣿⣳⣿⣟⣾⣻⢿⣿⣶⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⣀⣼⣿⣿⣿⣿⣿⣤⡉⠿⣿⣿⡇⠀⣾⣿⣧⡀⠀⠀⠀⠀⠀⢿⡟⣿⣿⡟⠁⠀⠀⠀⠀⣿⣿⣿⣿⣯⢳⡽⣲⡝⣯⢞⡵⣯⢏⡿⣞⣳⢿⣿⣿⣦⣤⣶⣿⣿⣿⣿⣿⣷⣻⢿⣿⡽⣯⡿⣿⣿⣧⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠰⣿⣿⣿⣿⣿⣿⣿⣿⣷⣾⣿⣿⣷⣾⣿⡿⣹⣿⣿⣿⣷⣀⣀⣷⣼⣿⣿⣅⠀⠀⠀⠀⢸⣿⣿⢻⡵⣞⢯⡳⣇⠿⣜⡯⣽⢞⣯⣟⣭⣟⣻⣞⢿⣿⣿⣿⣿⢿⣟⣿⣿⣿⣿⣻⣞⣿⣽⣻⣽⣿⣿⣿⣦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⢀⣽⣿⣿⣼⣿⣿⣿⣿⣿⣸⣿⣿⣿⣿⣿⣷⣿⠟⠻⠿⢿⣿⡿⠿⢛⣯⠻⣿⣦⡀⠀⢀⣿⣿⢏⡷⣽⢮⢷⡹⣎⣿⡹⣞⣧⣟⡾⣞⡵⣯⢷⣞⡿⣞⣷⣻⢾⣟⡾⣿⣿⣿⣿⣿⣻⣞⣷⢯⣷⣿⣾⣿⠿⢿⣦⣄⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠨⢿⣿⣿⠿⣿⣿⣿⣿⣿⣿⣿⣽⣿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀⠀⠈⠡⠁⠚⣿⣿⣿⣿⣿⣟⡯⣞⢧⡟⣧⢿⣱⡗⣯⡽⡶⣯⡽⣏⡿⣽⣻⢾⣽⣻⢾⣽⣻⣞⡿⣽⣿⣿⣿⣿⣳⣿⣾⣿⠿⠋⠁⠀⠀⠀⢹⣿⣧⡀⠀⠀⠀⠀⠀⠀
⠀⠀⠐⠃⣿⣿⣿⣯⣿⣟⣿⣿⣵⣿⣿⣿⡿⠋⠉⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣟⣞⡷⣭⣟⡾⡽⣏⡾⣽⣳⣻⣽⣳⠿⣽⣻⢷⣯⣟⣾⣽⣻⣞⣷⣻⢿⣷⣿⣿⣿⣿⣿⣿⠟⠁⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣆⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣏⠐⡀⠄⡀⢀⠈⡑⢶⡄⠀⠀⠀⢀⣠⣿⡟⣿⣿⢷⣻⢾⣽⣳⢯⣽⣻⣽⣻⣗⣯⢷⣯⣟⣿⣳⢯⣟⣾⡽⣞⣷⡿⣽⣾⣟⣿⣾⣿⣿⢿⣿⣿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣷⣄⠀⠀⠀
⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣵⣮⣴⣡⣒⣠⣀⣬⣤⣴⣾⡿⠛⠛⣷⣿⣿⡿⣽⣻⡾⣽⣯⢷⣟⣾⣳⣟⡾⣟⣾⣽⡾⣽⣟⣿⢾⣟⣿⣯⣿⢿⣳⣿⣿⣿⣿⡧⠀⢻⣿⣴⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⣿⣆⠀⠀
⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠛⠛⠛⠛⠛⠛⠛⠛⠉⠉⠁⠀⠀⢸⣿⣿⣿⣹⡟⣷⣿⢻⣾⡟⣿⣾⣏⣿⣿⡟⣿⣾⣿⡟⣿⣾⣿⣿⢻⣾⣿⣿⣿⣷⣿⣿⣿⠁⠀⠀⣿⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣆⠀
⠀⠀⠀⠀⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠂⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⣿⣿⣿⣻⣽⣿⣻⣿⣯⣿⣿⢿⣾⣿⣾⣿⣟⣿⣾⣿⣿⣟⣿⣾⣿⣿⣯⣿⣾⣿⣿⣿⡟⠀⠀⢰⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⡄
⠀⠀⠀⠀⠹⣿⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠰⡀⢆⠰⡀⠤⣠⠄⠀⠀⠀⠀⣴⣿⣿⡿⣾⣿⣟⣿⣿⣻⣽⣿⢿⣿⣿⣯⣷⣿⣿⣿⣿⣽⣾⣿⣿⣟⣿⣾⣿⣿⣽⣾⣿⣿⡇⠀⠀⢸⣿⠀⠀⠀⣰⣤⣤⣄⡀⠀⠀⠀⠀⠀⠀⠀⣀⢹⣿⡇
⠀⠀⠀⠀⠀⠀⠀⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣯⣦⣷⣼⣦⣴⣿⣶⠄⠀⢀⣿⣿⡿⣽⣳⣟⡾⣿⣽⡿⣿⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣻⣿⣿⣿⣿⣾⣿⣟⣿⣿⣷⠀⠀⢸⣧⠀⠀⢸⡿⠀⠀⠛⠀⠀⠀⠀⣠⣶⡶⢾⣿⣿⣿⡇
⠀⠀⠀⠀⠀⠀⠀⠈⠙⢿⣿⣿⣿⣿⣍⠋⠙⠛⠛⠛⠛⠛⠙⠋⠉⠀⠀⣼⣿⣿⣿⡽⣳⡽⣺⣽⣳⢯⣟⣿⣻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣻⡽⣾⢿⣿⣿⡄⠀⠘⢿⣄⡀⢸⣧⠀⠀⠀⠀⠀⣀⣼⣿⣿⣧⣿⣿⣿⠿⠁
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠿⣿⣿⣿⡟⢤⠐⢶⣶⣤⡀⠄⡀⠀⠀⠀⣼⣿⣿⣿⡿⣼⡳⢿⣳⢧⣟⣻⣞⣷⣻⣽⣾⣿⣿⣿⠻⣿⣿⣿⣿⣿⣿⡿⢿⣽⡺⢯⣷⣻⡽⣯⢿⣿⣷⠀⠀⠈⠻⢿⣿⣿⣷⣄⣀⣀⣰⣿⣿⣻⣿⣿⣿⠿⠁⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠶⠙⢿⣿⣿⣶⣭⣾⣿⣿⣷⣠⣴⣷⣶⣿⠿⠋⣿⣿⣟⣶⣻⣏⣷⢻⣞⣷⣻⢾⣽⣻⣿⣿⣿⡇⠁⠸⣿⣿⣿⣟⢷⣻⡟⣶⡻⣟⣾⣳⢿⣽⣻⣿⣿⡀⠀⠀⠀⠀⠈⠉⠙⠛⠿⢿⣿⣿⡿⠿⠿⠛⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠛⠛⠛⠻⠛⠿⠛⠛⠛⠋⠁⠀⠀⠀⣿⣿⣟⢶⣻⣼⡳⣟⣾⣳⢯⣿⡽⣿⣿⣿⣿⠃⠀⠀⢻⣿⣿⣞⣯⢷⣹⢷⡻⣽⢾⣽⣻⢾⡽⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⡟⣮⢳⣞⣽⢻⡼⣯⣟⣾⣟⣿⣿⣿⣿⠀⠀⠀⠸⣿⣿⣞⣧⠿⣽⠾⣝⣷⡻⣾⡽⣯⣟⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⣹⢎⡿⣼⢞⣯⣟⣷⣻⣞⣯⣿⣿⣿⡧⠀⠀⠀⠀⣿⣿⣟⣾⢻⣭⢿⡽⣶⣻⢗⣿⣳⣟⣾⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⡞⣵⢯⡽⣞⣻⣞⣾⣳⣟⣾⣟⣿⣿⣿⠁⠀⠀⠀⠀⣻⣿⣿⡞⣟⡾⡽⣞⡷⢯⣟⣾⣳⢯⣷⣿⣿⡧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣿⡽⣎⣷⣻⡽⣧⣟⣾⣳⣯⣿⣾⣿⣿⣿⠀⠀⠀⠀⠀⠸⣿⣿⡿⣭⢿⣽⡳⣟⣟⡾⣳⢯⣟⣷⣻⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⡿⣜⣳⡽⣶⣻⢷⣻⣞⡷⣯⣷⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⣿⣿⡿⣝⣯⢶⣻⡽⡾⣝⣯⢿⡾⣽⢯⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣺⣿⣟⡷⣽⣲⢟⡵⣯⣟⣳⢯⣟⡿⣿⣻⣿⣿⠃⠀⠀⠀⠀⠀⠀⢹⣿⣿⡽⣞⣯⢷⣻⢽⣛⡾⣏⡿⣽⣻⢿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⣿⣿⣯⡝⠳⣭⠿⣽⢳⢯⡟⡿⣾⡽⣿⣿⣿⡿⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⡽⣾⢹⢻⢾⣭⢷⢹⠾⣽⢳⡟⣯⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⣿⣿⣿⣷⣿⣶⣿⣿⡾⣽⣳⢯⣷⣿⣿⣿⡷⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⢷⣫⢯⣛⣮⢿⣼⣿⣶⣿⣿⣿⣿⣿⣿⣿⣦⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣶⣿⠟⢫⠱⡘⢦⢫⠽⣹⢻⠿⣿⣿⣿⣿⣿⣽⢿⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀⢸⣿⣯⡟⣭⣟⣷⣿⣿⠿⣟⠿⣫⠝⣍⡚⢴⠒⣌⠻⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⠋⡜⢀⠃⠜⠂⣇⢛⡬⢣⣟⣳⢯⡿⣿⣿⣿⣿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⣺⣿⣷⣿⣿⣿⢿⣫⢞⡹⢎⠳⡡⠞⡰⣉⠦⡙⢤⣋⠜⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⡧⣙⠤⢃⠌⡀⢂⠜⢮⡜⣳⢎⡷⢯⣟⣿⣽⣿⣿⣿⣿⣿⡄⠀⠀⠀⠀⠀⠀⣹⣿⣿⣛⠟⡼⣣⢳⡭⢞⣡⠃⡔⢁⠒⡤⢣⡙⠶⣌⡳⣼⣿⣿⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣿⣷⢩⢎⡕⣪⡔⣡⢞⡱⣎⡵⣯⣞⣿⣾⣿⣿⣿⣿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⠸⣿⣷⣯⣛⢼⣱⢳⡚⣭⠲⣍⡔⣪⠜⡴⣣⡝⣧⣏⣷⣿⣿⣿⣏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠿⣿⣿⣎⡾⣜⣳⡿⣔⣫⢷⣭⢿⣷⣿⣿⣿⣿⣿⣿⣿⣿⡿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⣿⣿⣾⣶⣧⣝⡎⣷⢚⡼⣳⣯⡳⢷⣻⣷⣿⣿⣿⣿⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⣿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡉⠛⠛⢿⣿⣿⣾⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀-}
