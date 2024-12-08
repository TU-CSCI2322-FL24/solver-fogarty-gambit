module Main where

import Gambit
import System.IO
import System.Environment
import Data.Maybe
import System.Console.GetOpt
import Control.Monad (when)
import Test.QuickCheck (Result(numDiscarded))

-- Default depth for move calculation
depth :: Int
depth = 3

-- Define the options
data Flag = Help | Winner | Depth Int | Move String | Verbose | Interactive | Unknown
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "Display this help message",
    Option ['w'] ["winner"] (NoArg Winner) "Show the best move using exhaustive search",
    Option ['d'] ["depth"] (ReqArg (Depth . read) "<num>") "Specify a cutoff depth for move calculation",
    Option ['m'] ["move"] (ReqArg Move "<move>") "Make a move and display the resulting board. The move format is two squares, ex. (a4,b7)",
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
  let (opts, nonOpts, errs) = getOpt RequireOrder options args
  if not (null errs)
    then do
      mapM_ putStrLn errs
      usage "gambit"
    else if Help `elem` opts
      then usage "gambit"
      else handleFlags opts nonOpts

-- Handle different flags
handleFlags :: [Flag] -> [String] -> IO ()
handleFlags _ [] = putStrLn "No game file provided..."

handleFlags [] (gameFile:_) = do  --story 21
  game <- loadGame gameFile
  putGoodMove game Nothing


--opts are the flags, nonOpts is a list that should just include the filename
handleFlags opts (gameFile:_) = do
  game@(_,playerColor,_,_) <- loadGame gameFile
  let isVerbose = Verbose `elem` opts
  let inputDepth = checkForDepth opts

  when (Winner `elem` opts) $ do
    putGoodMove game Nothing

  case checkForMove opts of 
    Just str -> case parseMove str game of
      Just move -> do --if the -m flag is passed and a valid move is given, play it.
        let newState = makeMove game move
        if isVerbose
          then do 
            putStrLn (displayBoard newState playerColor)
            putStrLn (boardEval newState inputDepth)
          else putStrLn (showGame newState)
 -- | Move movestr
 -- | otherwise = putStrLn $ "Flags provided: " ++ show opts-- Other functions to load the game and output the best move

--checks for the string arg of the Move constructor




boardEval :: Game -> Maybe Int -> String
boardEval game (Just depth') = let 
  ratingNum = fst (whoMightWin game depth')
  in case ratingNum of 
    100 -> "You are currently expected to win."
    -100 -> "You are currently expected to lose."
    num -> "It is unclear if a win is forced or not. The current evaluation is " ++ show num

boardEval game Nothing = boardEval game (Just depth)


checkForMove :: [Flag] -> Maybe String
checkForMove [] = Nothing
checkForMove ((Move str):xs) = Just str
checkForMove (x:xs) = checkForMove xs

checkForDepth :: [Flag] -> Maybe Int
checkForDepth [] = Nothing
checkForDepth ((Depth num):xs) = Just num
checkForDepth (x:xs) = checkForDepth xs

writeGame :: Game -> FilePath -> IO ()
writeGame game file = do
  writeFile file (showGame game)

loadGame :: FilePath -> IO Game
loadGame file = do
  gameStr <- readFile file
  return (readGame gameStr)

putBestMove :: Game -> IO ()
putBestMove game = do --print the outcome of whoWillWin here too
  let move = bestMove game
  let winner = whoWillWin game
  putStrLn ("The best move is " ++ showMove move ++ ". The current expected outcome is " ++ case winner of
    (Win White) -> "a win for white."
    (Win Black) -> "a win for black."
    Tie -> "a tie." 
    )

putGoodMove :: Game -> Maybe Int -> IO ()
putGoodMove game maybeDepth = do --print the outcome of whoWillWin here too
  case maybeDepth of
    Nothing -> do
      let (eval, maybeMove) = whoMightWin game depth
      case maybeMove of
        Just move -> putStrLn ("A good move is " ++ showMove move ++ ". The game rating is " ++ show eval ++ " out of 100")

        Nothing -> putStrLn "No good move was found."

    Just depthNum -> do
      let (eval, maybeMove) = whoMightWin game depthNum
      case maybeMove of
        Just move -> putStrLn ("A good move is " ++ showMove move ++ ". The game rating is " ++ show eval ++ " out of 100")

        Nothing -> putStrLn "No good move was found."

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
