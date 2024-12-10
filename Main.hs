module Main where

import Gambit
    ( bestMove,
      displayBoard,
      makeMove,
      parseMove,
      readGame,
      showGame,
      showMove,
      whoMightWin1,
      whoMightWin2,
      whoWillWin,
      getWinner,
      quickMove,
      Game,
      Move,
      Side(White, Black),
      Winner(..), allLegalMoves )
import System.IO
import System.Environment
import Data.Maybe
import System.Console.GetOpt
import Control.Monad (when)

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
    Option ['m'] ["move"] (ReqArg Move "<move>") "Make a move and display the resulting board. The move format is two positions separated by a comma, ex. a4,b7",
    Option ['v'] ["verbose"] (NoArg Verbose) "Display move quality (win, lose, tie, or rating)",
    Option ['i'] ["interactive"] (NoArg Interactive) "Play a new game interactively (You don't need to provide a game file as an arg for this)"
  ]

-- Simplified usage function
usage :: String -> IO ()
usage prog = putStrLn $ usageInfo ("Usage: " ++ prog ++ " [options]") options

-- Main function
main :: IO ()
main = do
  args <- getArgs
  let (opts, nonOpts, errs) = getOpt Permute options args
  if not (null errs)
    then do
      mapM_ putStrLn errs
      usage "gambit"
    else if Help `elem` opts
      then usage "gambit"
      else handleFlags opts nonOpts


-- Handle different flags
handleFlags :: [Flag] -> [String] -> IO ()
handleFlags opts [] 
  | (Interactive `elem` opts) = handleFlags opts ["initialGame.txt"]
  | otherwise                 = putStrLn "No game file provided..."

handleFlags opts (gameFile:_)
  | (Winner `elem` opts) = do --CHECK FOR WINNER FLAG
    --I would've used a case expression here to avoid isJust, but 
    -- that would've made the indentation uglier
    when (isJust (checkForDepth opts)) $ do
      putStrLn "The depth flag isn't compatible with the winner flag. It will be ignored."
    when (Interactive `elem` opts) $ do
      putStrLn "The interactive flag is only compatible with the verbose and depth flags. It will be ignored."
    when (isJust (checkForMove opts)) $ do
      putStrLn "The move flag isn't compatible with the winner flag. "

    game@(_,playerColor,_,_) <- loadGame gameFile
    let isVerbose = Verbose `elem` opts
    if isVerbose then do --print the best move and the resulting board (this function probably won't run)
      putStrLn (displayBoard game playerColor)
      move <- putBestMove game
      case move of
        Just m -> putStrLn (displayBoard (makeMove game m) playerColor)
        Nothing -> return ()
    else do
      --Trash is useless, I'm just using it to get the side effects of putBestMove
      trash <- putBestMove game
      return ()


  | (Interactive `elem` opts) = do  --CHECK FOR INTERACTIVE FLAG
    let inputDepth = checkForDepth opts
    putStrLn "Let's play a game. Would you like to: \n<w> Play a new game as white\n<b> Play a new game as black\n<FILENAME> Play a saved game from a txt file in this directory\n"
    hFlush stdout
    answer <- getLine
    case answer of 
      "w" -> do 
        game <- loadGame "initialGame.txt"
        startLoop game inputDepth
      "b" -> do
        game <- loadGame "initialGame.txt"
        let botMove = snd (whoMightWin2 game (if isNothing inputDepth then depth else fromJust inputDepth))
        case botMove of                          --it's impossible for this to be Nothing on the first move, so I'd argue using fromJust is ok
          Just move -> startLoop (makeMove game (fromJust botMove)) inputDepth
      gameFile -> do
        game <- loadGame gameFile
        startLoop game inputDepth


  | isJust (checkForMove opts) = do --CHECK FOR MOVE FLAG
    game@(_,playerColor,_,_) <- loadGame gameFile
    let isVerbose = Verbose `elem` opts
    let inputDepth = checkForDepth opts
    case checkForMove opts of
      Just str -> case parseMove str game of
        Just newState -> do --if the -m flag is passed and a valid move is given, play it.
          --let newState = quickMove game startPos endPos
          if isVerbose
            then do
              putStrLn (displayBoard newState playerColor)
              putStrLn (boardEval newState inputDepth)
            else putStrLn ("Here is the updated game with your move. Yeah, you probably meant to use the -v flag with this. \n" ++ showGame newState)
        Nothing -> putStrLn "That move is invalid! Remember, the imput format is two positions separated by a comma, like a3,b4"
      Nothing -> putStrLn "This will never happen, I'm using this case expression to pattern match"


  --if we make it to this case, there is no move flag or winner flag, or interactive flag. This is the default behavior, story 21
  | otherwise = do
    game@(_,playerColor,_,_) <- loadGame gameFile
    let isVerbose = Verbose `elem` opts
    let inputDepth = checkForDepth opts

    when isVerbose $ do
      putStrLn (displayBoard game playerColor)

    (rating, move) <- getGoodMove game inputDepth
    case move of --check if the move is valid, if so, then we can use it if the verbose flag was passed
      Just m -> do
        putStrLn ("A good move is " ++ showMove m)
        when isVerbose $ do
          putStrLn ("The current rating is " ++ show rating ++ " / 100")
          let newState = makeMove game m
          putStrLn (displayBoard newState playerColor)
      Nothing -> do
        putStrLn "No move can be played, the game is over."
        when isVerbose $ do
          let winnerStr 
                | rating == 100 = "a win for you!"
                | rating == -100 = "a loss..."
                | otherwise = "a tie."
          putStrLn ("The result is " ++ winnerStr)


--checks for the string arg of the Move constructor-}
startLoop :: Game -> Maybe Int -> IO ()
startLoop game Nothing = gameLoop game depth
startLoop game (Just botDepth) = gameLoop game botDepth

--                  optional depth parameter
gameLoop :: Game -> Int -> IO ()
gameLoop game@(_,playerColor,_,_) botDepth = do

  putStrLn (displayBoard game playerColor)
  --check if the enemy won last turn
  let maybeWinner = getWinner game
  case maybeWinner of 
    Just (Win color) -> if color == playerColor then putStrLn "You win! Well played." else putStrLn "Looks like I win. Good game!"
    Just Tie -> putStrLn "The game is a tie. Good game!"
    Nothing -> do

      putStrLn "What is your move? \n"
      hFlush stdout
      moveStr <- getLine
      let move = parseMove moveStr game

      case move of
        Nothing -> do
          putStrLn "That move is invalid/illegal. Remember, the input format is two positions separated by a comma, like a3,b4"
          putStrLn "Here are all the moves available to you: "
          printMoves (allLegalMoves game)
          gameLoop game botDepth
        Just withPlayerMove -> do
          --the game state after the player has moved
          --let withPlayerMove = quickMove game startPos endPos
          --print the board after the player has moved
          putStrLn (displayBoard withPlayerMove playerColor)

          --Now, it's the bot's turn to move
          let botMove = snd (whoMightWin2 withPlayerMove botDepth)
          case botMove of --if the bot couldn't make a move, the game should be over, so we check who won.
            Nothing -> do 
              let maybeWinner = getWinner withPlayerMove
              case maybeWinner of 
                Just (Win color) -> if color == playerColor then putStrLn "You win! Well played." else putStrLn "Looks like I win. Good game!"
                Just Tie -> putStrLn "The game is a tie. Good game!"
            Just bMove -> do
              let withBotMove = makeMove withPlayerMove bMove
              gameLoop withBotMove botDepth
              


boardEval :: Game -> Maybe Int -> String
boardEval game (Just depth') = let
  ratingNum = fst (whoMightWin2 game depth')
  in case ratingNum of
    100 -> "You are currently expected to win."
    -100 -> "You are currently expected to lose."
    num -> "It is unclear if a win is forced or not. The current evaluation is " ++ show num

boardEval game Nothing = boardEval game (Just depth)

printMoves :: [Move] -> IO ()
printMoves [] = return ()
printMoves (x:xs) = do
  putStrLn (showMove x)
  printMoves xs

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

putBestMove :: Game -> IO (Maybe Move)
putBestMove game = do --print the outcome of whoWillWin here too
  let move = bestMove game
  let winner = whoWillWin game
  let winStr
        | winner == Tie = "a tie."
        | winner == (Win White) = "a win for white."
        | winner == (Win Black) = "a win for black."

  case move of
    Just m -> do
      putStrLn ("The best move is " ++ showMove m ++ ". The current expected outcome is " ++ winStr)
      return (Just m)
    Nothing -> do
      putStrLn "The game is over, no move can be played."
      return Nothing


getGoodMove :: Game -> Maybe Int -> IO (Int, Maybe Move)
getGoodMove game maybeDepth = do
  case maybeDepth of 
    Nothing -> do 
      return (whoMightWin2 game depth)
    Just d -> do
      return (whoMightWin2 game d)



putGoodMove :: Game -> Maybe Int -> IO (Maybe Move)
putGoodMove game maybeDepth = do --print the outcome of whoWillWin here too
  case maybeDepth of
    Nothing -> do
      let (eval, maybeMove) = whoMightWin2 game depth
      case maybeMove of
        Just move -> do
          putStrLn ("A good move is " ++ showMove move ++ ". The game rating is " ++ show eval ++ " out of 100")
          return (Just move)

        Nothing -> do
          putStrLn "The game is over, no move can be played."
          return Nothing

    Just depthNum -> do
      let (eval, maybeMove) = whoMightWin2 game depthNum
      case maybeMove of
        Just move -> do
          putStrLn ("A good move is " ++ showMove move ++ ". The game rating is " ++ show eval ++ " out of 100")
          return (Just move)

        Nothing -> do
          putStrLn "The game is over, no move can be played."
          return Nothing



printWinner :: Maybe Winner -> Maybe String
printWinner Nothing = Nothing
printWinner (Just (Win Black)) = Just "Black wins!"
printWinner (Just (Win White)) = Just "White wins!"
printWinner (Just Tie) = Just "It's a tie."