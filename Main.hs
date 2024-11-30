module Main where
import Gambit
import System.IO
import System.Environment
import Data.Maybe

depth :: Int
depth = 4

--read file name from stdin or args, load the game, and print the best move
main :: IO ()
main = do
    putStrLn "Welcome to chess. Please enter the name of your game file: "
    hFlush stdout
    
    gameFile <- getLine
    game <- loadGame gameFile
    putBestMove game

--note that this will overwrite the contents of the file. If the file doesn't exist, it creates one with that name
writeGame :: Game -> FilePath -> IO ()
writeGame game file = do
    writeFile file (showGame game)

loadGame :: FilePath -> IO Game
loadGame file = do
    gameStr <- readFile file
    return (readGame gameStr)

putBestMove :: Game -> IO ()
putBestMove game = do --print the outcome of whoWillWin here too 
    case bestMove game depth of --                                                           this int is the depth, change as needed
        Just move -> putStrLn (showMove move ++ ". The expected outcome is " ++ case whoWillWin game depth of
            Just (Win White) -> "a win for white."
            Just (Win Black) -> "a win for black."
            Just Tie -> "a tie."
            Nothing -> "not certain.")
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