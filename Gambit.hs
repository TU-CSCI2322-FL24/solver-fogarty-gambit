module Gambit where
import Prelude
import Data.Maybe
import Data.List
import Data.Char
data Side = White | Black deriving (Show, Eq)

--              enpassantable   hasMoved                               hasMoved
data PieceType = Pawn Bool | Rook Bool | Knight | Bishop | Queen | King Bool deriving (Show, Eq)

data Winner = Win Side | Tie deriving (Show, Eq)

type CurrentTurn = Side

type Position = (Char, Int) -- 'A' to 'H' for columns, 1 to 8 for rows

type Piece = (Position, Side, PieceType)

type Move = (Piece, Position)

--      Turn limit              Board History for Threefold Repetition
type Game = (Int, CurrentTurn, [Piece], [(CurrentTurn, [Piece])])



strToPos :: String -> Maybe Position
strToPos str 
    | length str /= 2 || not (validPos (head str, read (tail str))) = Nothing
    | otherwise       = Just (head str, read (tail str))

--only need this for promotion, not gonna deal with pawns
strToPiece :: String -> Maybe PieceType
strToPiece "Rook" = Just (Rook True)
strToPiece "Queen" = Just Queen
strToPiece "Bishop" = Just Bishop
strToPiece "Knight" = Just Knight
strToPiece _ = Nothing

showMove :: Move -> String
showMove (piece, pos) = ((pieceToChar piece):(show (fst pos))) ++ (show (snd pos))

validPos :: Position -> Bool
validPos (c, n) = c `elem` ['A'..'H'] && n `elem` [1..8]

parsePieceType :: String -> Maybe PieceType
parsePieceType "PT" = Just (Pawn True)
parsePieceType "PF" = Just (Pawn False)
parsePieceType "RT" = Just (Rook True)
parsePieceType "RF" = Just (Rook False)
parsePieceType "KT" = Just (King True)
parsePieceType "KF" = Just (King False)
parsePieceType "N" = Just Knight
parsePieceType "B" = Just Bishop
parsePieceType "Q" = Just Queen
parsePieceType _ = Nothing


showPieceType :: PieceType -> String
showPieceType (Pawn False) = "PF"
showPieceType (Pawn True) = "PT"
showPieceType (Rook True) = "RT"
showPieceType (Rook False) = "RF"
showPieceType (King True) = "KT"
showPieceType (King False) = "KF"
showPieceType Knight = "N"
showPieceType Bishop = "B"
showPieceType Queen = "Q"



validMove :: Game -> Position -> Position -> Bool
validMove game startPos endPos 
    | validPos startPos && validPos endPos = 
        case pieceAt startPos game of 
            Just piece -> (piece, endPos) `elem` allLegalMoves game
            Nothing -> False


pieceAt :: Position -> Game -> Maybe Piece
pieceAt pos (_,_,pieces,_)
    | validPos pos = let
        pieceLst = [(piecePos,x,y) | (piecePos, x, y) <- pieces, pos == piecePos]
        in if null pieceLst then Nothing else Just (head pieceLst)
    | otherwise =  Nothing --throw an error here?

parseMove :: String -> Game -> Maybe Game
--        pattern matching to check for a correctly-formatted arg
parseMove [startChar,startNum,',',endChar,endNum] game
    | validPos (toUpper startChar, read [startNum]) && validPos (toUpper endChar, read [endNum]) =
        quickMove2 game (toUpper startChar, read[startNum]) (toUpper endChar, read[endNum])
    | otherwise = Nothing
parseMove _ _ = Nothing --throw an error here?

parseSide :: String -> Maybe Side
parseSide "White" = Just White
parseSide "W" = Just White
parseSide "Black" = Just Black
parseSide "B" = Just Black
parseSide _ = Nothing

-- Converts a side into the full string representing it
showSideFull :: Side -> String
showSideFull White = "White"
showSideFull Black = "Black"

-- converts a side into a single char representing it
showSideChar :: Side -> Char
showSideChar White = 'W'
showSideChar Black = 'B'

stringToChar :: String -> Maybe Char
stringToChar [c] = Just c
stringToChar _ = Nothing

readGame :: String -> Game
readGame str =
    let lineList = lines str
        turnCounter = read $ head lineList
        listAfterTurnCounter = tail lineList
        currentTurn = case parseSide (head listAfterTurnCounter) of
            Just turn -> turn
            Nothing -> error "Invalid current turn in the text file"
        pieceLines = tail listAfterTurnCounter

        parsePieceLine :: String -> Piece
        parsePieceLine line =
            let [col, rowStr, sideStr, pieceTypeStr] = words line
                position = case stringToChar col of
                    Just c -> (c, read rowStr)
                    Nothing -> error "Invalid column value"
                side = case parseSide sideStr of
                    Just s -> s
                    Nothing -> error "Invalid side value"
                pieceType = case parsePieceType pieceTypeStr of
                    Just pt -> pt
                    Nothing -> error "Invalid piece type"
            in (position, side, pieceType)

        pieces = map parsePieceLine pieceLines
    in (turnCounter, currentTurn, pieces, [])

showGame :: Game -> String
showGame game@(moveCount, currentTurn, pieces, threefoldStates) = let
    strPieces :: String
    strPieces = 
        concat ['\n':(fst pos):' ':(intToDigit (snd pos)):' ':(showSideChar side):' ':(showPieceType pieceType) | (pos, side, pieceType) <- pieces]
    in (showSideFull currentTurn) ++ strPieces

--Calls displayBoard but allows for text files to be displayed
showGameFile :: FilePath -> Side -> IO ()
showGameFile filePath side = do
    content <- readFile filePath
    putStrLn (displayBoard (readGame content) side)

whoWillWin :: Game -> Winner
whoWillWin game@(_, currentTurn, _, _)  =
    case getWinner game of
        Just outcome -> outcome
        Nothing ->
            let moves = allLegalMoves game
                winners = map (\x -> whoWillWin (makeMove game x)) moves
            in bestFor currentTurn winners

otherside :: Side -> Side
otherside White = Black
otherside Black = White

bestFor :: Side  -> [Winner] -> Winner
bestFor side lst
    | (Win side) `elem` lst = Win side
    | Tie `elem` lst = Tie
    | otherwise = Win (otherside side)

bestMove :: Game -> Maybe Move
bestMove game@(_, currentTurn, _, _) =
    case getWinner game of
        Just _ -> Nothing
        Nothing ->
            let moves = allLegalMoves game
                evaluateMove move =
                    let resultingGame = makeMove game move
                        outcome = whoWillWin resultingGame
                    in (outcome, move)
            in if null moves then Nothing
                else case foldr (\move acc@(outcome, _) -> 
                        if outcome == Win currentTurn then acc else evaluateMove move
                        ) (Win (otherside currentTurn), head moves) moves of
                (_, move) -> Just move 

--Version that immediately picks the winning move and stops searching the rest (faster for higher depths >=3)
whoMightWin2 :: Game -> Int -> (Int, Maybe Move)
whoMightWin2 game@(_, currentTurn, _, _) 0 =
    let rating = rateGame game
    in (rating, Nothing) -- At depth 0, no move is considered
whoMightWin2 game@(_, currentTurn, _, _) depth =
    let moves = allLegalMoves game
        evaluateMove move =
            let (rating, _) = whoMightWin2 (makeMove game move) (depth - 1)
            in (rating, Just move)
        evaluateAndStop (rating, move) acc =
            if (currentTurn == White && rating == 100) || (currentTurn == Black && rating == -100)
            then (rating, move)  -- Stop searching if a winning move is found
            else acc
        rankedMoves = map evaluateMove moves
    in if null moves
        then (rateGame game, Nothing) -- No moves available
        else foldr evaluateAndStop (if currentTurn == White
                                      then maximumBy (\(r1, _) (r2, _) -> compare r1 r2) rankedMoves
                                      else minimumBy (\(r1, _) (r2, _) -> compare r1 r2) rankedMoves)
                                      rankedMoves

--Original version (faster for lower depths <=2)
whoMightWin1 :: Game -> Int -> (Int, Maybe Move)
whoMightWin1 game@(_, currentTurn, _, _) 0 =
    let rating = rateGame game
    in (rating, Nothing) -- At depth 0, no move is considered
whoMightWin1 game@(_, currentTurn, _, _) depth =
    let moves = allLegalMoves game
        evaluateMove move =
            let (rating, _) = whoMightWin1 (makeMove game move) (depth - 1)
            in (rating, Just move)
        rankedMoves = map evaluateMove moves
    in if null moves
        then (rateGame game, Nothing) -- No moves available
        else if currentTurn == White
            then maximumBy (\(r1, _) (r2, _) -> compare r1 r2) rankedMoves -- Maximize for White
            else minimumBy (\(r1, _) (r2, _) -> compare r1 r2) rankedMoves -- Minimize for Black

rateGame :: Game -> Int
rateGame game@(_, currentTurn, _, _)= 
    case getWinner game of
        Just (Win White) -> 100
        Just (Win Black) -> -100
        Just Tie -> 0
        Nothing -> 
            if currentTurn == White then getMaterialDifference game White
            else -(getMaterialDifference game Black)

pieceToChar :: Piece -> Char
pieceToChar (_, Black, King _) = '\x2654'
pieceToChar (_, White, King _) = '\x265A'
pieceToChar (_, Black, Queen) = '\x2655'
pieceToChar (_, White, Queen) = '\x265B'
pieceToChar (_, Black, Bishop) = '\x2657'
pieceToChar (_, White, Bishop) = '\x265D'
pieceToChar (_, Black, Knight) = '\x2658'
pieceToChar (_, White, Knight) = '\x265E'
pieceToChar (_, Black, Rook _) = '\x2656'
pieceToChar (_, White, Rook _) = '\x265C'
pieceToChar (_, Black, Pawn _) = '\x2659'
pieceToChar (_, White, Pawn _) = '\x265F'

--IncrementPos increments a position one square to the right, starting at a8 and ending at h1.
-- Incrementing h1 returns a8. Alternatively, it could return a Maybe Position or throw an error.
incrementPos :: Position -> Position
incrementPos ('H', 1) = ('A', 8)
--                           gets the next letter in the alphabet
incrementPos ('H', num) = ('A', num - 1)
incrementPos (column, num) = (head (tail [column..'H']), num)

--These are the unicode chars used to display the board
--blackSquare = '\x2B1B'
whiteSquare = '\x2588'
blackSquare = '\x2591'
--whiteSquare = '\x2B1C'
whiteFirstRow = [whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare]
blackFirstRow = [blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare, blackSquare, whiteSquare]
emptyBoard = whiteFirstRow ++ blackFirstRow ++ whiteFirstRow ++ blackFirstRow ++ whiteFirstRow ++ blackFirstRow ++whiteFirstRow ++ blackFirstRow

displayBoard :: Game -> Side -> String --IO ()
displayBoard game@(_, _, pieces, _) pov = let
    --Association list of every piece on the board
    positionsAndPieces = [(pos, (side, pieceType)) | (pos, side, pieceType) <- pieces]

    (missingWhitePieces, missingBlackPieces) = getMissingPieces pieces
    missingWhiteStr = map pieceToChar missingWhitePieces
    missingBlackStr = map pieceToChar missingBlackPieces

    --Builds the board line by line, recursively
    aux :: String -> [Char] -> Position -> String
    --Return the completed board
    aux out [] currentPos = out

    --Case for the last (furthest right) square of each row.
    aux out (square:board) currentPos@('H', num) = let 
        maybePiece :: Maybe (Side, PieceType)
        maybePiece = lookup currentPos positionsAndPieces
        --Checks if we need to print a piece char or a square char
        in case maybePiece of
            --                                            Prints the row numbers on the side of the board
            Nothing -> aux (out ++ [square] ++ "|" ++  "\n" ++ (if (snd (incrementPos currentPos) == 8) then " " else show 
                 (snd (incrementPos currentPos))) ++ (if currentPos == ('H', 1) then " " else "|")) (board) (incrementPos currentPos) 
            (Just piece) -> aux (out ++ [pieceToChar (currentPos, fst piece, snd piece)] ++ "|" ++  "\n" ++ 
                (if (snd (incrementPos currentPos) == 8) then " " else show (snd (incrementPos currentPos))) ++ 
                (if currentPos == ('H', 1) then " " else "|")) (board) (incrementPos currentPos)

    aux out (square:rows) currentPos = let
        maybePiece = lookup currentPos positionsAndPieces
        in case maybePiece of 
            Nothing -> aux (out ++ [square]) (rows) (incrementPos currentPos) 
            (Just piece) -> aux (out ++ [pieceToChar (currentPos, fst piece, snd piece)]) (rows) (incrementPos currentPos)

    displayStr = 
        if pov == White then 
            ("\n" ++ missingWhiteStr ++ "\n  ________ \n8|" ++ (aux "" emptyBoard ('A', 8)) 
            ++ "\b ‾‾‾‾‾‾‾‾ \n  ABCDEFGH\n\n" ++ missingBlackStr ++ "\n") else ("\n" ++ missingBlackStr ++ "\n ________ " 
            ++ (reverse (((aux "" emptyBoard ('A', 8)) ++ ""))) ++ "|8\n ‾‾‾‾‾‾‾‾ \n HGFEDCBA\n\n" ++ missingWhiteStr ++ "\n")
    in displayStr --putStrLn displayStr

--use putStrLn in the shell to print this string

getMissingPieces :: [Piece] -> ([Piece], [Piece])
getMissingPieces [] = ([], [])
getMissingPieces pieces = let
    blackPieces = [piece | (pos, color, piece) <- pieces, color == Black]
    whitePieces = [piece | (pos, color, piece) <- pieces, color == White]
    (_, _, initialPieces, _) = initialGame
    (missingWhitePawns, missingBlackPawns) = 
        ((replicate (length [piece | (pos, color, piece) <- initialPieces, color == White, 
        (piece == Pawn True || piece == Pawn False)] - length [piece | piece <- whitePieces, 
        (piece == Pawn True || piece == Pawn False)]) (('A', 1), White, Pawn False)), 
        replicate (length [piece | (pos, color, piece) <- initialPieces, color == Black, 
        (piece == Pawn True || piece == Pawn False)] - length [piece | piece <- blackPieces, 
        (piece == Pawn True || piece == Pawn False)]) (('A', 1), Black, Pawn False))
    (missingWhiteRooks, missingBlackRooks) = 
        ((replicate (length [piece | (pos, color, piece) <- initialPieces, color == White, 
        (piece == Rook True || piece == Rook False)] - length [piece | piece <- whitePieces, 
        (piece == Rook True || piece == Rook False)]) (('A', 1), White, Rook False)), 
        replicate (length [piece | (pos, color, piece) <- initialPieces, color == Black, 
        (piece == Rook True || piece == Rook False)] - length [piece | piece <- blackPieces, 
        (piece == Rook True || piece == Rook False)]) (('A', 1), Black, Rook False))
    (missingWhiteKings, missingBlackKings) = 
        ((replicate (length [piece | (pos, color, piece) <- initialPieces, color == White, 
        (piece == King True || piece == King False)] - length [piece | piece <- whitePieces, 
        (piece == King True || piece == King False)]) (('A', 1), White, King False)), replicate 
        (length [piece | (pos, color, piece) <- initialPieces, color == Black, 
        (piece == King True || piece == King False)] - length [piece | piece <- blackPieces, 
        (piece == King True || piece == King False)]) (('A', 1), Black, King False))
    (missingWhiteQueens, missingBlackQueens) = 
        ((replicate (length [piece | (pos, color, piece) <- initialPieces, color == White, piece == Queen]
         - length [piece | piece <- whitePieces, piece == Queen]) (('A', 1), White, Queen)), 
         replicate (length [piece | (pos, color, piece) <- initialPieces, color == Black, piece == Queen] 
         - length [piece | piece <- blackPieces, piece == Queen]) (('A', 1), Black, Queen))
    (missingWhiteBishops, missingBlackBishops) = 
        ((replicate (length [piece | (pos, color, piece) <- initialPieces, color == White, piece == Bishop] 
        - length [piece | piece <- whitePieces, piece == Bishop]) (('A', 1), White, Bishop)), 
        replicate (length [piece | (pos, color, piece) <- initialPieces, color == Black, piece == Bishop] 
        - length [piece | piece <- blackPieces, piece == Bishop]) (('A', 1), Black, Bishop))
    (missingWhiteKnights, missingBlackKnights) = 
        ((replicate (length [piece | (pos, color, piece) <- initialPieces, color == White, piece == Knight] 
        - length [piece | piece <- whitePieces, piece == Knight]) (('A', 1), White, Knight)), 
        replicate (length [piece | (pos, color, piece) <- initialPieces, color == Black, piece == Knight] 
        - length [piece | piece <- blackPieces, piece == Knight]) (('A', 1), Black, Knight))
    in 
        (missingWhitePawns ++ missingWhiteKnights ++ missingWhiteBishops ++ missingWhiteRooks ++ missingWhiteQueens 
        ++ missingWhiteKings, missingBlackPawns ++ missingBlackKnights ++ missingBlackBishops ++ missingBlackRooks 
        ++ missingBlackQueens ++ missingBlackKings)


allPositions :: [Position]
allPositions = [(x, y) | x <- ['A'..'H'], y <- [1..8]]

initialGame :: Game
initialGame = (100, White, initialPieces, []) where
    initialPieces = initialPawns ++ initialRooks ++ initialKnights ++ initialBishops ++ initialQueens ++ initialKings where
        initialPawns = [ ((col, if side == White then 2 else 7), side, Pawn False) | side <- [White, Black], col <- ['A'..'H']]
        initialRooks = [ ((col, if side == White then 1 else 8), side, Rook False) | side <- [White, Black], col <- ['A','H']]
        initialKnights = [ ((col, if side == White then 1 else 8), side, Knight) | side <- [White, Black], col <- ['B','G']]
        initialBishops = [ ((col, if side == White then 1 else 8), side, Bishop) | side <- [White, Black], col <- ['C','F']]
        initialQueens = [ (('D', 1), White, Queen), (('D', 8), Black, Queen)]
        initialKings = [ (('E', 1), White, King False), (('E', 8), Black, King False)]

-- Calculates the difference in material between the input side and the other side
getMaterialDifference :: Game -> Side -> Int
getMaterialDifference (_, _, pieces, _) side =
    let 
        -- Assigns a material value to each piece type
        pieceValue :: PieceType -> Int
        pieceValue (Pawn _)   = 1
        pieceValue (Rook _)   = 5
        pieceValue Bishop     = 3
        pieceValue Knight     = 3
        pieceValue Queen      = 9
        pieceValue (King _)   = 0

        -- Calculates the total material for a given list of pieces
        materialValue :: [Piece] -> Int
        materialValue ps = sum [pieceValue pt | (_, _, pt) <- ps]

        -- Separate pieces into those belonging to the input side and the other side
        sideMaterial = materialValue [p | p@(_, s, _) <- pieces, s == side]
        otherMaterial = materialValue [p | p@(_, s, _) <- pieces, s /= side]

    in (sideMaterial - otherMaterial)

getMaterialWinner :: Game -> Maybe Side
getMaterialWinner game
    | whiteMaterial > blackMaterial = Just White
    | blackMaterial > whiteMaterial = Just Black
    | otherwise               = Nothing
  where
    whiteMaterial = getMaterialDifference game White
    blackMaterial = getMaterialDifference game Black

getWinner :: Game -> Maybe Winner
getWinner game@(_, currentTurn, _, _) =
    let otherSide = otherside currentTurn in
        if checkMate game currentTurn then Just (Win otherSide)
        else if checkMate game otherSide then Just (Win currentTurn)
                else if staleMate game || drawByMaterial game || drawByThreefoldRepetition game || drawByTurnCount game then Just Tie
                else Nothing

--Get every possible move given a specfic piece
allPieceMoves :: Game -> Piece -> [Move]
allPieceMoves game (pos, side, pieceType) =
    case pieceType of
        Pawn enPassantable -> 
            let (col, row) = pos
                forwardOne = if side == White then row + 1 else row - 1
                forwardTwo = if side == White then row + 2 else row - 2
                leftDiag = (pred col, forwardOne)  -- Diagonal left
                rightDiag = (succ col, forwardOne) -- Diagonal right
                leftPos = (pred col, row)          -- Directly left
                rightPos = (succ col, row)         -- Directly right
                isOnStartingRow = (side == White && row == 2) || (side == Black && row == 7)
                isPromotionRow = (side == White && forwardOne == 8) || (side == Black && forwardOne == 1)
                
                -- Check if the square directly forward is empty
                canMoveOne = getPiece game (col, forwardOne) == Nothing

                -- Check if both squares directly forward are empty for a two-square move
                canMoveTwo = isOnStartingRow && canMoveOne && getPiece game (col, forwardTwo) == Nothing

                -- Check if there’s an opponent’s piece on either diagonal for capture
                canCaptureLeft = case getPiece game leftDiag of
                    Just (_, otherSide, _) -> otherSide /= side
                    Nothing -> False

                canCaptureRight = case getPiece game rightDiag of
                    Just (_, otherSide, _) -> otherSide /= side
                    Nothing -> False
                
                -- Check if en passant is possible (en passant-able pawn directly to the left or right)
                enPassantLeft = case getPiece game leftPos of
                    Just (_, otherSide, Pawn True) -> otherSide /= side
                    _ -> False
                enPassantRight = case getPiece game rightPos of
                    Just (_, otherSide, Pawn True) -> otherSide /= side
                    _ -> False

                promotionPieces = [Queen, Rook True, Bishop, Knight]
                singleMove = if canMoveOne then
                                if isPromotionRow 
                                then [((pos, side, promotedPiece), (col, forwardOne)) | promotedPiece <- promotionPieces]
                                else [((pos, side, Pawn enPassantable), (col, forwardOne))]
                             else []

                doubleMove = if canMoveTwo
                             then [((pos, side, Pawn True), (col, forwardTwo))]
                             else []

                -- Generate capture moves, with promotion options if on promotion row
                captureMoves = concat
                    [ if canCaptureLeft then
                          if isPromotionRow 
                          then [((pos, side, promotedPiece), leftDiag) | promotedPiece <- promotionPieces]
                          else [((pos, side, Pawn False), leftDiag)]
                      else []
                    , if canCaptureRight then
                          if isPromotionRow 
                          then [((pos, side, promotedPiece), rightDiag) | promotedPiece <- promotionPieces]
                          else [((pos, side, Pawn False), rightDiag)]
                      else []
                    ]

                enPassantMoves = concat
                    [ if enPassantLeft then [((pos, side, Pawn False), leftDiag)] else []
                    , if enPassantRight then [((pos, side, Pawn False), rightDiag)] else []
                    ]

            --in filter (\move -> not (causeCheck game move side)) singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves
            in singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves

        Rook hasMoved ->
            let (col, row) = pos  -- Current position of the rook

                -- Generate positions in each direction
                positionsUp = [ (col, r) | r <- [row + 1 .. 8] ]
                positionsDown = [ (col, r) | r <- [row - 1, row - 2 .. 1] ]
                positionsLeft = [ (c, row) | c <- [chr (ord col - 1), chr (ord col - 2) .. 'A'] ]
                positionsRight = [ (c, row) | c <- [chr (ord col + 1), chr (ord col + 2) .. 'H'] ]

                -- Function to get rook moves in a direction
                rookMovesInDirection :: [Position] -> [Position]
                rookMovesInDirection [] = []
                rookMovesInDirection (p:ps) =
                    case getPiece game p of
                        Nothing -> p : rookMovesInDirection ps  -- Empty square, can move, continue
                        Just (_, pieceSide, _) ->
                            if pieceSide == side
                            then []      -- Friendly piece, cannot move further
                            else [p]     -- Opponent's piece, can capture, stop after this square

                -- Generate moves for each direction
                upMoves = [ ((pos, side, Rook True), p) | p <- rookMovesInDirection positionsUp ]
                downMoves = [ ((pos, side, Rook True), p) | p <- rookMovesInDirection positionsDown ]
                leftMoves = [ ((pos, side, Rook True), p) | p <- rookMovesInDirection positionsLeft ]
                rightMoves = [ ((pos, side, Rook True), p) | p <- rookMovesInDirection positionsRight ]

            --in filter (\move -> not (causeCheck game move side)) upMoves ++ downMoves ++ leftMoves ++ rightMoves
            in upMoves ++ downMoves ++ leftMoves ++ rightMoves
        
        Knight ->
            let (col, row) = pos  -- Current position of the knight

        -- List of possible relative moves for a knight
                possibleMoves = [
                    (chr (ord col + 2), row + 1),
                    (chr (ord col + 2), row - 1),
                    (chr (ord col - 2), row + 1),
                    (chr (ord col - 2), row - 1),
                    (chr (ord col + 1), row + 2),
                    (chr (ord col + 1), row - 2),
                    (chr (ord col - 1), row + 2),
                    (chr (ord col - 1), row - 2)
                    ]

        -- Filter moves to only include valid board positions and exclude moves landing on friendly pieces
                validMoves = [((pos, side, Knight), newPos) |
                        newPos@(newCol, newRow) <- possibleMoves,
                        newCol >= 'A' && newCol <= 'H',  -- Ensure within column bounds
                        newRow >= 1 && newRow <= 8,      -- Ensure within row bounds
                        case getPiece game newPos of
                            Nothing -> True  -- Empty square, valid move
                            Just (_, pieceSide, _) -> pieceSide /= side  -- Opponent's piece, valid move
                     ]
            --in filter (\move -> not (causeCheck game move side)) validMoves
            in validMoves

        Bishop ->
            let (col, row) = pos  -- Current position of the bishop

                -- Generate all diagonal positions in each direction
                positionsUpRight = [ (chr (ord col + i), row + i) | i <- [1..7], chr (ord col + i) <= 'H', row + i <= 8 ]
                positionsUpLeft = [ (chr (ord col - i), row + i) | i <- [1..7], chr (ord col - i) >= 'A', row + i <= 8 ]
                positionsDownRight = [ (chr (ord col + i), row - i) | i <- [1..7], chr (ord col + i) <= 'H', row - i >= 1 ]
                positionsDownLeft = [ (chr (ord col - i), row - i) | i <- [1..7], chr (ord col - i) >= 'A', row - i >= 1 ]

                -- Function to get bishop moves in a direction
                bishopMovesInDirection :: [Position] -> [Position]
                bishopMovesInDirection [] = []
                bishopMovesInDirection (p:ps) =
                    case getPiece game p of
                        Nothing -> p : bishopMovesInDirection ps  -- Empty square, can move, continue
                        Just (_, pieceSide, _) ->
                            if pieceSide == side
                            then []      -- Friendly piece, cannot move further
                            else [p]     -- Opponent's piece, can capture, stop after this square

                -- Generate moves for each diagonal direction
                upRightMoves = [ ((pos, side, Bishop), p) | p <- bishopMovesInDirection positionsUpRight ]
                upLeftMoves = [ ((pos, side, Bishop), p) | p <- bishopMovesInDirection positionsUpLeft ]
                downRightMoves = [ ((pos, side, Bishop), p) | p <- bishopMovesInDirection positionsDownRight ]
                downLeftMoves = [ ((pos, side, Bishop), p) | p <- bishopMovesInDirection positionsDownLeft ]

            --in filter (\move -> not (causeCheck game move side)) upRightMoves ++ upLeftMoves ++ downRightMoves ++ downLeftMoves
            in upRightMoves ++ upLeftMoves ++ downRightMoves ++ downLeftMoves

        Queen ->
            let (col, row) = pos  -- Current position of the queen

                -- Generate all diagonal positions (bishop-like movement)
                positionsUpRight = [ (chr (ord col + i), row + i) | i <- [1..7], chr (ord col + i) <= 'H', row + i <= 8 ]
                positionsUpLeft = [ (chr (ord col - i), row + i) | i <- [1..7], chr (ord col - i) >= 'A', row + i <= 8 ]
                positionsDownRight = [ (chr (ord col + i), row - i) | i <- [1..7], chr (ord col + i) <= 'H', row - i >= 1 ]
                positionsDownLeft = [ (chr (ord col - i), row - i) | i <- [1..7], chr (ord col - i) >= 'A', row - i >= 1 ]

                -- Generate all straight positions (rook-like movement)
                positionsUp = [ (col, r) | r <- [row + 1 .. 8] ]
                positionsDown = [ (col, r) | r <- [row - 1, row - 2 .. 1] ]
                positionsLeft = [ (c, row) | c <- [chr (ord col - 1), chr (ord col - 2) .. 'A'] ]
                positionsRight = [ (c, row) | c <- [chr (ord col + 1), chr (ord col + 2) .. 'H'] ]

                -- Helper function to get moves in a direction (works for both rook and bishop-like moves)
                queenMovesInDirection :: [Position] -> [Position]
                queenMovesInDirection [] = []
                queenMovesInDirection (p:ps) =
                    case getPiece game p of
                        Nothing -> p : queenMovesInDirection ps  -- Empty square, continue
                        Just (_, pieceSide, _) ->
                            if pieceSide == side
                            then []      -- Friendly piece, stop
                            else [p]     -- Opponent's piece, capture and stop

                -- Generate moves for each direction
                upMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsUp ]
                downMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsDown ]
                leftMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsLeft ]
                rightMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsRight ]
                
                upRightMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsUpRight ]
                upLeftMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsUpLeft ]
                downRightMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsDownRight ]
                downLeftMoves = [ ((pos, side, Queen), p) | p <- queenMovesInDirection positionsDownLeft ]

            --in filter (\move -> not (causeCheck game move side)) upMoves ++ downMoves ++ leftMoves ++ rightMoves ++ upRightMoves ++ upLeftMoves ++ downRightMoves ++ downLeftMoves
            in upMoves ++ downMoves ++ leftMoves ++ rightMoves ++ upRightMoves ++ upLeftMoves ++ downRightMoves ++ downLeftMoves
        
        King hasMoved ->
            let (col, row) = pos  -- Current position of the king

                -- List of possible moves for a king (one square in each direction)
                possibleMoves = [
                    (chr (ord col + 1), row),        -- Right
                    (chr (ord col - 1), row),        -- Left
                    (col, row + 1),                  -- Up
                    (col, row - 1),                  -- Down
                    (chr (ord col + 1), row + 1),    -- Up-right
                    (chr (ord col - 1), row + 1),    -- Up-left
                    (chr (ord col + 1), row - 1),    -- Down-right
                    (chr (ord col - 1), row - 1)     -- Down-left
                    ]

                -- Filter moves to only include valid board positions and exclude moves landing on friendly pieces
                validMoves = [((pos, side, King True), newPos) |
                                newPos@(newCol, newRow) <- possibleMoves,
                                newCol >= 'A' && newCol <= 'H',  -- Ensure within column bounds
                                newRow >= 1 && newRow <= 8,      -- Ensure within row bounds
                                case getPiece game newPos of
                                    Nothing -> True  -- Empty square, valid move
                                    Just (_, pieceSide, _) -> pieceSide /= side  -- Opponent's piece, valid move
                            ]
            --in filter (\move -> not (causeCheck game move side)) validMoves
            in validMoves
            

--Get every possible move for a side, including castling
allLegalMoves :: Game -> [Move]
allLegalMoves game@(_ , side, pieces, _) =
    let sidePieces = filter (\(_, s, _) -> s == side) pieces
        legalMoves = concatMap (allPieceMoves game) sidePieces
        kingSideCastle :: [Move] 
        kingSideCastle =
            if side == White then
                if inCheck game White == False && getPiece game ('F', 1) == Nothing 
                    && getPiece game ('G', 1) == Nothing && getPiece game ('E', 1) == Just (('E', 1), White, King False) 
                    && getPiece game ('H', 1) == Just (('H', 1), White, Rook False) then
                        if causeCheck game ((('E', 1), White, King False), ('F', 1)) side == False 
                           && causeCheck game ((('E', 1), White, King False), ('G', 1)) side == False 
                           then [((('E', 1), White, King True), ('G', 1))] 
                        else []
                else []
            else if inCheck game Black == False && getPiece game ('F', 8) == Nothing 
                    && getPiece game ('G', 8) == Nothing && getPiece game ('E', 8) == Just (('E', 8), Black, King False) 
                    && getPiece game ('H', 8) == Just (('H', 8), Black, Rook False) then
                        if causeCheck game ((('E', 8), Black, King False), ('F', 8)) side == False 
                           && causeCheck game ((('E', 8), Black, King False), ('G', 8)) side == False 
                            then [((('E', 8), Black, King True), ('G', 8))] 
                        else []
                else []
        queenSideCastle :: [Move]
        queenSideCastle =
            if side == White then
                if inCheck game White == False && getPiece game ('D', 1) == Nothing 
                    && getPiece game ('C', 1) == Nothing && getPiece game ('B', 1) == Nothing 
                    && getPiece game ('E', 1) == Just (('E', 1), White, King False) 
                    && getPiece game ('A', 1) == Just (('A', 1), White, Rook False) then
                        if causeCheck game ((('E', 1), White, King False), ('D', 1)) side == False 
                           && causeCheck game ((('E', 1), White, King False), ('C', 1)) side == False 
                            then [((('E', 1), White, King True), ('C', 1))] 
                        else []
                else []
            else if inCheck game Black == False && getPiece game ('D', 8) == Nothing 
                    && getPiece game ('C', 8) == Nothing && getPiece game ('B', 8) == Nothing 
                    && getPiece game ('E', 8) == Just (('E', 8), Black, King False) 
                    && getPiece game ('A', 8) == Just (('A', 8), Black, Rook False) then
                        if causeCheck game ((('E', 8), Black, King False), ('D', 8)) side == False 
                           && causeCheck game ((('E', 8), Black, King False), ('C', 8)) side == False 
                           then [((('E', 8), White, King True), ('C', 8))] 
                        else []
                else []
    in filter (\move -> not (causeCheck game move side)) legalMoves ++ kingSideCastle ++ queenSideCastle
    --in legalMoves ++ kingSideCastle ++ queenSideCastle

outOfBounds :: Position -> Bool
outOfBounds (x, y) = (ord(x) > 72 || ord(x) < 65 || y > 8 || y < 1)

makeMove :: Game -> Move -> Game
makeMove (turnCounter, side, positions, boardHistory) (piece@(startPos, pieceSide, pieceType), endPos) =
    if side == pieceSide then
        let 
            newSide = if side == White then Black else White
            -- Determine if this move is a capture or pawn move for the 50-move rule
            isCapture = isJust (getPiece (turnCounter, side, positions, boardHistory) endPos)
            isPawnMove = case pieceType of
                Pawn _ -> True
                _      -> False
            
            --newFiftyMoveCounter = if isCapture || isPawnMove then 0 else turnCounter + 1

            -- Determine if this move is a promotion move
            isPromotionMove = 
                case getPiece (turnCounter, side, positions, boardHistory) startPos of
                    Just (_, _, startPieceType) -> startPieceType /= pieceType
                    Nothing -> False

            -- Determine if this move is an en passant capture
            isEnPassantCapture =
                case getPiece (turnCounter, side, positions, boardHistory) endPos of
                    Just _ -> False
                    Nothing -> if pieceType == (Pawn False) && fst startPos /= fst endPos then True else False
            
            -- Determine the captured pawn's position for en passant
            capturedPawnPos = 
                if isEnPassantCapture 
                then (fst endPos, snd startPos) -- One row behind the landing position
                else endPos

            isCastlingMove = case pieceType of
                    King _ -> abs ((ord (fst endPos)) - (ord (fst startPos))) == 2
                    _      -> False
            newPositions = 
                if isCastlingMove
                then
                    let rookStartPos = if fst endPos > fst startPos
                                    then ('H', snd startPos)  -- King-side castling
                                    else ('A', snd startPos)  -- Queen-side castling
                        rookEndPos = if fst endPos > fst startPos
                                    then ('F', snd startPos)  -- King-side castling
                                    else ('D', snd startPos)  -- Queen-side castling
                    in map (\p@(pos, s, pt) -> 
                                if pos == startPos 
                                then (endPos, s, King True)       -- Move the King
                                else if pos == rookStartPos 
                                then (rookEndPos, s, Rook True)   -- Move the Rook
                                else p) positions
                else if isPromotionMove
                    then
                        map (\p@(pos, s, pt) -> 
                                if pos == startPos 
                                then (endPos, s, pieceType)  -- Replace the pawn with the new piece
                                else p
                        ) (filter (\(pos, _, _) -> pos /= endPos) positions) 
                
                else if isEnPassantCapture
                    then
                        map (\p@(pos, s, pt) -> 
                                if pos == startPos 
                                then (endPos, s, pt)  -- Move the pawn
                                else p
                ) (filter (\(pos, _, _) -> pos /= capturedPawnPos) positions) -- Remove the captured pawn
                    
                else
                    map (\p@(pos, s, pt) -> -- ChatGPT helped write this part about modifying the pawn, rook and king booleans after a move
                            if pos == startPos 
                            then case pt of
                                    Pawn _ -> (endPos, s, Pawn (abs (snd endPos - snd startPos) == 2))
                                    Rook _ -> (endPos, s, Rook True)
                                    King _ -> (endPos, s, King True)
                                    _ -> (endPos, s, pt)
                            else case pt of
                                    Pawn _ -> (pos, s, Pawn False)  -- Reset enpassantable for all other pawns
                                    _ -> p
                    ) (filter (\(pos, _, _) -> pos /= endPos) positions)
            updatedHistory = ((side, positions) : boardHistory)
        in (turnCounter -1, newSide, newPositions, updatedHistory)
    else error ("It is not " ++ show (if side == White then Black else White) ++ "'s turn")

--Quick move calls makeMove but uses two positions instead of the longer alternative
quickMove :: Game -> Position -> Position -> Game
quickMove game@(_, currentTurn, _, _) startPos endPos =
    case getPiece game startPos of
        -- Handle the pawn case
        Just (pos, side, Pawn _) ->
            if (currentTurn == White && snd startPos == 7 && snd endPos == 8) || 
                (currentTurn == Black && snd startPos == 2 && snd endPos == 1) then promotePiece game startPos endPos Queen
            else
            let isTwoSquareMove = abs (snd startPos - snd endPos) == 2
                move = if isTwoSquareMove
                       then ((pos, side, Pawn True), endPos)  -- Set Pawn True for two-square move
                       else ((pos, side, Pawn False), endPos) -- Keep Pawn False for one-square move
            in if move `elem` allLegalMoves game then makeMove game move else error ("Such move does not exist")
        
        -- Handle the rook case
        Just (pos, side, Rook _) -> 
            let move = ((pos, side, Rook True), endPos)  -- Set Rook True after it moves
            in if move `elem` allLegalMoves game then makeMove game move else error ("Such move does not exist")

        -- Handle the king case
        Just (pos, side, King _) -> 
            let move = ((pos, side, King True), endPos)  -- Set King True after it moves
            in if move `elem` allLegalMoves game then makeMove game move else error ("Such move does not exist")

        -- Default case for other pieces
        Just piece -> 
            let move = (piece, endPos)
            in if move `elem` allLegalMoves game then makeMove game move else error ("Such move does not exist")

        Nothing -> error "No piece at starting position"


--Quick move calls makeMove but uses two positions instead of the longer alternative
quickMove2 :: Game -> Position -> Position -> Maybe Game
quickMove2 game@(_, currentTurn, _, _) startPos endPos =
    case getPiece game startPos of
        -- Handle the pawn case
        Just (pos, side, Pawn _) ->
            if (currentTurn == White && snd startPos == 7 && snd endPos == 8) || 
                (currentTurn == Black && snd startPos == 2 && snd endPos == 1) then Just (promotePiece game startPos endPos Queen)
            else
            let isTwoSquareMove = abs (snd startPos - snd endPos) == 2
                move = if isTwoSquareMove
                       then ((pos, side, Pawn True), endPos)  -- Set Pawn True for two-square move
                       else ((pos, side, Pawn False), endPos) -- Keep Pawn False for one-square move
            in if move `elem` allLegalMoves game then Just (makeMove game move) else Nothing --error ("Such move does not exist")
        
        -- Handle the rook case
        Just (pos, side, Rook _) -> 
            let move = ((pos, side, Rook True), endPos)  -- Set Rook True after it moves
            in if move `elem` allLegalMoves game then Just (makeMove game move) else Nothing --error ("Such move does not exist")

        -- Handle the king case
        Just (pos, side, King _) -> 
            let move = ((pos, side, King True), endPos)  -- Set King True after it moves
            in if move `elem` allLegalMoves game then Just (makeMove game move) else Nothing --error ("Such move does not exist")

        -- Default case for other pieces
        Just piece -> 
            let move = (piece, endPos)
            in if move `elem` allLegalMoves game then Just (makeMove game move) else Nothing --error ("Such move does not exist")

        Nothing -> Nothing --error "No piece at starting position"

promotePiece :: Game -> Position -> Position -> PieceType -> Game
promotePiece game@(_, currentTurn, _, _) startPos endPos promotionPiece =
    if (currentTurn == White && snd startPos == 7 && snd endPos == 8) || (currentTurn == Black && snd startPos == 2 && snd endPos == 1) then
    case getPiece game startPos of
        Just (pos, side, Pawn _) ->
            let move = ((pos, side, promotionPiece), endPos)
            in if move `elem` allLegalMoves game then makeMove game move else error ("Such move does not exist")
        Just piece -> 
            error "Not a pawn at starting position"
        Nothing -> error "No piece at starting position"
    else error "not a piece able to be promoted"

--Check if a side is currently in the state of checkmate, aka that side is in check and has no more legal moves to be made
checkMate :: Game -> Side -> Bool
checkMate game@(_, currentTurn, _, _) side =
    (currentTurn == side) && (length (allLegalMoves game) == 0 && inCheck game side)

--Check if the current turn of the game as no moves but is also not in check
staleMate :: Game -> Bool
staleMate game@(_, side, _, _) =
    length (allLegalMoves game) == 0 && not (inCheck game side)

--Check if the game is in a default tie by material state
drawByMaterial :: Game -> Bool
drawByMaterial game@(_, _, pieces, _) =
    let pieceTypes = [pieceType | (_, _, pieceType) <- pieces]
        count :: PieceType -> Int
        count pType = length [pt | pt <- pieceTypes, pt == pType]
        bishopsSameColor :: Bool
        bishopsSameColor =
            let bishopPositions = [pos | (pos, _, Bishop) <- pieces]
                isLightSquare (col, row) = (ord col + row) `mod` 2 == 0
            in all isLightSquare bishopPositions || all (not . isLightSquare) bishopPositions
    in
        -- Check for King vs. King
        length pieces == 2 && all (\(_, _, p) -> case p of King _ -> True; _ -> False) pieces
        -- King and Bishop vs. King
        || length pieces == 3 && count (Bishop) == 1
        -- King and Knight vs. King
        || length pieces == 3 && count (Knight) == 1
        -- King and Bishop vs. King and Bishop (same-colored bishops)
        || length pieces == 4 && count (Bishop) == 2 && bishopsSameColor

drawByTurnCount :: Game -> Bool
drawByTurnCount (turnCounter, _, _, _) = turnCounter <= 0

drawByThreefoldRepetition :: Game -> Bool
drawByThreefoldRepetition (_, currentTurn, pieces, boardHistory) =
    let currentState = (currentTurn, pieces)
    in length (filter (== currentState) boardHistory) >= 3

--Check if a move puts one side's king in check. Also used to make sure you can't move a piece that is pinned to your king
--causeCheck White checks if a move will put the White king in check.
causeCheck :: Game -> Move -> Side -> Bool
causeCheck game move side = 
    let newGame = makeMove game move 
    in inCheck newGame side

-- Check if a specific side is currently in check
inCheck :: Game -> Side -> Bool
inCheck game currentSide = 
    let (x, y) = getKingPosition game currentSide in
      checkLineDiag (chr(ord (x)+1), y+1) (1 ,  1) game currentSide ||
      checkLine     (chr(ord (x)+1), y  ) (1 ,  0) game currentSide ||
      checkLineDiag (chr(ord (x)+1), y-1) (1 , -1) game currentSide ||
      checkLine     (chr(ord (x)  ), y-1) (0 , -1) game currentSide ||
      checkLineDiag (chr(ord (x)-1), y-1) (-1, -1) game currentSide ||
      checkLine     (chr(ord (x)-1), y  ) (-1,  0) game currentSide ||
      checkLineDiag (chr(ord (x)-1), y+1) (-1,  1) game currentSide ||
      checkLine     (chr(ord (x)  ), y+1) (0 ,  1) game currentSide ||
      checkPawn (chr(ord (x)+1), y + (if currentSide == White then 1 else -1)) game currentSide ||
      checkPawn (chr(ord (x)-1), y + (if currentSide == White then 1 else -1)) game currentSide ||
        checkKing (x, y) game currentSide ||
      checkKnights (chr(ord (x)), y) game currentSide

checkLine :: Position -> (Int, Int)-> Game -> Side -> Bool
checkLine (x, y) (xAdd, yAdd) game side = 
    if outOfBounds (x, y) then False else
        let otherSide = otherside side in case getPiece game (x, y) of 
            Just (_, v, Queen) -> v == otherSide
            Just (_, v, Rook _) -> v == otherSide
            Just (_, _, _) -> False
            otherwise -> checkLine (chr(ord(x) + xAdd), y + yAdd) (xAdd, yAdd) game side
checkLineDiag :: Position -> (Int, Int)-> Game -> Side -> Bool
checkLineDiag (x, y) (xAdd, yAdd) game side = 
    if outOfBounds (x, y) then False else
        let otherSide = otherside side in case getPiece game (x, y) of 
            Just (_, v, Queen) -> v == otherSide
            Just (_, v, Bishop) -> v == otherSide
            Just (_, _, _) -> False
            otherwise -> checkLineDiag (chr(ord(x) + xAdd), y + yAdd) (xAdd, yAdd) game side


checkPawn :: Position -> Game -> Side -> Bool
checkPawn (x, y) game side = 
    if outOfBounds (x, y) then False else
        if ord(x) > 72 || ord(x) < 65 then False else 
            let otherSide = otherside side in 
                case getPiece game (x, y) of 
                    Just (_, v, Pawn _) -> v == otherSide
                    otherwise -> False

checkKing :: Position -> Game -> Side -> Bool
checkKing (x, y) game side = 
    let (u, v) = getKingPosition game (if side == White then Black else White)
    in (abs(ord(x) - ord(u)) <= 1 && abs(y-v) <= 1)

checkKnights :: Position -> Game -> Side -> Bool
checkKnights (x, y) game current = 
    checkKnight ([(chr(ord(x) + z), y + u) | z <- [1, -1], u <- [2, -2]] ++ 
        [(chr(ord(x) + z), y + u) | z <- [2, -2], u <- [1, -1]]) game otherSide
    where otherSide = otherside current

checkKnight :: [Position] -> Game -> Side -> Bool
checkKnight [] _ _ = False
checkKnight ((x, y): xs) game side = 
    if outOfBounds (x, y) then checkKnight xs game side else
        case getPiece game (x, y) of 
            Just ((x, y), v, Knight) -> if v == side then True else checkKnight xs game side
            otherwise -> checkKnight xs game side

getKingPosition :: Game -> Side -> Position
getKingPosition (_, _, pieces, _) side =
    case find (\(_, s, pt) -> s == side && case pt of King _ -> True; _ -> False) pieces of
        Just (pos, _, _) -> pos
        Nothing -> error "King not found on the board"

--Takes a position and returns the current piece at that position, if any
getPiece :: Game -> Position -> Maybe Piece
getPiece (_, _, pieces, _) pos = find (\(pPos, _, _) -> pPos == pos) pieces
