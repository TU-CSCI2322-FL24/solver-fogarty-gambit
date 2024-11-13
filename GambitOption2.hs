import Prelude
import Data.Maybe
import Data.List (find)
import Data.Char
data Side = White | Black deriving (Show, Eq)

--              enpassantable   hasMoved                               hasMoved
data PieceType = Pawn Bool | Rook Bool | Knight | Bishop | Queen | King Bool deriving (Show, Eq)

data Winner = Win Side | Tie deriving (Show, Eq)

type CurrentTurn = Side

--type Label = Char -- 'a' to 'h' to label each piece by the column letter they start on. This might not be neccessary
type Position = (Char, Int) -- 'A' to 'H' for columns, 1 to 8 for rows

type Piece = (Position, Side, PieceType)

type Move = (Piece, Position) --Maybe move should take a position instead of a piece

type Game = (CurrentTurn, [Piece], Int)

getSnd :: (a, b, c) -> b
getSnd (a, b, c) = b


reverseList :: [a] -> [a]
reverseList [] = []
reverseList lst = let
    aux [] out = out
    aux (x:xs) out = aux xs (x:out)
    in aux lst []

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
pieceToChar _ = error "That piece doesn't exist"

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

--testGame = (White, [(('A', 5), White, Pawn True)])

displayBoard :: Game -> Side -> IO ()
displayBoard game pov = let
    --Association list of every piece on the board
    positionsAndPieces = [(pos, (side, pieceType)) | (pos, side, pieceType) <- getSnd game]

    (missingWhitePieces, missingBlackPieces) = getMissingPieces (getSnd game)
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
        in if isNothing maybePiece
            --                                            Prints the row numbers on the side of the board
            then aux (out ++ [square] ++ "|" ++  "\n" ++ (if (snd (incrementPos currentPos) == 8) then " " else show (snd (incrementPos currentPos))) ++ (if currentPos == ('H', 1) then " " else "|")) (board) (incrementPos currentPos) 
            else aux (out ++ [pieceToChar (currentPos, fst (fromJust maybePiece), snd (fromJust maybePiece))] ++ "|" ++  "\n" ++ (if (snd (incrementPos currentPos) == 8) then " " else show (snd (incrementPos currentPos))) ++ (if currentPos == ('H', 1) then " " else "|")) (board) (incrementPos currentPos)

    aux out (square:rows) currentPos = let
        maybePiece = lookup currentPos positionsAndPieces
        in if isNothing maybePiece 
            then aux (out ++ [square]) (rows) (incrementPos currentPos) 
            else aux (out ++ [pieceToChar (currentPos, fst (fromJust maybePiece), snd (fromJust maybePiece))]) (rows) (incrementPos currentPos)
    --                                                        Labels for the ranks                                                   The board is printed backwards for black              Labels for the ranks, backwards
    displayStr = if pov == White then ("\n" ++ missingWhiteStr ++ "\n  ________ \n8|" ++ (aux "" emptyBoard ('A', 8)) ++ "\b ‾‾‾‾‾‾‾‾ \n  ABCDEFGH\n\n" ++ missingBlackStr ++ "\n") else ("\n" ++ missingBlackStr ++ "\n ________ " ++ (reverseList (((aux "" emptyBoard ('A', 8)) ++ ""))) ++ "|8\n ‾‾‾‾‾‾‾‾ \n HGFEDCBA\n\n" ++ missingWhiteStr ++ "\n")
    in putStrLn displayStr

--use putStrLn in the shell to print this string

getMissingPieces :: [Piece] -> ([Piece], [Piece])
getMissingPieces [] = ([], [])
getMissingPieces pieces = let
    blackPieces = [piece | (pos, color, piece) <- pieces, color == Black]
    whitePieces = [piece | (pos, color, piece) <- pieces, color == White]
    (missingWhitePawns, missingBlackPawns) = ((replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == White, (piece == Pawn True || piece == Pawn False)] - length [piece | piece <- whitePieces, (piece == Pawn True || piece == Pawn False)]) (('A', 1), White, Pawn False)), replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == Black, (piece == Pawn True || piece == Pawn False)] - length [piece | piece <- blackPieces, (piece == Pawn True || piece == Pawn False)]) (('A', 1), Black, Pawn False))
    (missingWhiteRooks, missingBlackRooks) = ((replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == White, (piece == Rook True || piece == Rook False)] - length [piece | piece <- whitePieces, (piece == Rook True || piece == Rook False)]) (('A', 1), White, Rook False)), replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == Black, (piece == Rook True || piece == Rook False)] - length [piece | piece <- blackPieces, (piece == Rook True || piece == Rook False)]) (('A', 1), Black, Rook False))
    (missingWhiteKings, missingBlackKings) = ((replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == White, (piece == King True || piece == King False)] - length [piece | piece <- whitePieces, (piece == King True || piece == King False)]) (('A', 1), White, King False)), replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == Black, (piece == King True || piece == King False)] - length [piece | piece <- blackPieces, (piece == King True || piece == King False)]) (('A', 1), Black, King False))
    (missingWhiteQueens, missingBlackQueens) = ((replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == White, piece == Queen] - length [piece | piece <- whitePieces, piece == Queen]) (('A', 1), White, Queen)), replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == Black, piece == Queen] - length [piece | piece <- blackPieces, piece == Queen]) (('A', 1), Black, Queen))
    (missingWhiteBishops, missingBlackBishops) = ((replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == White, piece == Bishop] - length [piece | piece <- whitePieces, piece == Bishop]) (('A', 1), White, Bishop)), replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == Black, piece == Bishop] - length [piece | piece <- blackPieces, piece == Bishop]) (('A', 1), Black, Bishop))
    (missingWhiteKnights, missingBlackKnights) = ((replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == White, piece == Knight] - length [piece | piece <- whitePieces, piece == Knight]) (('A', 1), White, Knight)), replicate (length [piece | (pos, color, piece) <- getSnd initialGame, color == Black, piece == Knight] - length [piece | piece <- blackPieces, piece == Knight]) (('A', 1), Black, Knight))
    in (missingWhitePawns ++ missingWhiteKnights ++ missingWhiteBishops ++ missingWhiteRooks ++ missingWhiteQueens ++ missingWhiteKings, missingBlackPawns ++ missingBlackKnights ++ missingBlackBishops ++ missingBlackRooks ++ missingBlackQueens ++ missingBlackKings)


allPositions :: [Position]
allPositions = [(x, y) | x <- ['A'..'H'], y <- [1..8]]

initialGame :: Game
initialGame = (White, initialPieces, 0) where
    initialPieces = initialPawns ++ initialRooks ++ initialKnights ++ initialBishops ++ initialQueens ++ initialKings where
        initialPawns = [ ((col, if side == White then 2 else 7), side, Pawn False) | side <- [White, Black], col <- ['A'..'H']]
        initialRooks = [ ((col, if side == White then 1 else 8), side, Rook False) | side <- [White, Black], col <- ['A','H']]
        initialKnights = [ ((col, if side == White then 1 else 8), side, Knight) | side <- [White, Black], col <- ['B','G']]
        initialBishops = [ ((col, if side == White then 1 else 8), side, Bishop) | side <- [White, Black], col <- ['C','F']]
        initialQueens = [ (('D', 1), White, Queen), (('D', 8), Black, Queen)]
        initialKings = [ (('E', 1), White, King False), (('E', 8), Black, King False)]

-- Calculates the difference in material between the input side and the other side
getMaterial :: Game -> Side -> Int
getMaterial (_, pieces, _) side =
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

    in max 0 (sideMaterial - otherMaterial)

getMaterialWinner :: Game -> Maybe Side
getMaterialWinner game
    | whiteMaterial > blackMaterial = Just White
    | blackMaterial > whiteMaterial = Just Black
    | otherwise               = Nothing
  where
    whiteMaterial = getMaterial game White
    blackMaterial = getMaterial game Black

getWinner :: Game -> Maybe Winner
getWinner game =
    let (currentTurn, _, _) = game
        otherSide = if currentTurn == White then Black else White 
        in
            if checkMate game currentTurn then Just (Win otherSide)
            else if checkMate game otherSide then Just (Win currentTurn)
                 else if staleMate game || drawByMaterial game || drawBy50MoveRule game then Just Tie
                    else Nothing

--Get every possible move given a specfic piece
legalPieceMoves :: Game -> Piece -> [Move]
legalPieceMoves game (pos, side, pieceType) =
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

                captureMoves = concat
                    [ if canCaptureLeft then [((pos, side, Pawn False), leftDiag)] else []
                    , if canCaptureRight then [((pos, side, Pawn False), rightDiag)] else []
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
allLegalMoves :: Game -> Side -> [Move]
allLegalMoves game side =
    let sidePieces = filter (\(_, s, _) -> s == side) (getSnd game)
        allMoves = concatMap (legalPieceMoves game) sidePieces
        legalMoves = filter (\move -> not (causeCheck game move side)) allMoves
        kingSideCastle :: [Move] 
        kingSideCastle =
            if side == White then
                if inCheck game White == False && getPiece game ('F', 1) == Nothing && getPiece game ('G', 1) == Nothing && getPiece game ('E', 1) == Just (('E', 1), White, King False) && getPiece game ('H', 1) == Just (('H', 1), White, Rook False) then
                    if causeCheck game ((('E', 1), White, King False), ('F', 1)) side == False && causeCheck game ((('E', 1), White, King False), ('G', 1)) side == False then
                        [((('E', 1), White, King True), ('G', 1))] 
                    else []
                else []
            else if inCheck game Black == False && getPiece game ('F', 8) == Nothing && getPiece game ('G', 8) == Nothing && getPiece game ('E', 8) == Just (('E', 8), Black, King False) && getPiece game ('H', 8) == Just (('H', 8), Black, Rook False) then
                    if causeCheck game ((('E', 8), Black, King False), ('F', 8)) side == False && causeCheck game ((('E', 8), Black, King False), ('G', 8)) side == False then
                        [((('E', 8), Black, King True), ('G', 8))] 
                    else []
                else []
        queenSideCastle :: [Move]
        queenSideCastle =
            if side == White then
                if inCheck game White == False && getPiece game ('D', 1) == Nothing && getPiece game ('C', 1) == Nothing && getPiece game ('B', 1) == Nothing && getPiece game ('E', 1) == Just (('E', 1), White, King False) && getPiece game ('A', 1) == Just (('A', 1), White, Rook False) then
                    if causeCheck game ((('E', 1), White, King False), ('D', 1)) side == False && causeCheck game ((('E', 1), White, King False), ('C', 1)) side == False then
                        [((('E', 1), White, King True), ('C', 1))] 
                    else []
                else []
            else if inCheck game Black == False && getPiece game ('D', 8) == Nothing && getPiece game ('C', 8) == Nothing && getPiece game ('B', 8) == Nothing && getPiece game ('E', 8) == Just (('E', 8), Black, King False) && getPiece game ('A', 8) == Just (('A', 8), Black, Rook False) then
                    if causeCheck game ((('E', 8), Black, King False), ('D', 8)) side == False && causeCheck game ((('E', 8), Black, King False), ('C', 8)) side == False then
                        [((('E', 8), White, King True), ('C', 8))] 
                    else []
                else []

        in legalMoves ++ kingSideCastle ++ queenSideCastle

makeMove :: Game -> Move -> Game
makeMove (side, positions, fiftyMoveCounter) (piece@(startPos, pieceSide, pieceType), endPos) =
    if side == pieceSide then
        if (piece, endPos) `elem` legalPieceMoves (side, positions, fiftyMoveCounter) piece -- && not (causeCheck (side, positions) ((startPos, pieceSide, pieceType), endPos) side)
        then 
            let 
                newSide = if side == White then Black else White
                -- Determine if this move is a capture or pawn move for the 50-move rule
                isCapture = isJust (getPiece (side, positions, fiftyMoveCounter) endPos)
                isPawnMove = case pieceType of
                    Pawn _ -> True
                    _      -> False
                newFiftyMoveCounter = if isCapture || isPawnMove then 0 else fiftyMoveCounter + 1
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
                    else 
                        -- ChatGPT helped write this part about modifying the pawn, rook and king booleans after a move
                        map (\p@(pos, s, pt) -> 
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
            in (newSide, newPositions, newFiftyMoveCounter)
        else error "Such move is not allowed"
    else error ("It is not " ++ show (if side == White then Black else White) ++ "'s turn")

--Quick move calls makeMove but uses two positions instead of the longer alternative
quickMove :: Game -> Position -> Position -> Game
quickMove game startPos endPos =
    case getPiece game startPos of
        -- Handle the pawn case
        Just (pos, side, Pawn _) -> 
            let isTwoSquareMove = abs (snd startPos - snd endPos) == 2
                move = if isTwoSquareMove
                       then ((pos, side, Pawn True), endPos)  -- Set Pawn True for two-square move
                       else ((pos, side, Pawn False), endPos) -- Keep Pawn False for one-square move
            in makeMove game move
        
        -- Handle the rook case
        Just (pos, side, Rook _) -> 
            let move = ((pos, side, Rook True), endPos)  -- Set Rook True after it moves
            in makeMove game move

        -- Handle the king case
        Just (pos, side, King _) -> 
            let move = ((pos, side, King True), endPos)  -- Set King True after it moves
            in makeMove game move

        -- Default case for other pieces
        Just piece -> 
            let move = (piece, endPos)
            in makeMove game move

        Nothing -> error "No piece at starting position"

--Check if a side is currently in the state of checkmate, aka that side is in check and has no more legal moves to be made
checkMate :: Game -> Side -> Bool
checkMate game side =
    length (allLegalMoves game side) == 0 && inCheck game side

--Check if the current turn of the game as no moves but is also not in check
staleMate :: Game -> Bool
staleMate game =
    let (currentTurn, _, _) = game in
        length (allLegalMoves game currentTurn) == 0 && not (inCheck game currentTurn)

drawByMaterial :: Game -> Bool
drawByMaterial game =
    let (currentTurn, pieces, _) = game
        currentTurnPieces = filter (\(_, side, _) -> side == currentTurn) pieces
        otherSidePieces = filter (\(_, side, _) -> side /= currentTurn) pieces in
            length currentTurnPieces == 1 || length otherSidePieces == 1

drawBy50MoveRule :: Game -> Bool
drawBy50MoveRule (_, _, fiftyMoveCounter) = fiftyMoveCounter >= 100

--Check if a move puts one side's king in check. Also used to make sure you can't move a piece that is pinned to your king
--causeCheck White checks if a move will put the White king in check.
causeCheck :: Game -> Move -> Side -> Bool
causeCheck game move side = 
    let newGame = makeMove game move in
    inCheck newGame side

-- Check if a specific side is currently in check
inCheck :: Game -> Side -> Bool
inCheck game currentSide =
    let opponentSide = if currentSide == White then Black else White
        opponentPieces = filter (\(_, s, _) -> s == opponentSide) (getSnd game)
        opponentMoves = concatMap (legalPieceMoves game) opponentPieces
        kingPosition = getKingPosition game currentSide
    in any (\(_, pos) -> pos == kingPosition) opponentMoves

--Takes a piece and returns it's current position
getPosition :: Game -> Piece -> Position
getPosition (_, pieces, _) piece =
    case lookup piece (map (\p@(pos, _, _) -> (p, pos)) pieces) of
        Just pos -> pos
        Nothing -> error "Piece not found on the board"

getKingPosition :: Game -> Side -> Position
getKingPosition (_, pieces, _) side =
    case find (\(_, s, pt) -> s == side && case pt of King _ -> True; _ -> False) pieces of
        Just (pos, _, _) -> pos
        Nothing -> error "King not found on the board"

--Takes a position and returns the current piece at that position, if any
getPiece :: Game -> Position -> Maybe Piece
getPiece (_, pieces, _) pos = 
    case find (\(pPos, _, _) -> pPos == pos) pieces of
        Just piece -> Just piece
        Nothing    -> Nothing

--hi

{-
@#@@@@@@@@@@@@@@@@@@@@@@@#+@;##+'++####@@@@@@@@+#'+'+##+'+';;;+@@@@@@@@@@@@@@@@@@@@@@@@@@@
++####@@@@@@@@@@@@@@@@@@#+@+@##++#+###@#@@@##@@@@#####@+@#+''+@#@@@@@@@@@@@@@@@@@@@@@@####
++#+++###@@@@@@@@@@@@@@@@@@@@@##@###@@@#@@@@@@@###@##@#@@+@#@@@+@@@@@@@@@@@@@@@@@@###+####
+++#'++++#@@@@@@@@@@@@@@#####@@@@@@@@@@@#@@#@#####################@@@@@@@@@@@@@@@##+#++#++
+###++++++#@@@@@@@@@@@@#+#####++++++#+#+##+####+####################@@@@@@@@@@@@####+++###
++##+++++++#@@@@@@@@@###+####+++++#++++++++#+++#++###################@@@@@@@@@@@###+######
######+++++#@@@@@@@@###+####+#+++++++++++++++'+++++##############@####@@@@@@@@@###+####@##
@@@@@@@@++++##@@@@@##++####+++++++'+++++''+++++#++######+##############@@@@@@@#++###@@@@@@
#@#@###@@#+++#@@@@###++##++'##++'++'+++++'++'''++#+#####++##############@@@@@@#+##@@@####@
#########@#+++@@@@@##++###++#+##++'''+##'+++'+'+++#+#+##+++#++###########@@@@#++#@@#######
##########@+++@@@@###+++++++++++++'+++##++++'''''+++++++++++++++#########@@@@++#@@########
###########@+#@@@####''++'''++##+++++#+#+++'''''+''+++++++++++++##########@@@#+#@#########
###+#++####@++@@@###+''++'+'+++#++++###++++++''+''+++++++++'+'+++#+##+####@@@#+@@#########
#++++++#####+#@@###+'##+++++++#++++++#+++++++'+'+''''+'+++++''+++++#+######@@##@@#########
++++++++###++#@@####'++###++++++++++++++'+++++''''''++++++'+++++++++++#####@@@+#@#########
#+++++++##@++@#@+#+#++++##+'''+'++++++++++++++'+++''++++++'++++++++++#++###@@@#+@#########
#+++++++###++@@###+#+'++##++''+++++++++++++++++++''''++++++++++++++++++#####@@#+@@########
##++++++#@'+@@@++++''++#+++++++++#++++++++++++++++++''+++++++++++++++'+#####@@@++@########
++++++++##+#@;@+#++;''++++++++++###++++++++++++++++#++'+'++++++#+++++'+++###;#@#+@@#######
#+#+++###++#@@#++++'''+++#++++++####+##++++++'++++++'++++'+++++++++++++#####@@@@+#@#######
####++##@++@@@##+++'';'+++@@@@@@@@@#+++#+++#+++++'++++++#@@@@@@@@#+++++++####@@@#+@@######
###+####++#@@@+++++;;'''@@@@@@@@@@@@@@@#+##+#####+++#@@@@@@@@@@@@@@@+++++####@@@#+#@######
#######@'#@@@##+++'''''+@@@@@@@@@@@@@@@@@+++++#+++#@@@@@@@@@@@@@@@@@+''+++###@@@@++@@#####
#@@@@@##+#@@@#+#++';'';'@@@@@@@@@@@@@@@@#+++++++++#@@@@@@@@@@@@@@@@@'+++++++##@@@#+#@@@@#@
######+++@@@@++++#'''''+@@@@@@@@@@@@@@@#+'+++++++++#@@@@@@@@@@@@@@@++++'+++++#@@@@#++#####
#++#++++#@@@#++++''''+'+@@@@@##+'+'+++++'++++++++++++++####+##@@@@@+++++++'+###@@@@#++####
+++##++#@@@##++++;''+''+#@@#+++++++++++++++++#++++++++##+++++++#@@@+++++'++++###@@@####+++
##+##@@@@@@###++'''''''++++++++++##+'+++++++##+++++++++####++++++++++++++'''+###@@@@@@####
@@@@@@@@@@@####+'''''+++++++#@@@@@@@#++'+'++++++++++++@@@@@@@##+++++'+++++++++##@@@@@@@@@@
@@@@@@@@@@@####''''''''++###@@@@#@@@@@@'++++++++++++#@@#@@@@@@@#++''+++++++'++##@@@@@@@@@@
@@@@@@@@@@@####''';'''+'+##@@@@@@@@@@@@#+++++++++++#@@@@@@@@@@#@#+'++'++++++++##@@@@@@@@@@
@@@@@@@@@@####'''';;''+++##@@@@@@@@@@@@@++++++++++#@@@@@@@@###@@@#+++++#++++++##@@@@@@@@@@
@@@@@@@@@@###+';;'''''''+@@@@@@@;:,@@@@@#+++#+++++#@@:     :@@@#@@+++++++++#+++#@@@@@@@@@@
@@@@@@@@@@###';;';++''++#@@#:.`.:,:'@@@@@+++++++++;,``,:,:   `@@@@++++#+++++'#+#@@@@@@@@@@
@@@@@@@@@@##+';''''+++++#@@':.; ...,@@@@@###+++++#:,`; ...`   @@@@#++++#++++++##@@@@@@@@@@
@@@@@@@@@@+#''''''+'++++@@@':.;:@@@,@@@@@##++++++#:,``'@@.    @@@@@+++##+++++####@@@@@@@@@
@@@@@@@@@#+++''+'''+++++@@@#:,''@@@,@@@@@++#++#++#;,,'#@@;:   @@@@@#++#++++++####@@@@@@@@@
@@@@@@@@@++'++'''';'''++@@@@:,++@@':+@@@@##+++##+#;,.++@@;;   @@@@@#+++++++++#####@@@@@@@@
@@@@@@@#+++'+'+''''''''+#@@@;,.@@#+++@@@@##+++##+#::,'@##'    @@#@@#'+#+++++#+#+###@@@@@@@
@@@@@@#+#+'';';'''';'''+#@@@+:,``,  @@@@@@@@@@@@@@@+,.`';     @@@@@#++++++++#+#+####@@@@@@
@@@@@@+++''';';;';;;''+'+@@@@;:.`` `@@@@@@@@@@@@@@@@@@@`     @@@@@#++'#+#+##++++#####@@@@@
@@@@@#+'+'';'';;'';;'''++#@@@@@#@@@@@@@@@@@@@@@@@@@@@@@@`  :@@@@@#++++++++++'+##+####@@@@@
@@@@@+''+'''';;::;;;';''+++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##++''+++++++++#+#####@@@@
@@@@#+'''+'+';;;;;;;;''+++#@@@#@@@@@#@@@@@@@@@@@@@@@@@@@@#@@@@@####+++++#+++++++++####@@@@
@@@@#+''++'+'';';;'''++###+++##+##@@.@@@@@@@@@@@@@@@@@@'@@@#########++#++##++++++++###@@@@
@@@@#+'''''++''';;''++##+++++++++##@@@@@@@@@@@@@@@@@@@@@@###+###########+#++++++#++####@@@
@@@@#+'''''+'';';';'##+''++'+++++++#@+#@@@@@@@@@+'::+@@@####++#++###+####+#++#++##++###@@@
@@@@#+''+'''';;';;#++';''++'+'+'++++#@@@@@@@@@@@#@@@@@@##++#+++'+'+++++##++#+++++#++###@@@
@@@@##+'''';;;;;;#'';';''++'+'''''''''+@@@@@@@@@@@@@@###+++++++++++#+'++###++++##++++##@@@
@@@@#+'''''';;'';+'';'';''''''''+''''';+++#@@@@@@#++#++#+++++++++++++++++#++++++#+++##@@@@
@@@@##'''''';;;;#';;;;';''';'''''+';;''+'++++++++++++++++++++'+++++''+''++#++#+++#++##@@@@
@@@@@#+++';''';'';;;;@#;'''';'';;';';;;''''''+#''''++++'+++++'+'+''''@''++++++++##+###@@@@
@@@@@#'+++''';''';:;:@#;''@@';;;';;'';;;''''''#''''''''+''''++++@@'''@+'''+#++++##+###@@@@
@@@@@##+++;:';'+';::;;;;;;;'';;';'';'''';'''''+'''''+''''';'+'++''''''''++'+++++#++##@@@@@
@@@@@@##+;;;;'+';::;::::;;:';;''''''';''';'''''';'''''''''''''''''''''''#'''+#+##+###@@@@@
@@@@@@##+'''''''::::;;':;::;''''''''''''''''''''''+''''';'''''''''''+'''''''###+####@@@@@@
@@@@@@@##++'+'+':;';;:@@:;:;'''''''''''''''''''''''''''';;''''''''''@@'''''+#######@@@@@@@
@@@@@@@@##+++++';';;;;''':;;;''''''''''';;''''''''''''''''''''''''''''''''''######@@@@@@@@
@@@@@@@@@+#++##+'':';;;;;;';';''''+''''''''''''''+'''''''''''''''''''''''''+#####@@@@@@@@@
@@@@@@@@@@#+#+##+';'''';';;';'''''''''''''''''''''''''''''''''''''+'+''''+'####@@@@@@@@@@@
@@@@@@@@@@@#####+'''''+';';'''''''''''''''''+''+''+'''''''''''''+'+++++'''++###@@@@@@@@@@@
@@@@@@@@@@@@####+++++'';'++'+'''++'+'''''''''+''++++'+'''''''+++++++++'''+###@@@@@@@@@@@@@
@@@@@@@@@@@@@@+##++''+'''+++++'++++++++'''''++'+++'+'''''''++++++++'++++++#@#@@@@@@@@@@@@@
@@@@@@@@@@@@@@+'#++++'+++''+'++++++++++++'''++''++++''+++'''++++++++++#++####@@@@@@@@@@@@@
@@@@@@@@@@@@@@++#@++++++'++++++++++++++++++'+++++++++++'++++++++++++#+##+####@@@@@@@@@@@@@
@@@@@@@@@@@@@@+++@@#++++++++++++++++++++++++++++++'++++++++++++#++#####@#####@@@@@@@@@@@@@
@@@@@@@@@@@@@@+'#@@@###+###+++++#+#++++++++++++++++++++++++++###+######@#####@@@@@@@@@@@@@
@@@@@@@@@@@@@@+'+#@@@@##@########++++++++++++++++++++++############@#@@#@##++@@@@@@@@@@@@@
@@@@@@@@@@@@@@++##@@@@@@@@+#@@@@###############+#+########@@##@+'###@##@##+#+@@@@@@@@@@@@@
@@@@@@@@@@@@@@+++##@@@@@@@'#@@@@@@@@@@@@@@@@@@@@@@@@@@@#:`@@@#@#'@@@@@@@##++#@@@@@@@@@@@@@
@@@@@@@@@@@@@@+#+###@@@@@@'#@++'+'+'@@@@@@@@@@@@#+'';##'+#''''@''+@@@@@#####+@@@@@@@@@@@@@
@@@@@@@@@@@@@@++#+####@@@@:@@,:;;;;;##+'''#''';:'''''+@';;;;;:@;;+@@@@########@@@@@@@@@@@@
@@@@@@@@@@@@@@##++++++##@@;@@.:::::,;':;;;;;@@'@;::;;;:;,::::,@:,'@@##########@@@@@@@@@@@@
@@@@@@@@@@@@@@+#++'''+######@.:::,,,@+::::,,@@@@::,:,:@#,,,,,.@@@######++#####@@@@@@@@@@@@
@@@@@@@@@@@@@@+'+''++'''+########;.`@',,,.,.@@@@;,,,..@@,:'@@@##########++++#@@@@@@@@@@@@@
@@@@@@@##@@@@@++'+++;'''++++###########@@#@#####@#@@@##@########+#+++++++##+#@@@@@@@@@@@@@
@@@#########@@#++'++'';'++++++###############+#@##@#@##+##++#+++++++'+++++#+###########@@@
@#+##########@+++''+''++'++++'++++++###+###++########+++##+++++++++++++##+#+##############
###++######@@@#++''++''''+#'+#+++###+'+++#++++#++++++++#++'++++++++++++##+################
+++#####+##@@@@#+''++'''+'+#+++#++++++++++++++#++++++++++#++'++++++++###+#################
+++#########@@@#+'++++++''+'#++++'++++++'+++++#++++++++'+++++++#+++#+#######@#############
+#++####@#@#@@@@#++'+++''+'+'+'++'++++++'+++++++''+++++++++++++###+##+#####@@########+####
+++++#+####@@@@@@#++#+++#++++++++''+++++++++++++'''+++++++++++###++########@@########++##+
;#++#+#####@@@@@@@##+++++++++++++++++++++++##+++'+'+++++++++##+###########@@@@#########++#
++'+++#####@@@@@@@@#+##+#+++#++#+++++++++#+##+++++++++++++#+#############@@@@@#######+++++
'''++#++##@@@@@@@@@@###+#+###+##+++++++++++###++++++#++++################@@@@@######++++''
;''+++++###@@@@@@@@@#############+###+++#+###++++##+++++#################@@@@@#####++++++'
;''++++#####@@@@@@@###.############++++#######+++++#+#####################@+@@@#+#++'+++'+
+;''+'+####@@@@@@@####+@+###########+##########+++############@@###@###+##@@@@@###++++++''
@#'''+++###@@@@@@+###++.#@@###########++################@#@##@@@@@'@######@@@@@#++#++++'+@
-}
