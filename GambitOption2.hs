data Side = White | Black deriving (Show, Eq)

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

type CurrentTurn = Side

--type Label = Char -- 'a' to 'h' to label each piece by the column letter they start on. This might not be neccessary

type Position = (Char, Int) -- 'A' to 'H' for columns, 1 to 8 for rows

type MoveCount = Int
--type MoveCount = Bool    <- maybe bool here?

type Piece = (Side, PieceType, MoveCount)

type Move = (Piece, Position) --Maybe move should take a position instead of a piece

type Game = (CurrentTurn, [(Position, Piece)])

allPositions :: [Position]
allPositions = [(x, y) | x <- ['A'..'H'], y <- [1..8]]

initialGame :: Game
initialGame = (White, initialPieces) where
    initialPieces = initialPawns ++ initialRooks ++ initialKnights ++ initialBishops ++ initialQueens ++ initialKings where
        initialPawns = [ ((col, if side == White then 2 else 7), (side, Pawn, 0)) | side <- [White, Black], col <- ['A'..'H']]
        initialRooks = [ ((col, if side == White then 1 else 8), (side, Rook, 0)) | side <- [White, Black], col <- ['A','H']]
        initialKnights = [ ((col, if side == White then 1 else 8), (side, Knight, 0)) | side <- [White, Black], col <- ['B','G']]
        initialBishops = [ ((col, if side == White then 1 else 8), (side, Bishop, 0)) | side <- [White, Black], col <- ['C','F']]
        initialQueens = [ (('D', 1), (White, Queen, 0)), (('D', 8), (Black, Queen, 0))]
        initialKings = [ (('E', 1), (White, King, 0)), (('E', 8), (Black, King, 0))]

getScore :: Game -> Side -> Int --Gets the difference in material of the input side vs the other side
getScore (_, allPieces) side =
    let sidePieces = [pieceType | (_, (pieceSide, pieceType, _)) <- allPieces, pieceSide == side]
        otherSidePieces = [pieceType | (_, (pieceSide, pieceType, _)) <- allPieces, pieceSide /= side]
        sideMaterial = sum [case pieceType of
               Pawn   -> 1
               Rook   -> 5
               Bishop -> 3
               Knight -> 3
               Queen  -> 9
               King   -> 0
           | pieceType <- sidePieces]
        otherSideMaterial = sum [case pieceType of
               Pawn   -> 1
               Rook   -> 5
               Bishop -> 3
               Knight -> 3
               Queen  -> 9
               King   -> 0
           | pieceType <- otherSidePieces]
    in
        if sideMaterial > otherSideMaterial then sideMaterial - otherSideMaterial else 0

getWinner :: Game -> Maybe Side
getWinner game
    | whiteScore > blackScore = Just White
    | blackScore > whiteScore = Just Black
    | otherwise               = Nothing
  where
    whiteScore = getScore game White
    blackScore = getScore game Black

--Represent the current gameboard in a string
showGame :: Game -> String
showGame = undefined

--Get every possible move given a specfic piece
legalPieceMoves :: Game -> Piece -> [Move]
legalPieceMoves = undefined

--Get every possible move for all pieces
allLegalMoves :: Game -> [Move]
allLegalMoves = undefined

--Update the game after a move is made
makeMove :: Game -> Move -> Game
makeMove = undefined

--Check if a move puts one side's king in check. Also used to make sure you can't move a piece that is pinned to your king
causeCheck :: Game -> Move -> Side -> Bool
causeCheck = undefined

--Check if a specific side is currently in check
inCheck :: Game -> Side -> Bool
inCheck = undefined

getPiece :: Game -> Position -> Maybe Piece
getPiece = undefined

--hi