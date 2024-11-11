import Prelude
import Data.Maybe
import Data.List (find)
data Side = White | Black deriving (Show, Eq)

--              enpassantable   hasMoved                               hasMoved
data PieceType = Pawn Bool | Rook Bool | Knight | Bishop | Queen | King Bool deriving (Show, Eq)

type CurrentTurn = Side

--type Label = Char -- 'a' to 'h' to label each piece by the column letter they start on. This might not be neccessary
type Position = (Char, Int) -- 'A' to 'H' for columns, 1 to 8 for rows

--type MoveCount = Int
--type HasMoved = Bool    <- maybe bool here?

{- type Piece = (Side, PieceType)

type Move = (Position, Position) --Maybe move should take a position instead of a piece

type Game = (CurrentTurn, [(Position, Piece)]) -}

-- old types ^^^

type Piece = (Position, Side, PieceType)

type Move = (Piece, Position) --Maybe move should take a position instead of a piece

type Game = (CurrentTurn, [Piece])


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

{- displayBoard :: Game -> Side -> String
displayBoard game pov = let
    --Association list of every piece on the board
    positionsAndPieces = [(pos, piece) | (pos, piece) <- snd game]

    --Builds the board line by line, recursively
    aux :: String -> [Char] -> Position -> String
    --Return the completed board
    aux out [] currentPos = out

    --Case for the last (furthest right) square of each row.
    aux out (square:board) currentPos@('H', num) = let 
        maybePiece = lookup currentPos positionsAndPieces
        --Checks if we need to print a piece char or a square char
        in if isNothing maybePiece
            --                                            Prints the row numbers on the side of the board
            then aux (out ++ [square] ++ "|" ++  "\n" ++ (if (snd (incrementPos currentPos) == 8) then " " else show (snd (incrementPos currentPos))) ++ (if currentPos == ('H', 1) then " " else "|")) (board) (incrementPos currentPos) 
            else aux (out ++ [pieceToChar (fromJust maybePiece)] ++ "|" ++  "\n" ++ (if (snd (incrementPos currentPos) == 8) then " " else show (snd (incrementPos currentPos))) ++ (if currentPos == ('H', 1) then " " else "|")) (board) (incrementPos currentPos)

    aux out (square:rows) currentPos = let
        maybePiece = lookup currentPos positionsAndPieces
        in if isNothing maybePiece 
            then aux (out ++ [square]) (rows) (incrementPos currentPos) 
            else aux (out ++ [pieceToChar (fromJust maybePiece)]) (rows) (incrementPos currentPos)
    --                                                        Labels for the ranks                                                   The board is printed backwards for black              Labels for the ranks, backwards
    in if pov == White then ('\n':"  ________ \n8|" ++ (aux "" emptyBoard ('A', 8)) ++ "\b ‾‾‾‾‾‾‾‾ \n  ABCDEFGH") else ('\n':" ________ " ++ (reverseList (((aux "" emptyBoard ('A', 8)) ++ ""))) ++ "|8\n ‾‾‾‾‾‾‾‾ \n HGFEDCBA")

--use putStr in the shell to print this string -}


allPositions :: [Position]
allPositions = [(x, y) | x <- ['A'..'H'], y <- [1..8]]

initialGame :: Game
initialGame = (White, initialPieces) where
    initialPieces = initialPawns ++ initialRooks ++ initialKnights ++ initialBishops ++ initialQueens ++ initialKings where
        initialPawns = [ ((col, if side == White then 2 else 7), side, Pawn False) | side <- [White, Black], col <- ['A'..'H']]
        initialRooks = [ ((col, if side == White then 1 else 8), side, Rook False) | side <- [White, Black], col <- ['A','H']]
        initialKnights = [ ((col, if side == White then 1 else 8), side, Knight) | side <- [White, Black], col <- ['B','G']]
        initialBishops = [ ((col, if side == White then 1 else 8), side, Bishop) | side <- [White, Black], col <- ['C','F']]
        initialQueens = [ (('D', 1), White, Queen), (('D', 8), Black, Queen)]
        initialKings = [ (('E', 1), White, King False), (('E', 8), Black, King False)]

getScore :: Game -> Side -> Int --Gets the difference in material of the input side vs the other side
getScore (_, allPieces) side =
    let sidePieces = [pieceType | (_, pieceSide, pieceType) <- allPieces, pieceSide == side]
        otherSidePieces = [pieceType | (_, pieceSide, pieceType) <- allPieces, pieceSide /= side]
        sideMaterial = sum [case pieceType of
               Pawn bool -> 1
               Rook bool  -> 5
               Bishop -> 3
               Knight -> 3
               Queen  -> 9
               King bool  -> 0
           | pieceType <- sidePieces]
        otherSideMaterial = sum [case pieceType of
               Pawn bool  -> 1
               Rook bool  -> 5
               Bishop -> 3
               Knight -> 3
               Queen  -> 9
               King bool  -> 0
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
                
                --possibleMoves = singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves
                --in filter (\move -> causeCheck game move side == False) possibleMoves

                -- ^^^^^^
                --Once allLegalMoves is defined we can include making sure that each move doesnt put our own king in check

            in singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves
        _ -> []

--Get every possible move for a side
allLegalMoves :: Game -> Side -> [Move]
allLegalMoves = undefined

--Update the game after a move is made
makeMove :: Game -> Move -> Game
makeMove (side, positions) (piece@(startPos, pieceSide, pieceType), endPos) =
    if (piece, endPos) `elem` legalPieceMoves (side, positions) piece
    then 
        let 
            newSide = if side == White then Black else White
            newPositions = map (\p@(pos, s, pt) -> if pos == startPos then (endPos, s, pt) else p)
                                (filter (\(pos, _, _) -> pos /= endPos) positions)
        in (newSide, newPositions)
    else error "Such move is not allowed"

--Check if a move puts one side's king in check. Also used to make sure you can't move a piece that is pinned to your king
--causeCheck White checks if a move will put the White king in check.
causeCheck :: Game -> Move -> Side -> Bool
causeCheck game move side = 
    let newGame = makeMove game move in
    inCheck newGame side

--Check if a specific side is currently in check
inCheck :: Game -> Side -> Bool
inCheck game currentSide =
    let opponentSide = if currentSide == White then Black else White
        opponentMoves = allLegalMoves game opponentSide
        kingPosition = getKingPosition game currentSide
    in
        --Checks to see if the king's position exists as a valid move for any of the opponent's pieces
        any (\(_, pos) -> pos == kingPosition) opponentMoves

--Takes a piece and returns it's current position
getPosition :: Game -> Piece -> Position
getPosition (_, pieces) piece =
    case lookup piece (map (\p@(pos, _, _) -> (p, pos)) pieces) of
        Just pos -> pos
        Nothing -> error "Piece not found on the board"

getKingPosition :: Game -> Side -> Position
getKingPosition (_, pieces) side =
    case find (\(_, s, pt) -> s == side && case pt of King _ -> True; _ -> False) pieces of
        Just (pos, _, _) -> pos
        Nothing -> error "King not found on the board"

--Takes a position and returns the current piece at that position, if any
getPiece :: Game -> Position -> Maybe Piece
getPiece (_, pieces) pos = 
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
