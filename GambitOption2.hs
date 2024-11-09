import Prelude
import Data.Maybe
data Side = White | Black deriving (Show, Eq)

--              enpassantable   hasMoved                               hasMoved
data PieceType = Pawn Bool | Rook Bool | Knight | Bishop | Queen | King Bool deriving (Show, Eq)

type CurrentTurn = Side

--type Label = Char -- 'a' to 'h' to label each piece by the column letter they start on. This might not be neccessary
type Position = (Char, Int) -- 'A' to 'H' for columns, 1 to 8 for rows

type MoveCount = Int
--type HasMoved = Bool    <- maybe bool here?

type Piece = (Side, PieceType, MoveCount)

type Move = (Piece, Position) --Maybe move should take a position instead of a piece

type Game = (CurrentTurn, [(Position, Piece)])


reverseList :: [a] -> [a]
reverseList [] = []
reverseList lst = let
    aux [] out = out
    aux (x:xs) out = aux xs (x:out)
    in aux lst []

pieceToChar :: Piece -> Char
pieceToChar (Black, King _, _) = '\x2654'
pieceToChar (White, King _, _) = '\x265A'
pieceToChar (Black, Queen, _) = '\x2655'
pieceToChar (White, Queen, _) = '\x265B'
pieceToChar (Black, Bishop, _) = '\x2657'
pieceToChar (White, Bishop, _) = '\x265D'
pieceToChar (Black, Knight, _) = '\x2658'
pieceToChar (White, Knight, _) = '\x265E'
pieceToChar (Black, Rook _, _) = '\x2656'
pieceToChar (White, Rook _, _) = '\x265C'
pieceToChar (Black, Pawn _, _) = '\x2659'
pieceToChar (White, Pawn _, _) = '\x265F'
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
displayBoard :: Game -> Side -> String
displayBoard game pov = let
    positionsAndPieces = [(pos, piece) | (pos, piece) <- snd game]

    aux :: String -> [Char] -> Position -> String
    aux out [] currentPos = out
    --case for if this is the last square in the row, need to add a newline
    aux out (square:board) currentPos@('H', num) = let 
        maybePiece = lookup currentPos positionsAndPieces
        in if isNothing maybePiece
            then aux (out ++ [square] ++ "\n") (board) (incrementPos currentPos) 
            else aux (out ++ [pieceToChar (fromJust maybePiece)] ++ "\n") (board) (incrementPos currentPos)

    aux out (square:rows) currentPos = let
        maybePiece = lookup currentPos positionsAndPieces
        in if isNothing maybePiece 
            then aux (out ++ [square]) (rows) (incrementPos currentPos) 
            else aux (out ++ [pieceToChar (fromJust maybePiece)]) (rows) (incrementPos currentPos)

    in if pov == White then '\n':(aux "" emptyBoard ('A', 8)) else (reverseList (aux "" emptyBoard ('A', 8))) ++ "\n"

--use putStr in the shell to print this string


allPositions :: [Position]
allPositions = [(x, y) | x <- ['A'..'H'], y <- [1..8]]

initialGame :: Game
initialGame = (White, initialPieces) where
    initialPieces = initialPawns ++ initialRooks ++ initialKnights ++ initialBishops ++ initialQueens ++ initialKings where
        initialPawns = [ ((col, if side == White then 2 else 7), (side, Pawn False, 0)) | side <- [White, Black], col <- ['A'..'H']]
        initialRooks = [ ((col, if side == White then 1 else 8), (side, Rook False, 0)) | side <- [White, Black], col <- ['A','H']]
        initialKnights = [ ((col, if side == White then 1 else 8), (side, Knight, 0)) | side <- [White, Black], col <- ['B','G']]
        initialBishops = [ ((col, if side == White then 1 else 8), (side, Bishop, 0)) | side <- [White, Black], col <- ['C','F']]
        initialQueens = [ (('D', 1), (White, Queen, 0)), (('D', 8), (Black, Queen, 0))]
        initialKings = [ (('E', 1), (White, King False, 0)), (('E', 8), (Black, King False, 0))]

getScore :: Game -> Side -> Int --Gets the difference in material of the input side vs the other side
getScore (_, allPieces) side =
    let sidePieces = [pieceType | (_, (pieceSide, pieceType, _)) <- allPieces, pieceSide == side]
        otherSidePieces = [pieceType | (_, (pieceSide, pieceType, _)) <- allPieces, pieceSide /= side]
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
