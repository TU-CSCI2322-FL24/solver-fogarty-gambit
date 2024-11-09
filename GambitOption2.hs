
data Side = White | Black deriving (Show, Eq)

--              enpassantable   hasMoved                               hasMoved
data PieceType = Pawn Bool | Rook Bool | Knight | Bishop | Queen | King Bool deriving (Show, Eq)

type CurrentTurn = Side

--type Label = Char -- 'a' to 'h' to label each piece by the column letter they start on. This might not be neccessary

type Position = (Char, Int) -- 'A' to 'H' for columns, 1 to 8 for rows

--type MoveCount = Int
--type HasMoved = Bool    <- maybe bool here?

type Piece = (Side, PieceType)

type Move = (Piece, Position) --Maybe move should take a position instead of a piece

type Game = (CurrentTurn, [(Position, Piece)])

allPositions :: [Position]
allPositions = [(x, y) | x <- ['A'..'H'], y <- [1..8]]

initialGame :: Game
initialGame = (White, initialPieces) where
    initialPieces = initialPawns ++ initialRooks ++ initialKnights ++ initialBishops ++ initialQueens ++ initialKings where
        initialPawns = [ ((col, if side == White then 2 else 7), (side, Pawn False)) | side <- [White, Black], col <- ['A'..'H']]
        initialRooks = [ ((col, if side == White then 1 else 8), (side, Rook False)) | side <- [White, Black], col <- ['A','H']]
        initialKnights = [ ((col, if side == White then 1 else 8), (side, Knight)) | side <- [White, Black], col <- ['B','G']]
        initialBishops = [ ((col, if side == White then 1 else 8), (side, Bishop)) | side <- [White, Black], col <- ['C','F']]
        initialQueens = [ (('D', 1), (White, Queen)), (('D', 8), (Black, Queen))]
        initialKings = [ (('E', 1), (White, King False)), (('E', 8), (Black, King False))]

getScore :: Game -> Side -> Int --Gets the difference in material of the input side vs the other side
getScore (_, allPieces) side =
    let sidePieces = [pieceType | (_, (pieceSide, pieceType)) <- allPieces, pieceSide == side]
        otherSidePieces = [pieceType | (_, (pieceSide, pieceType)) <- allPieces, pieceSide /= side]
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
legalPieceMoves :: Game -> Position -> [Move]
legalPieceMoves game pos =
    case getPiece game pos of
        Just (side, Pawn enPassantable) -> 
            let (col, row) = pos
                forwardOne = if side == White then row + 1 else row - 1
                forwardTwo = if side == White then row + 2 else row - 2
                leftDiag = (pred col, forwardOne)   -- Diagonal left
                rightDiag = (succ col, forwardOne)  -- Diagonal right
                leftPos = (pred col, row)           -- Directly left
                rightPos = (succ col, row)          -- Directly right
                isOnStartingRow = (side == White && row == 2) || (side == Black && row == 7)
                isPromotionRow = (side == White && forwardOne == 8) || (side == Black && forwardOne == 1)

                -- Check if the square directly forward is empty
                canMoveOne = getPiece game (col, forwardOne) == Nothing

                -- Check if both squares directly forward are empty for a two-square move
                canMoveTwo = getPiece game (col, forwardTwo) == Nothing

                -- Check if there’s an opponent’s piece on either diagonal for capture
                canCaptureLeft = case getPiece game leftDiag of
                    Just (otherSide, _) -> otherSide /= side
                    Nothing -> False

                canCaptureRight = case getPiece game rightDiag of
                    Just (otherSide, _) -> otherSide /= side
                    Nothing -> False

                -- Check if en passant is possible (en passant-able pawn directly to the left or right)
                enPassantLeft = case getPiece game leftPos of
                    Just (otherSide, Pawn True) -> otherSide /= side
                    _ -> False

                enPassantRight = case getPiece game rightPos of
                    Just (otherSide, Pawn True) -> otherSide /= side
                    _ -> False

                -- Define moves for each possible legal move
                promotionPieces = [Queen, Rook True, Bishop, Knight]

                singleMove = if canMoveOne then
                                if isPromotionRow 
                                then [((side, promotedPiece), (col, forwardOne)) | promotedPiece <- promotionPieces]
                                else [((side, Pawn enPassantable), (col, forwardOne))]
                             else []
                doubleMove = if isOnStartingRow && canMoveOne && canMoveTwo
                             then [((side, Pawn True), (col, forwardTwo))]  -- Mark as en passant-able
                             else []
                captureMoves = concat
                    [ if canCaptureLeft then [((side, Pawn enPassantable), leftDiag)] else []
                    , if canCaptureRight then [((side, Pawn enPassantable), rightDiag)] else []
                    ]
                enPassantMoves = concat
                    [ if enPassantLeft then [((side, Pawn enPassantable), leftDiag)] else []
                    , if enPassantRight then [((side, Pawn enPassantable), rightDiag)] else []
                    ]

                --possibleMoves = singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves
                --in filter (\move -> causeCheck game move side == False) possibleMoves

                -- ^^^^^^
                --Once allLegalMoves is defined we can include making sure that each move doesnt put our own king in check
                
                in singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves

        -- Other pieces will be added here
        _ -> []

--Get every possible move for a side
allLegalMoves :: Game -> Side -> [Move]
allLegalMoves = undefined

--Update the game after a move is made
makeMove :: Game -> Move -> Game
makeMove = undefined

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
        yourMoves = allLegalMoves game currentSide
        yourPieces = map fst yourMoves
        kingBool = (currentSide, King True) `elem` yourPieces --gets the correct bool of the king before it is called
        kingPosition = getPosition game (currentSide, King kingBool)
    in
        --Checks to see if the king's position exists as a valid move for any of the opponent's pieces
        any (\(_, pos) -> pos == kingPosition) opponentMoves

--Takes a piece and returns it's current position
getPosition :: Game -> Piece -> Position
getPosition (_, pieces) piece = 
    case lookup piece (map (\(pos, p) -> (p, pos)) pieces) of
        Just pos -> pos
        Nothing -> error "Piece not found on the board"

--Takes a position and returns the current piece at that position, if any
getPiece :: Game -> Position -> Maybe Piece
getPiece (_, pieces) position = 
    case lookup position pieces of
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
