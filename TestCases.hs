module TestCases where
import Gambit 

mateInOneRookGame :: Game
mateInOneRookGame = (2, White, [(('A',3), White, Pawn False), (('F',2), White, Pawn False), (('G',3), White, Pawn False), (('H',2), White, Pawn False), (('G',7), White, Pawn False), (('C',3), White, Bishop), (('G',1), White, King True), (('E',7), White, Rook True), 
    (('A',5), Black, Pawn False), (('B',6), Black, Pawn False), (('F',7), Black, Pawn False), (('H',7), Black, Pawn False), (('E',8), Black, Rook True),(('G',6), Black, Bishop),(('G',8), Black, King True)],[])

mateInTwoRookGame :: Game
mateInTwoRookGame = (3, White, [(('A',3), White, Pawn False), (('H',2), White, Pawn False), (('A',5), White, Rook True), (('C',2), White, King True), 
    (('C',5), Black, Pawn False), (('G',7), Black, Pawn False), (('H',7), Black, Pawn False), (('E',5), Black, Queen), (('H',8), Black, King True)],[])

mateInOneQueenGame :: Game
mateInOneQueenGame = (2, White, [(('A',2), White, Pawn False), (('B',2), White, Pawn False), (('C',2), White, Pawn False), (('F',2), White, Pawn False), (('G',2), White, Pawn False), (('H',2), White, Pawn False),(('A',1), White, Rook False), (('H',1), White, Rook False),(('E',1), White, King False), (('C',3), White, Knight), (('C',4), White, Bishop),(('C',1), White, Bishop),(('F',3), White, Queen), 
    (('A',7), Black, Pawn False), (('B',7), Black, Pawn False), (('D',6), Black, Pawn False), (('G',7), Black, Pawn False), (('F',7), Black, Pawn False), (('H',7), Black, Pawn False), (('E',8), Black, King False), (('D',8), Black, Queen), (('G',8), Black, Knight), (('D',4), Black, Knight),(('C',8), Black, Bishop), (('F',8), Black, Bishop),(('A',8), Black, Rook False), (('H',8), Black, Rook False)],[])

mateInTwoKnightQueenGame :: Game
mateInTwoKnightQueenGame = (3, White, [(('A',2), White, Pawn False), (('B',2), White, Pawn False), (('E',5), White, Pawn False), (('F',4), White, Pawn False), (('G',2), White, Pawn False), (('H',2), White, Pawn False),(('A',1), White, Rook False), (('F',1), White, Rook True),(('H',1), White, King True), (('G',4), White, Knight), (('H',4), White, Queen), 
    (('A',6), Black, Pawn False), (('B',7), Black, Pawn False), (('C',6), Black, Pawn False), (('D',5), Black, Pawn False), (('F',7), Black, Pawn False), (('G',6), Black, Pawn False), (('H',7), Black, Pawn False),(('A',8), Black, Rook False), (('G',8), Black, King True), (('B',6), Black, Queen), (('D',2), Black, Knight), (('F',5), Black, Bishop), (('F',8), Black, Rook True)],[])

mateInThreeQueenGame :: Game
mateInThreeQueenGame = (4, White, [(('A',2), White, Pawn False), (('B',2), White, Pawn False), (('C',2), White, Pawn False), (('F',2), White, Pawn False), (('F',6), White, Pawn False),(('G',2), White, Pawn False), (('H',2), White, Pawn False),(('A',1), White, Rook False), (('H',1), White, Rook False),(('E',1), White, King False), (('F',3), White, Queen), 
    (('A',7), Black, Pawn False), (('B',7), Black, Pawn False), (('C',5), Black, Pawn False), (('C',4), Black, Pawn False), (('F',7), Black, Pawn False), (('H',7), Black, Pawn False), (('H',6), Black, Pawn False), (('G',8), Black, King True), (('D',8), Black, Queen), (('C',8), Black, Bishop), (('A',8), Black, Rook False), (('F',8), Black, Rook True)],[])

pawnUpGame :: Game
pawnUpGame = (2, White, [(('A',4), White, Pawn False), (('B',3), White, Pawn False), (('C',4), White, Pawn False), (('D',5), White, Pawn False), (('F',2), White, Pawn False), (('G',2), White, Pawn False), (('H',3), White, Pawn False), (('D',1), White, Rook True), (('H',1), White, King True),
    (('A',7), Black, Pawn False), (('B',7), Black, Pawn False), (('H',7), Black, Pawn False), (('B',2), Black, Rook True), (('G',7), Black, King True), (('G',6), Black, Bishop)],[])

queenCheckWinsRookGame :: Game
queenCheckWinsRookGame = (2, Black, [(('A',3), White, Pawn False), (('B',2), White, Pawn False), (('C',2), White, Pawn False), (('E',3), White, Pawn False), (('F',5), White, Pawn False), (('G',4), White, Pawn False),(('G',1), White, Rook True), (('C',1), White, King True), (('E',2), White, Queen),
    (('A',7), Black, Pawn False), (('B',7), Black, Pawn False), (('C',6), Black, Pawn False), (('E',4), Black, Pawn False),(('G',7), Black, Pawn False), (('F',7), Black, Pawn False), (('E',7), Black, King True), (('E',6), Black, Queen), (('D',8), Black, Rook True)],[])

hangingRookGame :: Game
hangingRookGame = (2, Black, [(('B',2), White, Pawn False), (('F',2), White, Pawn False), (('G',3), White, Pawn False), (('E',6), White, Rook True), (('C',1), White, King False), (('F',5), White, Bishop), 
    (('A',7), Black, Pawn False), (('B',7), Black, Pawn False), (('C',7), Black, Pawn False), (('A',1), Black, Knight), (('A',2), Black, Bishop), (('F',7), Black, King False)],[])

promoteIntoCheckGame :: Game
promoteIntoCheckGame = (2, Black, [(('B',4), White, Pawn False), (('E',1), White, King True),
    (('H',2), Black, Pawn False), (('B',8), Black, King True)],[])

enPassantGame :: Game
enPassantGame = (2, Black, [(('D',4), White, Pawn True), (('F',4), White, Pawn True), (('E',1), White, King True),
    (('E',4), Black, Pawn False), (('B',8), Black, King True)],[])

castleGame :: Game
castleGame = (2, White, [(('H',1), White, Rook False), (('A',1), White, Rook False), (('E',1), White, King False),
    (('E',4), Black, Pawn False), (('B',8), Black, King True)],[])

whiteIsCookedGame :: Game
whiteIsCookedGame = (4, White, [(('F',2), White, Pawn False), (('E',4), White, Pawn False), (('E',1), White, King False),
    (('C',4), Black, Knight), (('F',3), Black, Bishop), (('H',4), Black, Bishop), (('G',2), Black, Queen),(('E',8), Black, King False)],[])