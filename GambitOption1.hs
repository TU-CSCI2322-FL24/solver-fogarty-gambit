--stores positions in typical chess notation format (ex: f4, g7)
type Coordinate = (Char, Int)

--The Board type represents part of the game state. Other important bits of info in the game state will probably
-- include whose turn it is, the material advantage of each player, and maybe who has won.
--two ways to represent the board:
-- This one only stores squares on the board that contain pieces
type Board = [(Piece, Coordinate)]

--this one stores every square on the board, and Nothing if a piece isn't on it
--type Board = [(Coordinate, Maybe Piece)]


data Piece = King Color | Queen Color | Bishop Color | Knight Color | Rook Color | Pawn Color

data Color = Black | White

--Still need a type representing a move, which will probably contain a piece and where it's moving.

--stores every coordinate on the board. Will be useful in checking if a move is valid. (ex: j8 is not a valir square to move to)
allCoords :: [Coordinate]
allCoords = [(x, y) | x <- ['a'..'h'], y <- [1..8]]