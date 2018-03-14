module Board
    ( initialBoard
    , board_DisplayString
    )
    where

-- invariant: flipCount for 4 corners == 0


import Data.Vector as V ( Vector, (!), (//), fromList, map, toList )
import Data.Ix as Ix ( Ix, index, range )
import Data.Function ( (&) )


data RowIndex = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 deriving (Eq, Ix, Ord, Show)
data ColIndex = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 deriving (Eq, Ix, Ord, Show)


data Position = Position RowIndex ColIndex deriving (Eq, Ord, Show)

data EmptySquare = EmptySquare Position deriving (Eq, Show)

data FilledSquare = FilledSquare Disk Position deriving (Eq, Show)

data BoardSquare 
    = Board_EmptySquare EmptySquare
    | Board_FilledSquare FilledSquare 
        deriving (Eq, Show)

newtype BoardRow = BoardRow (Vector BoardSquare) deriving (Eq, Show)

newtype Board = Board (Vector BoardRow) deriving (Eq, Show)

data Disk = Disk {_initColor :: Color,  _flipCount :: Int} deriving (Eq, Show)

data Color = Black | White deriving (Eq, Show)


firstRowIndex :: RowIndex
firstRowIndex = 
    R0


lastRowIndex :: RowIndex
lastRowIndex = 
    R7


firstColIndex :: ColIndex
firstColIndex = 
    C0


lastColIndex :: ColIndex
lastColIndex = 
    C7


rowIndexRange :: [RowIndex]
rowIndexRange = 
    Ix.range (firstRowIndex, lastRowIndex)


colIndexRange :: [ColIndex]
colIndexRange = 
    Ix.range (firstColIndex, lastColIndex)


rowIndexInt :: RowIndex -> Int
rowIndexInt x = 
    Ix.index (firstRowIndex, lastRowIndex) x


colIndexInt :: ColIndex -> Int
colIndexInt x = 
    Ix.index (firstColIndex, lastColIndex) x
 
 
boardSquare_DisplayString :: BoardSquare -> String
boardSquare_DisplayString boardSquare =
        let 
            string = 
                case boardSquare of
                    Board_EmptySquare _ -> "-"
                    Board_FilledSquare (FilledSquare (Disk color _) _)  ->
                        case color of
                            Black -> "x"
                            White -> "o"
        in
            " " ++ string ++ " "


boardRow_DisplayString :: BoardRow -> String          
boardRow_DisplayString (BoardRow v) =
        concat $ V.toList $ V.map boardSquare_DisplayString v


board_DisplayString :: Board -> String
board_DisplayString (Board v) =
        concat $ V.toList $ V.map (\x -> boardRow_DisplayString x ++ "\n") v


boardRow :: RowIndex -> Board -> BoardRow
boardRow r (Board v) = 
    v ! rowIndexInt r


squareAt :: Position -> Board -> BoardSquare
squareAt (Position r c) board = 
    v ! colIndexInt c
        where (BoardRow v) = boardRow r board



emptyBoard :: Board
emptyBoard = 
    let
        vCol = V.fromList colIndexRange
        f = \r -> BoardRow $ V.map (\c -> Board_EmptySquare $ EmptySquare $ Position r c) vCol
    in
        Board $ V.map f (V.fromList rowIndexRange) 


newDisk :: Color -> Disk
newDisk color = 
    Disk {_initColor = color,  _flipCount = 0}


newWhiteDisk :: Disk
newWhiteDisk = 
    newDisk White


newBlackDisk :: Disk
newBlackDisk = 
    newDisk Black


initialBoard :: Board
initialBoard =
    let
        board = emptyBoard
    in
        board
            & place newWhiteDisk (squareAt (Position R3 C3) board) 
            & place newBlackDisk (squareAt (Position R3 C4) board)
            & place newBlackDisk (squareAt (Position R4 C3) board)
            & place newWhiteDisk (squareAt (Position R4 C4) board)


place :: Disk -> BoardSquare -> Board -> Board
place disk boardSquare board = 
    case boardSquare of
        Board_EmptySquare (EmptySquare pos) -> placePositioned disk pos board
        Board_FilledSquare _ -> board


placePositioned :: Disk -> Position -> Board -> Board
placePositioned disk pos@(Position r c) board@(Board v) =
    let
        (BoardRow row) = boardRow r board 
        ri = rowIndexInt r
        ci = colIndexInt c
        item' = Board_FilledSquare $ FilledSquare disk pos

        row' = BoardRow $ row // [(ci, item')]
    in
        Board $ v // [(ri, row')]


flipAt :: BoardSquare -> Board -> Board
flipAt boardSquare board =
    case boardSquare of
        Board_EmptySquare _ -> board
        Board_FilledSquare (FilledSquare disk pos) -> placePositioned (flipDisk disk) pos board
           

flipDisk :: Disk -> Disk
flipDisk (Disk color flipCount) =
    let        
        color' = (if color == White then Black else White)
        flipCount' = (flipCount + 1)
    in
        Disk color' flipCount'