module Project where

import Safe
import Data.List
import Data.Maybe

--creates symbol data
data Symbol = X | O | Empty deriving (Eq)
instance Show Symbol where
    show X = "X"
    show O = "O"
    show Empty = " "

--creates three type
type Three = [Symbol]

--creates board data
data Board = Board Three Three Three deriving (Eq)

--constructs empty board
emptyBoard :: Board
emptyBoard = Board [Empty,Empty,Empty] [Empty,Empty,Empty] [Empty,Empty,Empty]

--checks win condition
full :: Three -> Bool
full [a,b,c] = a==b && b==c && (noneEmpty [a,b,c])
    where
     noneEmpty :: Three -> Bool
     noneEmpty [a,b,c] = a /= Empty && b /= Empty && c /= Empty

--extracts columns
cols :: Board -> [Three]
cols (Board [a,b,c] [f,g,h] [j,k,l]) = [[a,f,j],[b,g,k],[c,h,l]]

--extract rows
rows :: Board -> [Three]
rows (Board x y z) =[x,y,z]

--extract diagonals
diag:: Board -> [Three]
diag (Board [a,b,c] [f,g,h] [j,k,l]) = [[a,g,l],[c,g,j]]

--checks if game is won
won :: Board -> Bool
won b = (length(filter (\x -> full x) (rows b))) > 0 || (length(filter (\x -> full x) (cols b))) > 0 || (length(filter (\x -> full x) (diag b))) > 0

--calculates possible moves
possibleMoves :: Board -> [Int]
possibleMoves b = [index | (index, x) <- zip [1..9] (boardToList b), x == Empty] 

--checks if game is draw
draw :: Board -> Bool
draw b = length(possibleMoves b) == 0

--transforms the board to a list
boardToList :: Board -> [Symbol]
boardToList (Board x y z) = x ++ y ++ z

--transforms a list to a board
listToBoard :: [Symbol] -> Board
listToBoard [a,b,c,d,f,g,h,j,k] = Board [a,b,c] [d,f,g] [h,j,k]

--makes a move
findAndReplace :: Board -> Int -> Symbol -> Board
findAndReplace b k p = listToBoard [if  (index == k) then p else x | (index, x) <- zip [1..9] (boardToList b)]

--if AI has a winning move, do it
winningOMove :: Board -> Maybe Board
winningOMove b = headMay [findAndReplace b k O | k <- (possibleMoves b), won (findAndReplace b k O)]

--if AI can block a player winning move, do it
blockXwin :: Board -> Maybe Board
blockXwin b = headMay [findAndReplace b k O | k <- (possibleMoves b), won (findAndReplace b k X)]

--check if board state is forked by AI
isForkO :: Board -> Bool
isForkO b = 2 == length [findAndReplace b k O | k <- (possibleMoves b), won (findAndReplace b k O)]

--if AI can make a fork, do it
forkO :: Board -> Maybe Board
forkO b = headMay [findAndReplace b k O | k <- (possibleMoves b), isForkO (findAndReplace b k O)]

--check if player has forked
isForkX :: Board -> Bool
isForkX b = 2 == length [findAndReplace b k X | k <- (possibleMoves b), won (findAndReplace b k X)]

--check if next move can lead to a player's fork
forkX :: Board -> Maybe Board
forkX b = headMay [findAndReplace b k X | k <- (possibleMoves b), isForkX (findAndReplace b k X)]

--validates player fork type
properFork :: Board -> Bool
properFork b = length [x | (k, x) <- zip [1..9] (boardToList b) ,elem k [1,3,7,9], x==X] >=2

--helper needed to get AI possible fork block positions
blockFork :: Board -> Bool
blockFork b = 1 == length [blockXwin (findAndReplace b k X) | k <- (possibleMoves b), won (findAndReplace b k X)]

--if AI can block player's fork, do it
blockXFork :: Board -> Maybe Board
blockXFork b = if (isJust(forkX b)  && properFork b) then headMay [findAndReplace b k O | k <- (possibleMoves b), blockFork (findAndReplace b k X)] else Nothing

--if AI can play in opposite corner, do it
playOppcorner :: Board -> Maybe Board
playOppcorner b = headMay[findAndReplace b k O | k <- (possibleMoves b), (k==1 && (boardToList b)!!8 == X) || (k==3 && (boardToList b)!!6 == X) || (k==7 && (boardToList b)!!2 == X) || (k==9 && (boardToList b)!!0 == X)]

--if AI can play in the corner, do it
playOcorner :: Board -> Maybe Board
playOcorner b = headMay[findAndReplace b k O | k <- (possibleMoves b),elem k [1,3,7,9]]

--choose AI next move
makeOMove :: Board -> Board
makeOMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
    | isJust (winningOMove board) = fromJust (winningOMove board)
    | isJust (blockXwin board) = fromJust (blockXwin board)
    | isJust (forkO board) = fromJust (forkO board)
    | isJust (blockXFork board) = fromJust (blockXFork board)
    | elem 5 (possibleMoves board) = findAndReplace board 5 O
    | isJust (playOppcorner board) = fromJust (playOppcorner board)
    | isJust (playOcorner board) = fromJust (playOcorner board)
    | otherwise = if length (possibleMoves board) > 0
    then findAndReplace board (head (possibleMoves board)) O
    else board



