import System.IO -- Необходимо е, за да се изключи буферирането на stdout

import Project
import Safe
import Data.Maybe

main:: IO()
main = do
    hSetBuffering stdout NoBuffering
    welcomeMessage
    let board = emptyBoard
    playerMove board

playerMove :: Board -> IO ()
playerMove board = do
    if (won board ) then putStrLn ("Computer wins!")
    else do
    if(length (possibleMoves board) == 0) 
    then do
    printBoard board
    putStrLn("Draw!")
    else do
    putStrLn("")
    putStr("Where do you want to place your X? ")
    loc <- getLine
    if(read loc < 1 || read loc > 9 || (not (elem (read loc) (possibleMoves board)))) then moveError board
    else do
    putStrLn ""
    let moveLoc = (read loc)
    let newBoard = findAndReplace board moveLoc X
    compMove newBoard

compMove :: Board -> IO ()
compMove board = do
    if(won board) then putStrLn ("Player wins!")
    else do
    if(length (possibleMoves board) == 0) 
    then do
    printBoard board
    putStrLn("Draw!")
    else do
    let newBoard = makeOMove board
    printBoard newBoard
    playerMove newBoard

moveError :: Board -> IO()
moveError board = do
    printBoard board
    putStrLn(" ")
    putStrLn("Not a valid move!")
    playerMove board
    
welcomeMessage :: IO()
welcomeMessage = do
    putStrLn("")
    putStrLn ("Welcome to Tic-Tac-Toe!")
    putStrLn("")
    putStrLn ("You will be starting first!")
    putStrLn ("Choose a move!")
    putStrLn (" ")
    putStrLn(" --- --- ---")
    putStrLn("| 1 | 2 | 3 |")
    putStrLn(" --- --- ---")
    putStrLn("| 4 | 5 | 6 |")
    putStrLn(" --- --- ---  ")
    putStrLn("| 7 | 8 | 9 | ")
    putStrLn(" --- --- ---")

printBoard:: Board -> IO()
printBoard (Board [a,b,c] [f,g,h] [j,k,l])= do
    putStrLn(" --- --- ---")
    putStrLn("| "++show a++" | "++show b++" | "++show c++" |")
    putStrLn(" --- --- ---")
    putStrLn("| "++show f++" | "++show g++" | "++show h++" |")
    putStrLn(" --- --- ---  ")
    putStrLn("| "++show j++" | "++show k++" | "++show l++" | ")
    putStrLn(" --- --- ---")
    

    