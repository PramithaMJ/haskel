import Data.Char (isDigit)

-- 1. Main Entry Point
main :: IO ()
main = nim

-- 2. Initial Setup
-- The exercise hint suggests starting with [5, 4, 3, 2, 1] [cite: 400]
nim :: IO ()
nim = play [5, 4, 3, 2, 1] 1

-- 3. The Game Loop
play :: [Int] -> Int -> IO ()
play board player = do
  putStrLn ""
  putBoard board
  if finished board
    then do putStrLn $ "Player " ++ show (next player) ++ " wins!"
    else do
      putStrLn $ "Player " ++ show player ++ "'s turn"

      -- Get Row Number
      putStr "Select row number: "
      rowStr <- getLine

      -- Get Number of Stars
      putStr "How many stars to remove? "
      numStr <- getLine

      -- Validate and Update
      if isValidMove board rowStr numStr
        then
          let row = read rowStr :: Int
              num = read numStr :: Int
              newBoard = applyMove board row num
           in play newBoard (next player)
        else do
          putStrLn "ERROR: Invalid move. Try again."
          play board player

-- 4. Display Logic
-- Prints the board with row numbers and stars
putBoard :: [Int] -> IO ()
putBoard board = putBoard' board 1

putBoard' :: [Int] -> Int -> IO ()
putBoard' [] _ = return ()
putBoard' (x : xs) n = do
  putStr (show n ++ ": ")
  putStrLn (replicate x '*') -- Prints x number of stars
  putBoard' xs (n + 1)

-- 5. Game Logic Helpers

-- Switch player: 1 becomes 2, 2 becomes 1
next :: Int -> Int
next 1 = 2
next 2 = 1

-- Check if board is empty (sum of all stars is 0)
finished :: [Int] -> Bool
finished board = sum board == 0

-- Validates that inputs are digits, row is valid, and enough stars exist
isValidMove :: [Int] -> String -> String -> Bool
isValidMove board rowStr numStr
  | not (all isDigit rowStr) || not (all isDigit numStr) = False
  | otherwise =
      let row = read rowStr
          num = read numStr
       in validBoardIndex board row && validStarCount board row num

validBoardIndex :: [Int] -> Int -> Bool
validBoardIndex board row = row >= 1 && row <= length board

validStarCount :: [Int] -> Int -> Int -> Bool
validStarCount board row num = board !! (row - 1) >= num && num > 0

-- Apply the move: subtracts 'num' from the specific 'row'
applyMove :: [Int] -> Int -> Int -> [Int]
applyMove board row num = [if r == row then n - num else n | (n, r) <- zip board [1 ..]]
