import Data.Char
-- Board utitlities:
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished b = all (== 0) b  --- > finished [0,0,0,0,0] => True

valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row - 1) >= num

move ::Board -> Int -> Int -> Board
move b row num = [adjust r n | (r,n) <- zip [1..5] b] 
                    where 
                        adjust r n = if r == row then n-num else n
-- I/O utilities
newline :: IO ()
newline = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do putStr(show row)
                    putStr ": "
                    putStrLn (stars num)
putBoard ::Board -> IO ()
putBoard [a,b,c,d,e] = do   putRow 1 a
                            putRow 2 b 
                            putRow 3 c
                            putRow 4 d
                            putRow 5 e
getDigit :: String -> IO Int
getDigit promt = do putStr promt
                    x <- getChar
                    newline
                    if isDigit x then 
                        return (digitToInt x)
                    else
                        do  newline
                            putStrLn "ERROR: Invalid Digit"
                            getDigit promt
main :: IO ()
main = do
  putBoard initial
  row <- getDigit "Get a row number: "
  num <- getDigit "How many stars to remove: "
  print row
  print num
