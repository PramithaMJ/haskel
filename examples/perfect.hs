-- 1. Helper Function: Find all factors except the number itself
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

-- 2. Check if the sum of factors equals the number
perfect :: Int -> Bool
perfect n = sum (factors n) == n

-- 3. Generate list of perfect numbers
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], perfect x]

main :: IO ()
main = do
  let no = 500
  let perfectnos = perfects no
  putStrLn $ "Perfect numbers: " ++ show perfectnos