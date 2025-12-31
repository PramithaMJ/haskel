pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <-[1..n], y <- [1 ..n], z <- [1 .. n], x^2 + y^2 == z^2 ]

-- x^2 + y^2 = z^2

-- pyths 5
-- [(3,4,5), (4,3,5)]
main :: IO ()
main = do
    let no = 5
    let pythons =  pyths no
    putStrLn $ "pythons: " ++ show pythons


-- EX 2: 
-- perfect :: Int -> Bool
-- perfect n = sum (int (factors n )) == n
-- perfects n = [x| x <- [1..n] , perfect x]