sumAndSquare :: IO ()
sumAndSquare = do
    let listNum = [1..100]
    putStrLn $ "Numbers from1 to 100 : " ++ show listNum
    let divisable = [x | x <- listNum , x `mod` 3 ==0 || x `mod` 5 ==0]
    putStrLn $ "numbers divisable by 3 and 5: " ++ show divisable
    let total = sum(divisable)  
    putStrLn $ "sum of the total: " ++ show total

    let sumOfSquare = sum (map (^2) divisable)
    putStrLn $ "sum of square: " ++ show sumOfSquare
    putStrLn $ "suma of total: " ++ show total ++ " , sum of square total: " ++ show sumOfSquare
main :: IO()

main = sumAndSquare 

-- how to run:
-- ghc hello.hs -o hello
-- ./hello