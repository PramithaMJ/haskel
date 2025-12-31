factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

main :: IO()
main = do
    let factn = factors 15
    putStrLn $ "factors of 15: " ++ show factn

    let p = prime 15

    putStrLn $ "Is Prime: " ++ show p
    