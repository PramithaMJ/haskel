
    -- sp xs ys = sum ( xs !! i * ys !! i | i <- [0..n-1])
    --             where n = lenght xs

sp :: [Int] -> [Int] -> Int
sp xs ys = sum [x * y | (x, y) <- zip xs ys]

main :: IO ()
main = do
  let y = sp [1, 2, 3] [2, 3, 4]
  putStrLn $ "Scalar Product: " ++ show y
  