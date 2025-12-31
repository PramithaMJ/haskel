pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [ x <= y | (x,y) <- pairs xs]

main :: IO ()
main = do
  let n = [1, 2, 3, 4, 5]
  let adjElement = pairs n
  putStrLn $ "Adjecent element: " ++ show adjElement
  let sortedArray = sorted n
  putStrLn $ "Sorted Array: " ++ show sortedArray
