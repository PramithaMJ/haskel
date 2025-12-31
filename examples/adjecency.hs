pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

main :: IO ()
main = do
    let n = [1,2,3,4,5]
    let adjElement = pairs n
    putStrLn $ "Adjecent element: " ++ show adjElement
