count :: Char -> String -> Int
count x xs = length [x1 | x1 <- xs , x == x1]  

main :: IO ()
main = do
    let charc = 's'
    let word = "Mission"
    let no = count charc word
    putStrLn $ "caractor count: " ++ show no
