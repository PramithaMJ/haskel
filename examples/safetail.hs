-- safetail :: [a] -> [a]
-- safetail x = if length x == 0 then [] else tail x

-- safetail :: [a] -> [a]
-- safetail xs
--     | length xs <=0 = []
--     | otherwise = tail xs

safetail :: [a] -> [a]
safetail [a] = []
safetail (_:xs) = xs


main :: IO ()
main = do
  let num :: [Int]
      num = [2,3,6]
  let safe = safetail num
  putStrLn $ "Safe tail: " ++ show safe
