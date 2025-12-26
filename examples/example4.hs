-- CONDITIONALA EXPRESSION
-- conditionalsafetail :: [a] -> [a]
-- conditionalsafetail x = if length x == 0 then [] else tail x

-- conditionalsafetail :: [a] -> [a]
-- conditionalsafetail xs = if null xs then [] else tail xs

-- GUARDED EQUATION

-- guardedsafetail :: [a] -> [a]
-- guardedsafetail xs
--     | null xs = [] 
--     | otherwise = tail xs


-- PATTERN MATCHING
patternmatchingsafetail :: [a] -> [a]
patternmatchingsafetail [] = []
patternmatchingsafetail (_:xs) = xs

main :: IO ()
main = do 
    let num :: [Int]
        num = []
    let safe = patternmatchingsafetail num
    putStrLn $ "safe array: " ++ show safe