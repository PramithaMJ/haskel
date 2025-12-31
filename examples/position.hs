positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x1, i) <- zip xs [0 ..], x == x1]

main :: IO ()
main = do
  let array = [1, 23, 45, 6, 7, 34, 33, 6, 4, 6, 56, 33]
  let num = 6
  let posi = positions num array

  putStrLn $
    "Position "
      ++ show num
      ++ " in array "
      ++ show array
      ++ " at positions: "
      ++ show posi

-- positions x xs = [i | (x1, i) <- zip xs [0 ..], x == x1]
-- zip xs [0 ..]
-- xs = [1,23,45,6,7,34,33,6,4,6,56,33]
-- zip xs [0..]
-- [(1,0),(23,1),(45,2),(6,3),(7,4),(34,5),(33,6),(6,7),(4,8),(6,9),(56,10),(33,11)]

-- (x1, i) <- zip xs [0 ..]
-- This:
--	•	takes each pair from the zipped list
--	•	x1 is the element
--	•	i is the index

--x == x1 This is a filter condition:
--	•	keep only the elements where the value equals x

--[i | ... ]

-- This means:
--	•	collect only the indices i
--	•	for elements where x == x1
