-- Define a function called sumAndSquares
-- :: IO () means this function performs Input/Output (IO) and returns "nothing useful" (unit type ())

sumAndSquares :: IO ()
sumAndSquares = do
    -- Step 1: Generate numbers from 1 to 100
    let numbers = [1..100]
    putStrLn $ "Numbers from 1 to 100: " ++ show numbers  
    
    -- Step 2: Filter numbers divisible by 3 or 5 using a list comprehension
    let divisible = [x | x <- numbers, x `mod` 3 == 0 || x `mod` 5 == 0] -- [ expression | variable <- list, condition1, condition2, ... ]
    putStrLn $ "Numbers divisible by 3 or 5: " ++ show divisible
    
    -- Step 3: Compute the sum of divisible numbers
    let total = sum divisible
    putStrLn $ "Sum of divisible numbers: " ++ show total
    
    -- Step 4: Compute the sum of squares of divisible numbers
    let totalSquares = sum (map (^2) divisible)
    putStrLn $ "Sum of squares of divisible numbers: " ++ show totalSquares
    
    -- Step 5: Print the final result as a tuple
    putStrLn $ "Final Result as Tuple: (" ++ show total ++ ", " ++ show totalSquares ++ ")"

-- main is the entry point of a Haskell program
-- :: IO () means it performs input/output and returns unit type ()
main :: IO ()
main = sumAndSquares
