-- Define a function called sumAndSquares
-- :: IO () means this function performs Input/Output (IO) and returns "nothing useful" (unit type ())

sumAndSquares :: IO ()
sumAndSquares = do
    -- Generate a list of numbers from 1 to 100
    let numbers = [1..10]
    putStrLn $ "Numbers: " ++ show numbers
    -- Filter number divisible by 3 and 5 using list comprehension

    let divisableBy3And5 = [x | x <- numbers, x `mod` 3 == 0 ||  x `mod` 5 == 0]
    -- [expression | variable <- list ,condition1, condition2,...]
    putStrLn $ "Numbers divisible by 3 or 5: " ++ show divisableBy3And5

    -- Compute sum of divisible numbers
    let totalSum = sum divisableBy3And5
    putStrLn $ "Sum of numbers divisible by 3 or 5: " ++ show totalSum

    -- Compute squares of divisible numbers
    let totalSquare = sum (map (^2) divisableBy3And5)
    putStrLn $ "Sum of squares of numbers divisible by 3 or 5: " ++ show totalSquare

    -- print final result as tuple
    putStrLn $ "Final Result (Sum, Sum of Squares): " ++ show (totalSum, totalSquare)

main :: IO ()
main = sumAndSquares