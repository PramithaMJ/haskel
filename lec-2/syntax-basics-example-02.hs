
-- Define function compoundInterest
-- :: Double -> Double -> Int -> Double
-- Input: principal p, rate r, years n
-- Output: total amount (Double)
-- compoundInterest is a function that takes three inputs: a Double, another Double, an Int, and returns a Double.
compoundInterest :: Double -> Double -> Int -> Double
compoundInterest p r n = total
  where
    -- Compute total amount using formula A = P * (1 + r)^n
    total = p * (1 + r) ^ n

-- main is the entry point of the Haskell program
-- :: IO () means it performs input/output and returns unit type ()
main :: IO ()
main = do
    -- Define example inputs
    let principal = 1000     -- initial principal amount
    let rate = 0.05          -- annual interest rate (5%)
    let years = 10           -- number of years

    -- Compute compound interest using the function
    let totalAmount = compoundInterest principal rate years

    -- Print inputs and result
    putStrLn $ "Principal amount: " ++ show principal
    putStrLn $ "Annual interest rate: " ++ show rate
    putStrLn $ "Number of years: " ++ show years
    putStrLn $ "Total amount after " ++ show years ++ " years: " ++ show totalAmount

    -- Example: can also compute for other values
    let totalAmount2 = compoundInterest 5000 0.03 7
    putStrLn $ "\nExample 2: Principal 5000, rate 3%, years 7"
    putStrLn $ "Total amount: " ++ show totalAmount2
