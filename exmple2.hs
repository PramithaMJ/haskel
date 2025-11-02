annualRate :: IO ()
annualRate = do
-- Given a principal amount p, annual rate r (as decimal), and number of years n
-- A=P×(1+r)n
    let p = 1000      -- principal amount
        r = 0.05      -- annual interest rate (5%)
        n = 10        -- number of years
        amount = p * (1 + r) ^ n
    putStrLn $ "Amount after " ++ show n ++ " years: " ++ show amount
main :: IO ()
main = annualRate
