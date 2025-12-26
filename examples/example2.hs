compoundInterest :: Double -> Double -> Int -> Double
compoundInterest p r n = total
    where
        total = p * (1 + r) ^ n

main :: IO ()
main = do
    let principle = 10
    let rate = 0.05
    let year = 5

    let totalAmmount = compoundInterest principle rate year

    putStrLn $ "Total ammount is: " ++ show totalAmmount
