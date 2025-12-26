-- Solve quadratic equation ax^2 + bx + c = 0
-- Return a tuple with two roots

quadraticEqationRoot :: Double -> Double -> Double -> (Double, Double)
quadraticEqationRoot a b c = (x1, x2)
    where
        discriminant = b^2 -4*a*c
        sqrtDisc = sqrt discriminant
        x1 = (-b + sqrtDisc) / (2*a)
        x2 = (-b - sqrtDisc) / (2*a)

main :: IO ()
main = do
    let a = 1
    let b = 9 
    let c = 6
    let roots = quadraticEqationRoot a b c

    putStrLn $ "Quadratic equation roots are: " ++ show roots
