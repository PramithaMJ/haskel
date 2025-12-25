

quadraticRoots :: Double -> Double -> Double -> (Double, Double)
quadraticRoots a b c = (root1, root2)
  where
    -- Compute discriminant D = b^2 - 4ac
    d = b^2 - 4*a*c
    -- Compute roots using quadratic formula
    root1 = (-b + sqrt d) / (2*a)
    root2 = (-b - sqrt d) / (2*a)

-- main is the entry point of the Haskell program
-- :: IO () means it performs input/output and returns unit type ()
main :: IO ()
main = do
    let a = 1
    let b = -3
    let c = 2
    let roots = quadraticRoots a b c

    
    putStrLn $ "equation: " ++ show a ++ "x^2 + " ++ show b ++ "x + " ++ show c ++ " = 0"
    putStrLn $ "roots: " ++ show roots

   
