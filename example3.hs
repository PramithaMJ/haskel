-- Write a Haskell program that:
-- Solve quadratic equation ax^2 + bx + c = 0
-- Return a tuple with two roots

solveQuadratic :: (Floating a) => a -> a -> a -> (a, a)
solveQuadratic a b c =
    let discriminant = b^2 - 4*a*c
        root1 = (-b + sqrt discriminant) / (2*a)
        root2 = (-b - sqrt discriminant) / (2*a)
    in (root1, root2)
main :: IO ()
main = do
    let a = 1
        b = -3
        c = 2
        (root1, root2) = solveQuadratic a b c
    putStrLn $ "Roots of the equation are: " ++ show (root1, root2)