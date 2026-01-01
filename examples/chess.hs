-- Returns a list of valid (x, y) coordinates
knightMoves :: (Int, Int) -> [(Int, Int)]
knightMoves (x, y) =
  [(x + dx, y + dy) | (dx, dy) <- moves, onBoard (x + dx, y + dy)]
  where
    -- The 8 possible jumps (as seen in your image)
    moves =
      [ (1, 2),
        (1, -2),
        (-1, 2),
        (-1, -2),
        (2, 1),
        (2, -1),
        (-2, 1),
        (-2, -1)
      ]

    -- Helper function to check if a square is inside the 8x8 grid
    onBoard (c, r) = c `elem` [1 .. 8] && r `elem` [1 .. 8]

main :: IO ()
main = do
  -- Knight at position (1,1) (bottom-left corner)
  print (knightMoves (1, 1))

-- Result: [(2,3), (3,2)]
-- (Note: It only returns 2 moves because the other 6 would fall off the board!)