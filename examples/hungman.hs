import System.IO

-- 1. The Entry Point
-- This tells Haskell to start the program by running the 'hangman' action.
main :: IO ()
main = hangman

-- 2. The Game Setup
hangman :: IO ()
hangman = do
  putStrLn "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

-- 3. Secure Input (prints dashes instead of letters)
sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

-- 4. Helper to read a char without echoing
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

-- 5. The Game Loop
play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then
      putStrLn "You got it!"
    else do
      putStrLn (match word guess)
      play word

-- 6. The Logic (Pure function)
match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]
