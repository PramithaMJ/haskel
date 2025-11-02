> ```haskell
> ghci> (map reverse . lines) "dog\ncat\nfish"
> ["god","tac","hsif"]
> ```


lines → split text into lines

map reverse → reverse each line

unlines → join them back

```haskell
ghci> (unlines . map reverse . lines) "dog\ncat\nfish"
"god\ntac\nhsif\n"
```

```haskell
ghci> (reverse.show) 12315
"51321"
```
