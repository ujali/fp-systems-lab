module Main where

main :: IO ()
main = do
    putStrLn "Welcome to Haskell Learning Lab!"
    putStrLn "This is a playground for learning functional programming in Haskell."
    
    -- Example: Basic functional programming concepts
    let numbers = [1, 2, 3, 4, 5]
    let doubled = map (*2) numbers
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Doubled: " ++ show doubled
    
    -- Example: Higher-order functions
    let total = foldr (+) 0 numbers
    putStrLn $ "Sum: " ++ show total
    
    -- Example: Function composition
    let addThenDouble = (*2) . (+1)
    putStrLn $ "addThenDouble 5 = " ++ show (addThenDouble 5)
