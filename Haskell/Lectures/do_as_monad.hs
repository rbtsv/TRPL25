main :: IO ()
main = do
    putStrLn "Enter the numerator:"
    numStr <- getLine                 -- like: numStr = input()
    let num = read numStr :: Double  -- parse string to Double

    putStrLn "Enter the denominator:"
    denomStr <- getLine
    let denom = read denomStr :: Double

    if denom == 0
      then putStrLn "Cannot divide by zero."
      else do
          let result = num / denom
          putStrLn ("Result is: " ++ show result)