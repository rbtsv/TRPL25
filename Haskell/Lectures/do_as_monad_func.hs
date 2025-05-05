main :: IO ()
main =
    putStrLn "Enter the numerator:" >>= \_ ->
    getLine >>= \numStr ->
    let num = read numStr :: Double in

    putStrLn "Enter the denominator:" >>= \_ ->
    getLine >>= \denomStr ->
    let denom = read denomStr :: Double in

    if denom == 0
      then putStrLn "Cannot divide by zero."
      else let result = num / denom
           in putStrLn ("Result is: " ++ show result)