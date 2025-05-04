list = [3,2,1,7,5,6,4,2,1,5,9,8,8]

merge_sort :: (Ord) a => [a] -> [a]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort xs = merge (merge_sort ys) (merge_sort zs)
   where (ys, zs) = split_in_halves xs
--    where (ys, zs) = splitAt n xs
--          n = length xs `div` 2

split_in_halves [] = ([], [])
split_in_halves [x] = ([x], [])
split_in_halves (x:y:zs)   = (x:xs, y:ys)
  where (xs, ys) = split_in_halves zs


merge xs ys -- @(x:xs') ys@(y:ys')  -- wrong 
    | xs == [] = ys
    | ys == [] = xs
    | x <= y = x : merge xs' ys
    | otherwise = y : merge xs ys'
    where (x:xs') = xs
          (y:ys') = ys


main = do
    print (list)
    print (merge_sort $ list)