sum' :: (Num a) => [a] -> a
sum' [] = error "Cannot sum empty list" 
sum' [x] = x
sum' (x:xs) = x + (sum xs)

fib :: Int -> Int 
fib x 
	| x == 0 = x
	| x == 1 = x 
	| otherwise  = fib(x-2) + fib (x-1)	


set [] = error "cannot use an empty list"
set [x] = [x]
set (x:xs) 
	| x `elem` xs = set xs
	|otherwise = x : set xs


mergeSort [] = error "cannot sort empty list" 
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort ( take (length xs `div` 2) xs )) (mergeSort (drop (length xs `div` 2)  xs))


merge [] [] = [] 
merge [x] (y:ys) 
	| x > y = y : x : ys
	| otherwise = x : y : ys
merge (x:xs) [y]
	| x > y = y : x : xs
	| otherwise = x : y : xs
merge (x:xs) (y:ys) 
	| x > y = y : merge (x:xs) ys
	| otherwise = x : merge xs (y:ys) 
