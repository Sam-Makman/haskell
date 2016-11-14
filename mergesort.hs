mergeSort [] = error "Cannot Sort An Empty List"
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take (length xs `div` 2) xs )) (mergeSort ( drop (length xs `div` 2) xs ))


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
