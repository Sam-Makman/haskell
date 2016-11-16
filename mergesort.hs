mergeSort [] = error "Cannot Sort An Empty List"
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take half xs )) (mergeSort ( drop half xs ))
			where half = length xs `div` 2


merge [] [] = [] 
merge [x] [] = [x]
merge [] [y] = [y]
merge [x] (y:ys)
	| x  < y = x : y : ys 
	| otherwise = y : merge [x]  ys 
merge (x:xs) [y] 
	| x > y = y : x : xs 
	| otherwise = x : merge [y]  xs 
merge (x:xs) (y:ys)
	| x > y = y : merge (x:xs) ys 
	| otherwise = x : merge xs (y:ys)
