--translate [] = [] 
--translate x = x
--translate (x:xs) = x : "f" : translate xs


reverse' [] = [] 
reverse' (x:xs) = (reverse' xs) ++ x  
