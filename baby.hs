doubleMe x = x + x

doubleUs x y = doubleMe x  + doubleMe y

doubleIfSmall x = if x > 100
			then x
			else x * 2


length' xs = sum [1 | _ <- xs]
