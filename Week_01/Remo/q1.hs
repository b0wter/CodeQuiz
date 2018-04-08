-- einfache Variante: Array aufteilen und wieder zusammensetzen
lshift xs k = drop k xs ++ take k xs

-- rekursiv ein Element nach dem anderen verschieben:
lshift' [] _ = []
lshift' xs 0 = xs
lshift' (x:xs) n = lshift' (xs ++ [x]) (n-1)

-- 
main = do
	let parseWords line = [read w :: Int | w <- words line]

	line <- getLine
	let (n:k:_) = parseWords line
	
	line <- getLine
	let arr = parseWords line
	
	putStrLn $ unwords [show i | i <- (lshift' arr k)]

