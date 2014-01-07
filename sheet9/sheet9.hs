-- Aufgabe 1:
first (x:xs) = x
fibList_ x (a:b:l)
  | (x > 0)   = fibList_ (x-1) ([a+b] ++ [a] ++ [b] ++ l)
  | otherwise = [a] ++ [b] ++ l
fibList x = fibList_ x [0,1]
fastFib x = first(fibList x)

-- Aufgabe 2:
delMax l = delValue (maximum l) l
delValue a l = delValue_ a [] l
delValue_ a l (x:xs)
  | (a == x)  = l ++ xs
  | otherwise = delValue_ a (l ++ [x]) xs

selStep :: ([Int], [Int]) -> ([Int], [Int])
selStep (a, b) = ((delMax a), ([maximum a] ++ b))

selSort l = selSort_ (l, [])

selSort_ (a, b)
  | (a == []) = b
  | otherwise = selSort_ (selStep(a, b))
