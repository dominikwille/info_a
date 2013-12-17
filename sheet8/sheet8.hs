-- Aufgabe 1:
g :: Int -> Int
g n = g_ n 3 [2, 1, 0]

g_ :: Int -> Int -> [Int] -> Int
g_ n k l
  | (n < 3)   = n
  | (n == k)  = g__ l
  | otherwise = g_ n (k + 1) ([g__ l] ++ l) 

g__ :: [Int] -> Int
g__ (g1:g2:g3:l) = g1 + 2 * g2 - g3

-- Aufgabe 2:
pointOnLine (a, b) (m, n) = (b == m * a + n)

pointOverLine (a, b) (m, n) = (b > m * a + n)

lineThrough (a, b) (c, d)
  | (a == c)  = error "That is not possible (x1 = x2)"
  | (a < c)   = (((d - b) / (c - a)), (-((d - b) / (c - a)) * a + b))
  | otherwise = lineThrough (c, d) (a, b)

crossing (m, n) (o, p)
  | (m == o)  =  error "That is not possible (Lines have the same slope)"
  | otherwise = (((p - n) / (m - o)), ((p * m - n * o) / (m - o)))

parallelThrough (m, n) (a, b) = (m, (-m * a + b))

-- Aufgabe 3:
factorial n 
  | (n <= 1)  = 1
  | otherwise = n * factorial (n - 1)  

biomDef n k = (factorial n) / (factorial k) / (factorial (n - k))

biomRec n k 
  | (n == k || k == 0) = 1
  | otherwise          = (biomRec (n - 1) k) + biomRec (n - 1) (k - 1)

countCalls n k = countCalls_ n k 0
countCalls_ n k a
  | (n == k || k == 0) = a + 1
  | otherwise          = (countCalls_ (n - 1) k (a + 1)) + countCalls_ (n - 1) (k - 1) (a + 1)
