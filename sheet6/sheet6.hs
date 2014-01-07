-- Hilffunktionen:

sum' l = sum_ l 0

sum_ (x:xs) m = sum_ xs (m + x)
sum_ [] m = m

abs' x = 
  if(x > 0)
    then x
    else -x

length' l = length_ l 0

length_ (x:xs) m = (length_ xs m) + 1
length_ [] m = m

avg l = div (sum' l) (length' l)

isInList [] e = False
isInList (x:xs) e = 
  if(x == e)
    then True 
    else isInList xs e


removeElement l p = removeElement_ l p [] 0

removeElement_ (x:xs) p l n = 
  if(n == p)
    then l++xs
    else removeElement_ xs p (l++[x]) (n + 1)

getElement l p = getElement_ l p 0

getElement_ (x:xs) p n = 
  if(n == p)
    then x
    else getElement_ xs p (n + 1)

smallestDifference_2 [] a b = b
smallestDifference_2 (x:xs) a b =
  if(abs' (a - x) < b || b < 0)
    then smallestDifference_2 xs a (abs' (a - x))
    else smallestDifference_2 xs a b

smallestDifference_1 l n d =
  if((length' l) < 2 || n >= length' l)
    then d
    else 
      if(((smallestDifference_2 (removeElement l n) (getElement l n) (-1)) < d) || (n == 0))
        then smallestDifference_1 l (n + 1) (smallestDifference_2 (removeElement l n) (getElement l n) (-1))
        else smallestDifference_1 l (n + 1) d	     			     

smallestDifference l = smallestDifference_1 l 0 0

-- ich habe ehrlichgesagt keine Ahnung was die Antivalenz ist... Kann gut sein, dass ich falsch geraten habe.
antival [] = False
antival (x:xs) =
  if((antival xs || x) && (x == False || (antival xs == False)))
    then True
    else False

maxExp2_ n k = 
  if((mod n (2^k)) == 0)
    then maxExp2_ n (k + 1)
    else (k - 1)

minPow3_ n k =
  if((3^k) <= n)
    then minPow3_ n (k + 1)
    else (3^(k-1))

thirdRoot_ n k =
  if((k^3) <= n)
    then thirdRoot_ n (k + 1)
    else (k - 1)

sumOfOdds_ n k m =
  if(k > n)
    then m
    else
      if(mod k 2 == 1)
        then sumOfOdds_ n (k + 2) (m + k)
        else sumOfOdds_ n (k + 1) m

-- Aufgabe 1:
avgIncluded a b c = isInList [a,b,c] (avg [a,b,c])
smallestDifference3 a b c = smallestDifference [a,b,c]
smallestDifference4 a b c d = smallestDifference [a,b,c,d]
antival4 a b c d = antival [a,b,c,d]

-- Aufgabe 2:
maxExp2 n = maxExp2_ n 0
minPow3 n = minPow3_ n 0
thirdRoot n = thirdRoot_ n 1
sumOfOdds n = sumOfOdds_ n 0 0

-- Aufgabe 3:
--
-- magic1(n): Diese Funktion gibt für gerade n 1 und für ungerade n 2 zurück.
-- 
-- Die while-Schleife wird n mal durchgelaufen und bei jedem Durchgang wird k auf 3 - k gesetzt, 
-- da k zu beginn 1 ist sorgt das dafür, dass k abwechselnd 1 und 2 ist.
--
--
-- magic2(x): Diese Funktion gibt den kleinsten Exponenten der zweier Potenz zurück, die größer als x ist.
-- 
-- Die while-Schleife verdoppelt j solange es kliener als x ist und eben noch einmal mehr. Gesatrtet wird 
-- mit j = 0. k wird dabei bei jedem Schritt um eins inkrementiert, d.h. k ider der 2er-Exponent.
--
--
-- magic3(n, m): Die Funktion gibt den Aufgerundeten Mittelwert der beiden Werte n und m zurück.
--
-- zu beginn werden die werte j und k auf den kleineren bzw. größen wert m und n gesetzt. p wird zunächst völlig 
-- sinnlos auf 0 gesetzt. anschließend wird p auf den größen der bedien werte k gestzt, wenn der unterschied der 
-- beiden kleinergleich 1 ist. Anderenfalls wird macic3(n, m) recursiv mit n = j + 1 und m = k - 1 aufgerufen.
-- Das sorgt dafür, dass die beiden Werte imer näcer zusammenrücken und sich in der Mitte treffen. 
