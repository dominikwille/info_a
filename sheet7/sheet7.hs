import Data.Char

caesar k a = chr ((mod ((ord a) + k - 97) 26) + 97)

reflectLetter a =
  if(isLower a)
    then chr (187 - (ord a))
    else error "out of range"

newOrder_ a = 
  if(isLower a)
    then chr (((ord a) - 97) * 2)
    else chr (((ord a) - 65) * 2 + 1)

newOrder a b =
  if(isAlpha a && isAlpha b)
    then (newOrder_ a <= newOrder_ b)
    else False


