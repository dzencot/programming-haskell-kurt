myMap _ [] = []
myMap func (x:xs) = func x : myMap func xs

add3ToAll = myMap (3 +)
mul3ByAll = myMap (3 *)

remove _ [] = []
remove test (x:xs) =
    if test x
    then remove test xs
    else x:remove test xs

myProduct = foldl (*) 1

main = do
  print (add3ToAll [1,2,3])
  print (mul3ByAll [1,2,3])
  print (remove even [1,2,3,4])
  print (myProduct [1,2,3,4])
