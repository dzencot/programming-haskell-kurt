import Data.Char

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

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs

myElem pattern xs = length (filter (== pattern) xs) > 0

isPalindrome text = correctText == reverse correctText
  where correctText = map toLower (filter (/= ' ') text)

harmonic count = foldr (+) 0 (map (1/) [2 .. count])

main = do
  print (add3ToAll [1,2,3])
  print (mul3ByAll [1,2,3])
  print (remove even [1,2,3,4])
  print (myProduct [1,2,3,4])
  print (myFoldl (+) 0 (take 4 [1..]))
  print (myFoldr (-) 0 (take 5 [1..]))
  print (myElem 1 [1,2,3])
  print (myElem 4 [1,2,3])
  print (isPalindrome "TEST tset")
  print (harmonic 5)
