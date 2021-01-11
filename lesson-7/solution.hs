myTail (_:xs) = xs
myTail [] = []

myGCD a 0 = a
myGCD a b = myGCD b remainder
  where remainder = a `mod` b

main = do
  print (myTail [1,2,3])
  print (length (myTail []))
  print (myGCD 20 16)
