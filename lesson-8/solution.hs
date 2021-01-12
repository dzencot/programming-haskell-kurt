myDrop 0 xs = xs
myDrop 1 (x:xs) = xs
myDrop n (x:xs) = myDrop (n - 1) xs

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

finiteCycle (first:rest) = first:rest ++ [first]

ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

collatz 1 = 1
collatz n =
    if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFibIter previous next counter num =
    if counter == num
    then next
    else fastFibIter next (previous + next) (counter + 1) num
fastFib num = fastFibIter 0 1 1 num

main = do
  print (myDrop 0 [1,2,3,4,5])
  print (myDrop 1 [1,2,3,4,5])
  print (myDrop 2 [1,2,3,4,5])
  print (myLength [1,2,3,4,5])
  print (finiteCycle [1,2,3,4,5])
  print (collatz 91)
  print (map collatz [100 .. 120])
  print (myReverse [1,2,3,4,5])
  -- print (fastFib 1 1 1000)
  -- print (fastFib 1 1 5)
  print (fib 5)
  print (fastFib 5)
