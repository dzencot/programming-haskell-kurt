half :: Int -> Int
half n = div n 2

printDouble :: Int -> String
printDouble n = show (n * 2)

myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

main = do
  print (half 5)
  print (printDouble 4)
