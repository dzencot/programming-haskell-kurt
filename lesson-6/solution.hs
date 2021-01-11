myRepeat n = cycle [n]
myVar = take 5 (myRepeat "test")

subseq startIndex endIndex list = take size (drop startIndex list)
  where size = endIndex - startIndex

inFirstHalf n list = n `elem` take (div (length list) 2) list

main = do
  print myVar
  print (subseq 2 5 [1 .. 10])
  print (subseq 4 8 "its doggy")
  print (inFirstHalf 2 [1,2,3,4])
  print (inFirstHalf 3 [1,2,3,4])
