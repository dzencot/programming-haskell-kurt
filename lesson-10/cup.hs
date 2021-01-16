cup ml = \message -> message ml

getMl aCup = aCup (\ml -> ml)

drink aCup mlDrank =
    if mlDiff >= 0
    then cup mlDiff
    else cup 0
  where mlDiff = ml - mlDrank
        ml = getMl aCup

isEmpty aCup = getMl aCup == 0

coffeeCup = cup 500
afterASip = drink coffeeCup 530
afterManySips = foldl drink coffeeCup [30, 30, 30, 30, 30]

main = do
    print (getMl coffeeCup)
    print (getMl afterASip)
    print (getMl afterManySips)
