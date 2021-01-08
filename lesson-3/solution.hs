doubleDouble x = dubs * 2
  where dubs = x * 2
doubleDoubleLambda x = (\dubs -> dubs * 2) (x * 2)

overwrite x = let x = 2
              in let x = 3
               in let x = 4
                in x
overwriteLambda x = (\x -> (\x -> (\x -> x) 4) 3) 2

-- counter x = let x = x + 1
--             in
--              let x = x + 1
--              in
--               x
counterLambda x = (\x ->
                    (\x ->
                      (\x -> x) x + 1) x + 1) x
