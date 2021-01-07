messyMain :: IO ()
messyMain = do
  putStrLn "Кто получатель этого письма?"
  recipient <- getLine
  putStrLn "Название книги:"
  title <- getLine
  putStrLn "Кто автор этого письма?"
  author <- getLine
  putStrLn ("Дорогой " ++ recipient ++ "!\n"
        ++ "Спасибо за то, что купили \""
        ++ title ++ "\"!\nС уважением,\n" ++ author)

toPart recipient = "Дорогой " ++ recipient ++ "!\n"

bodyPart bookTitle = "Спасибо за то, что купили \""
                     ++ bookTitle ++ "\"!\n"

fromPart author = "С уважением,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

main = do
  putStrLn "Кто получатель этого письма?"
  recipient <- getLine
  putStrLn "Название книги:"
  title <- getLine
  putStrLn "Кто автор этого письма?"
  author <- getLine
  putStrLn (createEmail recipient title author)
