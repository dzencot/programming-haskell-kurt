import Data.List

ifEven myFunction x = if even x
                      then myFunction x
                      else x

ifEvenCube n = ifEven (\x -> x^3) n

compareFirstNames name1 name2 = compare firstName1 firstName2
  where firstName1 = fst name1
        firstName2 = fst name2
        compareResult = compare firstName1 firstName2

compareLastNames name1 name2 = if compareResult == EQ
                               then compareFirstNames name1 name2
                               else compareResult
  where lastName1 = snd name1
        lastName2 = snd name2
        compareResult = compare lastName1 lastName2

names = [("Sergey", "Ivanov"),("Andrey", "Petrov"),("Andrey", "Ivanov"),("Anderey", "Sidorov")]
sorted = sortBy compareLastNames names

sfOffice name =
    if lastName < "Л"
    then nameText ++
         " - А/я 1234, Сан-Франциско, штат Калифорния, 94111"
    else nameText ++
         " - А/я 1010, Сан-Франциско, штат Калифорния, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name =
    nameText ++
         " - А/я 789, Нью-Йорк, штат Нью-Йорк, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name =
    nameText ++
         " - А/я 456, Рино, штат Невада, 89523"
  where nameText = snd name

washingtonOffice name =
    nameText ++
         " - А/я 456, Вашингттон, округ Колумбия, 20004"
  where nameText = snd name

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "wshngtn" -> washingtonOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

check1 = addressLetter ("Боб","Смит") "ny"
check2 = addressLetter ("Боб","Джонс") "ny"
check3 = addressLetter ("Дейзи","Смит") "sf"
check4 = addressLetter ("Боб","Смит") "reno"
check5 = addressLetter ("Боб","Смит") "la"
check6 = addressLetter ("Боб","Смит") "wshngtn"

main = do
  putStrLn check1
  putStrLn check2
  putStrLn check3
  putStrLn check4
  putStrLn check5
  putStrLn check6
