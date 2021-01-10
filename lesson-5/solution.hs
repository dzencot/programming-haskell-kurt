genIfEvenX x = (\f -> if even x then f x else x)

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id ->
                              getRequestUrl host apiKey resource id)
exampleUrlBuilder = genHostRequestBuilder "http://example"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

genResourceRequestBuilder  builder resource = (\id -> builder resource id)
myResourceUrlBuilder = genResourceRequestBuilder myExampleUrlBuilder "book"

myResourceBuilderUrl1 = getRequestUrl "http://example" "1337hAsk3ll" "book"

subtract2 = flip (-) 2

ifEven myFunction x = if even x
                      then myFunction x
                      else x

ifEvenInc = ifEven (1 +)

ifEvenDouble = ifEven (2 *)

ifEvenSquare = ifEven (^ 2)

binaryPartialApplication func x = (\y -> func x y)

main = do
  print (myResourceUrlBuilder "1234")
  print (myResourceBuilderUrl1 "1234")
  print (subtract2 5)
  print (ifEvenInc 4)
  print (ifEvenDouble 4)
  print (ifEvenSquare 4)
  print ((binaryPartialApplication (+) 2) 3)
