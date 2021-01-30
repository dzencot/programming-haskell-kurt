type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

patientInfo :: PatientName -> Int -> Int -> String
patientInfo patient age height = name ++ " " ++ ageHeight
  where name = lname ++ ", " ++ fname
        ageHeight = "(Age: " ++ show age ++ "; height: " ++ show height ++ "sm)"
        lname = lastName patient
        fname = firstName patient

patientTest = ("John", "Dou")

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

type MiddleName = String
type FirstName = String
type LastName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Sex = Male | Female
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'
showSex :: Sex -> String
showSex Male = "male"
showSex Female = "female"

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType}

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 157
                      , weight = 52
                      , bloodType = BloodType O Neg }

janeESmith :: Patient
janeESmith = Patient { name = NameWithMiddle "Jane" "Elizabet" "Smith"
                     , age = 28
                     , height = 157
                     , weight = 64
                     , sex = Female
                     , bloodType = BloodType A Neg }

patientSummary :: Patient -> String
patientSummary patient = "******************************************************\n" ++
                         "Patient name: " ++ showName (name patient) ++ "\n" ++
                         "Sex: " ++ showSex (sex patient) ++ "\n" ++
                         "Age: " ++ show (age patient) ++ "\n" ++
                         "Height: " ++ show (height patient) ++ "\n" ++
                         "Weight: " ++ show (weight patient) ++ "\n" ++
                         "Blood type: " ++ showBloodType (bloodType patient) ++ "\n" ++
                         "******************************************************\n"
main = do
  putStr (patientSummary jackieSmith)
  putStr (show (canDonateToPatient jackieSmith janeESmith))
  putStr "\n"
  putStr (show (canDonateToPatient janeESmith jackieSmith))
  putStr "\n"
