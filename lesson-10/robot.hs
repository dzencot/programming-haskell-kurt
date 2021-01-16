robot (name, attack, hp) = \message ->
                              message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 0
                 then getAttack aRobot
                 else 0

printRobot aRobot = aRobot (\(n, a, h) -> n ++ ". Attack: " ++ (show a) ++ ". HP: " ++ (show h))

killerRobot = robot ("Killer-1", 25, 200)
killerRobot2 = robot ("Killer-2", 30, 300)

-- threeRoundFightIter 0 robotA robotB = robotA
  -- where winner = if getHP robotA > getHP robotB then robotA else robotB
-- threeRoundFightIter counter robotA robotB = robotARound
--   where robotARound = fight robotB robotA
--         robotBRound = fight robotA robotB

threeRoundFight 0 robotA robotB = robotA
  -- where winner = if getHP robotA > 0
  --                then robotA
  --                else robotB
threeRoundFight count robotA robotB = winner
  where robotBRound1 = fight robotA robotB
        robotARound1 = fight robotBRound1 robotB
        robotBRound2 = fight robotARound1 robotBRound1
        robotARound2 = fight robotARound2 robotBRound1
        robotBRound3 = fight robotARound2 robotBRound2
        robotARound3 = fight robotARound3 robotBRound2
        winner = if getHP robotARound3
                 then robotARound3
                 else robotBRound3

main = do
    -- print (printRobot killerRobot2Round4)
    -- print (printRobot killerRobotRound4)
    -- print (map getHP [killerRobotRound4, killerRobot2Round4])
    -- print (printRobot (threeRoundFight killerRobot killerRobot2))
