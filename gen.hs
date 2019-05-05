import Data.List


games = ["Omaha", "Holdem"]

potTypes = ["NoLimit", "PotLimit"]

newbies = [1, 2]
  
limits = [2, 4, 6, 8, 10, 12, 50, 100, 200, 600, 2000, 999999]

main = putStrLn $ unlines [
  "\"" ++ intercalate "\",\"" 
    ["test-room", game, potType, show limit, show newbie, "0","0","0","0","0","0","0","1","1"] 
    ++ "\"" | 
  game <- games,
  potType <- potTypes,
  limit <- limits,
  newbie <- newbies
  ]