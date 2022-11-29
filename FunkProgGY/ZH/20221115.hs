import Data.Char
import Data.List

database =
  [ ("KIA", [("Optima", "plug-in", 5), ("Soul", "electric", 2), ("CEE'D", "plug-in", 2), ("CEE'D", "gasoline", 12), ("XCEE'D", "plug-in", 3)])
  , ("Renault", [("Zoe", "electric", 4), ("Megane", "plug-in", 2), ("Megane", "gasoline", 5), ("Captur", "plug-in", 1)])
  , ("Nissan", [("Leaf", "electric", 6), ("Qashqai", "mild-hybrid", 1), ("X-Trail", "gasoline", 2)])
  , ("Opel", [("Corsa", "gasoline", 2), ("Corsa", "electric", 3)])
  , ("Toyota", [("RAV4", "hybrid", 4), ("Camry", "hybrid", 2), ("Corolla", "hybrid", 9), ("Prius", "plug-in", 3)])
  , ("Ssangyong", [("Korando", "diesel", 4), ("Rexton", "diesel", 2)])
  , ("Mazda", [("MX-30", "electric", 1), ("3", "mild-hybrid", 2), ("6", "gasoline", 1)])
  ]

-- greenBrands :: [(String, [(String, String, Integer)])] -> Int
-- adatbázis bejárása (párokból áll a db (brand, cars) és a carst tovább definiáljuk (typ, fuel, count) majd szűrjük)
-- végül a nub kiszűri a duplimátumokat a length pedig visszaadja az eredmény hosszát :
greenBrands xs = length $ nub [brand | (brand, cars) <- xs, (typ, fuel, count) <- cars, fuel == "electric" || fuel == "plug-in"]