module Employ where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
     putStrLn $ show e ++ " is the boss of "++ e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
                -> Employee
                -> Employee
                -> IO ()

employeeRank f e e' =
     case f e e'
     GT -> reportBoss e e'
     EQ -> putStrLn "Equals"
     LT -> (flip reportBoss) e e'