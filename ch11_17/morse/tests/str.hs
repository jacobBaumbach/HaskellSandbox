module DD where

deDup :: (Eq a) => [a] -> [a]
deDup lst = foldr f [] ldst where
    f i acc = if (elem i acc) then acc else i:acc 

