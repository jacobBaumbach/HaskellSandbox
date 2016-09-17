module MaybeLibrary where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing x = (not . isJust) x

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe alt _ Nothing = alt
mayybe alt f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe alt Nothing = alt
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes x = map f (filter isJust x) where
     f (Just a) = a

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x = case y of
     True -> Nothing
     False -> Just (catMaybes x)
   where
        y = or (map isNothing x)