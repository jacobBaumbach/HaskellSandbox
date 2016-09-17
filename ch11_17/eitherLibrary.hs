module EitherLibrary where

lefts' :: [Either a b] -> [a]
lefts' x = foldr f [] x where
     f (Right b) acc = acc
     f (Left a) acc = a:acc

rights' :: [Either a b] -> [b]
rights' x = foldr f [] x where
     f (Left a) acc = acc
     f (Right b) acc = b:acc


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = foldr f ([],[]) x where
     f (Left a) (l,r) = (a:l, r)
     f (Right b) (l,r) = (l, b:r)


partitionEithers1' :: [Either a b] -> ([a], [b])
partitionEithers1' x = (lefts' x, rights' xe)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b


eitherMaybe'' :: (b -> c)  -> Either a b -> Maybe c
eitherMaybe'' f ethr = either' (\x -> Nothing) (\x -> Just (f x)) ethr


