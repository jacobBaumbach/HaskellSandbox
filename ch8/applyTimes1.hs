module ApplyTimes1 where

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b

applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

result = applyTimes 5 (+1) 5