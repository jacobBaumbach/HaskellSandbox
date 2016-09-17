module FilterEx where
answer1 :: Integral a => [a] -> [a]
answer1 lst = filter (\x -> mod x 3 == 0) lst

answer2 x = (length . answer1) x

breakChar :: Char -> String ->[String]

breakChar brk str = go str [] where
     dw = dropWhile (/= brk)
     tw = takeWhile (/= brk)
     go x acc
          | (dw x) =="" = acc++[x]
          | otherwise = go y (acc++z) where
          	    y = (tail . dw) x
          	    z = [tw x]

elimSpace = breakChar ' '

elimWords :: [String] -> String -> [String]
elimWords elimSet input = filter (\x -> (not (elem x elimSet))) (elimSpace input)
elimArticles = elimWords ["the", "a", "an"]