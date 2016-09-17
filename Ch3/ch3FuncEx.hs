module Ch3FuncEx where


gC :: Int -> String -> Char
gC x y = y !! x

a x = x ++ "!"

b x = (gC 4) x

c x = drop 9 x

 
thirdLetter x = (gC 2) x

stringThenInt :: String -> Int -> Char
stringThenInt x y = x !! y
stringGrab :: String -> Int -> Char
stringGrab x = (stringThenInt x)

rvrs :: String -> String
rvrs x = concat[(drop 9 x), (take 4 (drop 5 x)), (take 5 x)]