module GreetIfCool2 where

greetIfCool :: String -> IO()
greetIfCool coolness = 
	if cool coolness
		then putStrLn "true value"
	else 
		putStrLn "false value"
	where cool v = v == "true"