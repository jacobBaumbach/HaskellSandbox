
module GreetIfCool1 where

greetIfCool :: String -> IO()

greetIfCool coolness =
	if cool
		then putStrLn "true value"
	else
		putStrLn "false value"
	where cool = coolness == "true"