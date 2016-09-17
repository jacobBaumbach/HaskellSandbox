module Pali where
import Control.Monad
import Data.Char
palindrome :: IO ()
palindrome = forever $ do
	line1 <- getLine
	if ((f line1) == reverse (f line1)) then putStrLn "true" else putStrLn "false"
	where
	      f x = filter g (map toLower x) where
	           g z = a>97 && a<123 where
	                a = ord z

