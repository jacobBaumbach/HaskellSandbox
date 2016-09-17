module FearfulSymmetry where

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
elimNewLine = breakChar '\n'

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?" 
sentences = firstSen ++ secondSen++ thirdSen ++ fourthSen

