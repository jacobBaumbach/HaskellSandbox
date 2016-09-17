module Scanex where

fibs = 1: scanl (+) 1 fibs
numFibs x = take x fibs
fibsBelow x = takeWhile (\y -> y<=x) fibs

fctrl = scanl (\x -> \y -> y*(x)) 1 (enumFrom 1)
factorial x = take x fctrl