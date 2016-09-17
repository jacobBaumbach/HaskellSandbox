module WaxOn where

z=7
y=z+8 --15
x=y^2 --225
waxOn = x * 5 
      where x = y^2--1125

mult x y = x*y
triple x = x*3

waxOff x = mult y y
     where y = mult (triple x) $ 1/3+1 -1

waxOff' x = let y = mult (triple x) $ 1/3 +1 -1 in mult y y

