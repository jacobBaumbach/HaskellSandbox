module WhereEx where

a = x
     where x = 5

b = x*x
     where x = 5

c = x*y
     where x = 5
           y = 6

d = x + 3
     where x =3
           y =1000

e = x*3+y
     where x = 3
           y = 1000

f = x*5 
     where y = 10
           x = 10*5+y

g = z/x+5
     where x = 7
           y = negate x
           z = y*10
