type Stream a = [a]

inv = map not

and = map
or = map
xor = map

reg:: a -> Stream a -> Stream a
reg init d = init : d

ha a cin = (s, cout)
  where 
    s = xor a cin
    cout = and a cin

fa a b cin = (s, cout)
  where 
    s = xor (a xor b) c
    cout = or (or (a and b) (b and c)) (c and a)

counter1 cin = count cout
   where
      (s, cout) = ha count cin
      count = reg 0 s
     


