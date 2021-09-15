type Stream a = [a]

inv = map not

reg:: a -> Stream a -> Stream a
reg init d = init : d

tff = q
  where 
    q = reg False d
    d = inv q -- note recursive equations
