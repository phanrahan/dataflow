type Stream a = [a]

reg:: a -> Stream a -> Stream a
reg init d = init : d

forever x = reg x $ forever x

lo = forever False
hi = forever True

buf = map id
inv = map not

tff = q
   where 
       q = reg False d
       d = inv q
