type Stream a = [a]


c_and :: Stream (Bool, Bool) -> Stream Bool
c_and = map (uncurry (&&))

c_or :: Stream (Bool, Bool) -> Stream Bool
c_or = map (uncurry (||))

c_xor :: Stream (Bool, Bool) -> Stream Bool 
c_xor = map (uncurry (/=))

c_not :: Stream Bool -> Stream Bool 
c_not = map not

s_zip :: Stream a -> Stream b -> Stream (a, b)
s_zip (x:xs) (y:ys) = (x, y) : zip xs ys

s_proj1 :: Stream (a, b) -> Stream a
s_proj1 ((x, y):xys) = x : s_proj1 xys

s_proj2 :: Stream (a, b) -> Stream b
s_proj2 ((x, y):xys) = y : s_proj2 xys


reg:: a -> Stream a -> Stream a
reg init d = init : d

fs :: Stream Bool
fs = reg False fs

ts :: Stream Bool
ts = reg True ts


tff = q
  where 
    q = reg False d
    d = c_not q -- note recursive equations

ha :: Stream Bool -> Stream Bool -> Stream (Bool, Bool)
ha x c = s_zip (c_xor xc) (c_and xc) 
   where 
     xc = s_zip x c

count1 :: Stream Bool -> Stream (Bool, Bool)
count1 cin = s_zip c (s_proj2 sc)
  where
     sc = ha c cin
     c = reg False (s_proj1 sc)
