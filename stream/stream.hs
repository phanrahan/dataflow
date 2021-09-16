type Stream a = [a]


s_zip :: Stream a -> Stream b -> Stream (a, b)
s_zip = zip

s_fst :: Stream (a, b) -> Stream a
s_fst = map fst

s_snd :: Stream (a, b) -> Stream b
s_snd = map snd


c_not :: Stream Bool -> Stream Bool 
c_not = map not

c_and :: Stream (Bool, Bool) -> Stream Bool
c_and = map (uncurry (&&))

c_or :: Stream (Bool, Bool) -> Stream Bool
c_or = map (uncurry (||))

c_xor :: Stream (Bool, Bool) -> Stream Bool 
c_xor = map (uncurry (/=))


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
count1 cin = s_zip c (s_snd sc)
  where
     sc = ha c cin
     c = reg False (s_fst sc)
