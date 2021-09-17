type Stream a = [a]

s_id :: Stream a -> Stream a
s_id x = x

-- s_comp

-- s_const

s_prod :: Stream a -> Stream b -> Stream (a, b)
s_prod = zip

s_dup :: Stream a -> Stream (a, a)
s_dup s = s_prod s s

s_exl :: (Stream a -> Stream c) -> Stream (a, b) -> Stream c
s_exl f = f . (map fst)

s_exr :: (Stream b -> Stream c) -> Stream (a, b) -> Stream c
s_exr f = f . (map snd)

s_fst = s_exl id
s_snd = s_exr id


s_join :: (Stream a -> Stream b) -> (Stream a -> Stream c) -> Stream a -> Stream (b, c)
s_join f g s = s_prod (f s) (g s)


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

ha :: Stream (Bool, Bool) -> Stream (Bool, Bool)
ha = s_join c_xor c_and

count1 :: Stream Bool -> Stream (Bool, Bool)
count1 cin = s_prod c (s_snd sc)
  where
     sc = ha $ s_prod c cin
     c = reg False (s_fst sc)

count2 :: Stream Bool -> Stream ((Bool, Bool), Bool)
count2 cin = s_prod (s_prod (s_fst sc0) (s_fst sc1)) (s_snd sc1)
  where
     sc0 = count1 cin
     sc1 = count1 (s_snd sc0)
