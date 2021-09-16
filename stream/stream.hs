type Stream a = [a]


s_zip :: Stream a -> Stream b -> Stream (a, b)
s_zip = zip

s_dup :: Stream a -> Stream (a, a)
s_dup s = s_zip s s

s_fst :: Stream (a, b) -> Stream a
s_fst = map fst

s_snd :: Stream (a, b) -> Stream b
s_snd = map snd

s_join :: (Stream a -> Stream c) -> (Stream b -> Stream d) -> Stream (a, b) -> Stream (c, d)
s_join f g s = s_zip (f $ s_fst s) (g $ s_snd s)


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
ha = (s_join c_xor c_and) . s_dup

count1 :: Stream Bool -> Stream (Bool, Bool)
count1 cin = s_zip c (s_snd sc)
  where
     sc = ha $ s_zip c cin
     c = reg False (s_fst sc)

count2 :: Stream Bool -> Stream ((Bool, Bool), Bool)
count2 cin = s_zip (s_zip (s_fst sc0) (s_fst sc1)) (s_snd sc1)
  where
     sc0 = count1 cin
     sc1 = count1 (s_snd sc0)
