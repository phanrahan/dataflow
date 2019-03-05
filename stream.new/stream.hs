data Stream a = Reg a (Stream a)

instance Functor Stream where
    fmap f (Reg a as) = Reg (f a) (fmap f as)

reg = Reg

join2:: Stream a -> Stream b -> Stream (a,b)
join2 (Reg x xs) (Reg y ys) = Reg (x,y) $ join2 xs ys

fst2:: Stream (a,b) -> Stream a
fst2 = fmap fst
snd2:: Stream (a,b) -> Stream b
snd2 = fmap snd

ext:: Int -> Stream a -> [a]
ext n (Reg x xs) = if n <= 0 then [] else (x : ext (n-1) xs)

buf:: Stream a -> Stream a
buf = fmap id

data Bit = Zero | One deriving Show
show Zero = "0"
show One = "1"

ones = reg One ones
zeros = reg Zero zeros

bit_not:: Bit -> Bit 
bit_not One = Zero
bit_not Zero = One

bit_mux:: (Bit, (a, a)) -> a
bit_mux (One, (x, _)) = x
bit_mux (Zero, (_, y)) = y

mux:: Stream (Bit, (a, a)) -> Stream a
mux = fmap bit_mux

inv:: Stream Bit -> Stream Bit
inv = fmap bit_not

-- inv x = mux $ join2 x (join2 zeros, ones)
-- and2 x y = mux $ join2 x (join2 y x)
-- or2  x y = mux $ join2 x (join2 x y)
-- xor2 x y = mux $ join2 x (join2 (inv y) y) 

inc:: Stream Bit -> Stream Bit
inc x = q
   where 
       q = mux $ join2 x (join2 v (inv v))
       v = reg Zero $ mux $ join2 x (join2 v (inv x))

--add:: Stream (Bit, Bit) -> Stream Bit
--add (Stream (x, y)) = d
--   where 
--       a0 = mux $ join2 x (join2 ones b)
--       a1 = mux $ join2 x (join2 b zeros)
--       b = reg Zero $ mux $ join2 y (join2 a0 a1)
--       c = mux $ join2 x (join2 (inv b) b)
--       d = mux $ join2 y (join2 (inv c) c)

-- fulladder a b c
--    where 
--      b0 = mux b b a
--      b1 = mux b a b
--      b2 = mux b (inv a) a
--      r = mux c b0 b1
--      s = mux c (inv b2) b2
       

tff = q
   where
       q = reg Zero $ reg One $ q

twothirds = p
   where
       p = reg One $ reg One $ q
       q = reg Zero $ reg One $ q
