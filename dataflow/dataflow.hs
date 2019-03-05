type Dataflow a = [a]

initial:: a -> Dataflow a -> Dataflow a
initial init d = init : d

inv = map not

tff = q
   where 
       q = initial False d
       d = inv q -- note recursive values

-- functional semantics eliminates the need for fork

-- firing rules implemented as pattern matching

-- merge two streams into a single stream controlled by the first argument.
-- Merge is lazy, both x and y need not be present,
-- and if x is forwarded, y still stays around.

merge :: Dataflow Bool -> Dataflow a -> Dataflow a -> Dataflow a
merge (False:bs) (x:xs) ys = x : merge bs xs ys
merge (True:bs) xs (y:ys) = y : merge bs xs ys

-- tgate acts as switch controlled by the first argument.
-- The second argument is forwarded only if the first argument is True.
-- tgate is a filter. The number of outputs can be less than 
-- the number of inputs.

tgate :: Dataflow Bool -> Dataflow a -> Dataflow a
tgate (True:bs) (x:xs) = x : tgate bs xs
tgate (False:bs) (x:xs) = tgate bs xs

-- fgate outputs a token if the first argument is False
fgate :: Dataflow Bool -> Dataflow a -> Dataflow a
fgate (False:bs) (x:xs) = x : fgate bs xs
fgate (True:bs) (x:xs) = fgate bs xs

-- swtich can be done with a tgate and fgate pair

-- gate outputs a token if the first argument is present, else continues
gate :: Dataflow Bool -> Dataflow a -> Dataflow a
gate (_:bs) (x:xs) = x : gate bs xs


down x = y
    where 
       y =  tgate a x
       a = gate x b
       b = initial True $ inv a

up x = y
    where 
       y =  merge b x x
       a = gate y b
       b = initial True $ inv a
  
