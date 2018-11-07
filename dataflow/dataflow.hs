type Dataflow a = [a]

initial:: a -> Dataflow a -> Dataflow a
initial init d = init : d

inv = map not

tff = q
   where 
       q = initial False d
       d = inv q

-- merge is lazy, both x and y need not be present
merge :: Dataflow Bool -> Dataflow a -> Dataflow a -> Dataflow a
merge (False:bs) (x:xs) ys = x : merge bs xs ys
merge (True:bs) xs (y:ys) = y : merge bs xs ys

-- tgate act as switch controlled by the first argument

tgate :: Dataflow Bool -> Dataflow a -> Dataflow a
tgate (True:bs) (x:xs) = x : tgate bs xs
tgate (False:bs) (x:xs) = tgate bs xs

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
  
