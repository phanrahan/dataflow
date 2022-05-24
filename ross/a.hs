import Data.Tuple (swap)

ross :: Bool -> Bool
ross x = x'
   where 
      (y', x') = swap (y', x)

-- ross True == True
-- ross False == False

