nand :: Bool -> Bool -> Bool
nand False _ = True
nand True  b = not b

rsff :: Bool -> Bool -> Bool
rsff rbar sbar = q
  where 
    q = nand sbar qbar 
    qbar = nand rbar q

e1 = rsff False True            -- False
e2 = rsff True  False           -- True
e3 = rsff False False           -- True
e4 = rsff True  True            -- ⊥
