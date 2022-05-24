nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True
nand False True = True
nand False False = True

rsff :: Bool -> Bool -> Bool
rsff rbar sbar = q
  where 
    q = nand sbar qbar
    qbar = nand rbar q

e1 = rsff False True            -- ⊥
e2 = rsff True  False           -- ⊥
e3 = rsff False False           -- V
e4 = rsff True  True            -- ⊥

