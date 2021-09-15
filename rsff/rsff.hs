nand :: Bool -> Bool -> Bool
nand False _ = True
nand _ False = True
nand True True = False

-- Or either of the following equivalent definitions

-- nand False _ = True
-- nand True  b = not b

-- nand a b = if a then not b else True

rsff :: Bool -> Bool -> Bool
rsff r s = q
  where 
    q = nand s qbar 
    qbar = nand r q

e1 = rsff False True            -- False
e2 = rsff True  False           -- True
e3 = rsff False False           -- True
e4 = rsff True  True            -- ⊥
