nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True
nand False True = True
nand False False = True

rsff :: Bool -> Bool -> Bool
rsff r s = q
  where 
    q = nand qbar s
    qbar = nand q r
