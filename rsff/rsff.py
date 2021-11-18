def nand(x,y):
   return ~(x and y)

def rsff( rbar, sbar ):
    q = nand( sbar, qbar )
    qbar = nand( rbar, q )
    return q

print(rsff(False, True))
