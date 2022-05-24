Experiments in Haskell combinational loops.

$ python rsff.py
Traceback (most recent call last):
  File "/Users/hanrahan/git/dataflow/rsff/rsff.py", line 9, in <module>
    print(rsff(False, True))
  File "/Users/hanrahan/git/dataflow/rsff/rsff.py", line 5, in rsff
    q = nand( sbar, qbar )
UnboundLocalError: local variable 'qbar' referenced before assignment

-- Doesn't terminate because of the recursion
$ ghci rsff1.hs
*Main> e1
⊥

-- _ can handle bottom
$ ghci rsff2.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( rsff2.hs, interpreted )
Ok, one module loaded.
*Main> e1
False
*Main> e2
True
*Main> e3
True
*Main> e4
^CInterrupted.


