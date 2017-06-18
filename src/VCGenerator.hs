module VCGenerator where

import SLParser

sl2VCs :: SL -> [Condition]
sl2VCs (WithPre l c)      = undefined 
sl2VCs (WithPost l c)     = undefined
sl2VCs (WithBoth l c1 c2) = undefined
sl2VCs (Without l)        = undefined

expr2VC :: Expression -> [Condition]
expr2VC (CycleInv c inv _) = Or (Not (And inv c)) inv


condition2VC :: Condition -> Condition
condition2VC (And c1 c2)           = And (condition2VC c1) (condition2VC c2)
condition2VC (Or c1 c2)            = Or (condition2VC c1) (condition2VC c2)
condition2VC (Not c)               = Not (condition2VC c)
condition2VC (Equal c1 c2)         = Equal (expr2VC c1) (expr2VC c2)
condition2VC (LessThan c1 c2)      = LessThan (expr2VC c1) (expr2VC c2)
condition2VC (LessThanEqual c1 c2) = LessThanEqual (expr2VC c1) (expr2VC c2)