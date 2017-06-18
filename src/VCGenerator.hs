module VCGenerator where

import SLParser
import Z3.Monad

type Identifiers = Map String AST

vcGen :: MonadZ3 z3 => Identifiers -> SL -> z3 [AST]
vcGen (WithBoth l prec posc) = do
    x <- condition2VC idents prec
    y <- wp l posc
    ys <- vcAux l posc
    res <- mkImplies x y
    return (res:ys)
vcGen (Without l) = do
    x <- mkTrue
    y <- wp l (Equal (Constant 0) (Constant 0))
    ys <- vcAux l (Equal (Constant 0) (Constant 0))
    res <- mkImplies x y
    return (res:ys) 
vcGen (WithPre l prec) = do
    x <- condition2VC idents prec
    y <- wp l (Equal (Constant 0) (Constant 0))
    ys <- vcAux l (Equal (Constant 0) (Constant 0))
    res <- mkImplies x y
    return (res:ys) 
vcGen (WithPost l posc) = do 
    x <- mkTrue
    y <- wp l posc
    ys <- vcAux l posc
    res <- mkImplies x y
    return (res:ys)

vcAux :: MonadZ3 z3 => Identifiers -> [Expression] -> Condition -> z3 [AST]
vcAux [] c = return []
vcAux ((AssignmentStatement _ _):[]) c = return []
vcAux ((Conditional b ct cf):[]) c = do
    x <- vcAux ct c
    y <- vcAux cf c
    return (x++y)
vcAux ((Cycle b cc):[]) c = do
    inv <- mkTrue
    b' <- condition2VC idents b
    vc1esq <- mkAnd [inv,b']
    vc1dir <- wp cc (Equal (Constant 0) (Constant 0))
    vc1 <- mkImplies vc1esq vc1dir 
    b'' <- condition2VC idents (Not b)
    vc2esq <- mkAnd [inv,b'']
    vc2dir <- condition2VC idents c 
    vc2 <- mkImplies vc2esq vc2dir
    y <- vcAux cc (Equal (Constant 0) (Constant 0))
    return ([vc1,vc2]++y) 
vcAux ((CycleInv b inv cc):[]) c = do
    inv' <- condition2VC idents inv
    b' <- condition2VC idents b
    vc1esq <- mkAnd [inv',b']
    vc1dir <- wp cc inv
    vc1 <- mkImplies vc1esq vc1dir 
    b'' <- condition2VC idents (Not b)
    vc2esq <- mkAnd [inv',b'']
    vc2dir <- condition2VC idents c 
    vc2 <- mkImplies vc2esq vc2dir
    y <- vcAux cc inv
    return ([vc1,vc2]++y) 
vcAux (l:ls) c = do
    x <- vcAux [l] (wpcond ls c)
    y <- vcAux ls c 
    return (x++y)

wp :: MonadZ3 z3 => Identifiers -> [Expression] -> Condition -> z3 AST
wp a b = condition2VC idents $ wpcond a b

wpcond :: Identifiers -> [Expression] -> Condition -> Condition
wpcond [] c = c
wpcond ((AssignmentStatement s expr):[]) c = replaceQ idents s expr c
wpcond ((Conditional b ct cf):[]) q = And (Or (Not b) (wpcond ct q)) (Or (Not (Not b)) (wpcond cf q))
wpcond ((Cycle b c):[]) q = Equal (Constant 0) (Constant 0)
wpcond ((CycleInv b i c):[]) q = i
wpcond (l:ls) q = wpcond [l] (wpcond ls q)

replaceQ :: Identifiers -> String -> Expression -> Condition -> Condition
replaceQ idents s expr (And c1 c2) = And (replaceQ idents s expr c1) (replaceQ idents s expr c2)
replaceQ idents s expr (Or c1 c2)  = Or (replaceQ idents s expr c1) (replaceQ idents s expr c2)
replaceQ idents s expr (Not c)     = Not (replaceQ idents s expr c)
replaceQ idents s expr (Equal exp1 exp2) = Equal (replaceExp idents s expr exp1) (replaceExp idents s expr exp2)
replaceQ idents s expr (LessThan exp1 exp2) = LessThan (replaceExp idents s expr exp1) (replaceExp idents s expr exp2)
replaceQ idents s expr (LessThanEqual exp1 exp2) = LessThanEqual (replaceExp idents s expr exp1) (replaceExp idents s expr exp2)

replaceExp :: Identifiers -> String -> Expression -> Expression -> Expression
replaceExp idents s expr (Addition e1 e2) = Addition (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Subtraction e1 e2) = Subtraction (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Multiplication e1 e2) = Multiplication (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Division e1 e2) = Division (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Modulus e1 e2) = Modulus (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Negation e) = Negation (replaceExp idents s expr e) 
replaceExp idents s expr (AssignmentStatement ss ex) = if ss==s then AssignmentStatement ss expr else AssignmentStatement ss ex

condition2VC :: MonadZ3 z3 => Identifiers -> Condition -> z3 AST
condition2VC idents (And c1 c2) = do
    x <- condition2VC idents c1
    y <- condition2VC idents c2
    mkAnd [x,y]
condition2VC idents (Or c1 c2) = do
    x <- condition2VC idents c1
    y <- condition2VC idents c2
    mkOr [x,y]
condition2VC idents (Not c) = do
    x <- condition2VC idents c
    mkNot x
condition2VC idents (Equal c1 c2) = do
    x <- expression2VC idents c1
    y <- expression2VC idents c2
    mkEq x y
condition2VC idents (LessThan c1 c2) = do
    x <- expression2VC idents c1
    y <- expression2VC idents c2
    mkLt x y
condition2VC idents (LessThanEqual c1 c2) = do
    x <- expression2VC idents c1
    y <- expression2VC idents c2
    mkLe x y

expression2VC :: MonadZ3 z3 => Identifiers -> Expression -> z3 AST
expression2VC idents (Constant i) = mkInteger i
expression2VC idents (Identifier s) = map 
expression2VC idents (Addition e1 e2) = do
    x <- expression2VC idents e1
    y <- expression2VC idents e2
    mkAdd [x,y]
expression2VC idents (Subtraction e1 e2) = do
    x <- expression2VC idents e1
    y <- expression2VC idents e2
    mkSub [x,y]
expression2VC idents (Multiplication e1 e2) = do
    x <- expression2VC idents e1
    y <- expression2VC idents e2
    mkMul [x,y]
expression2VC idents (Division e1 e2) = do
    x <- expression2VC idents e1
    y <- expression2VC idents e2
    mkDiv x y
expression2VC idents (Modulus e1 e2) = do
    x <- expression2VC idents e1
    y <- expression2VC idents e2
    mkMod x y
expression2VC idents (Negation e) = do
    x <- expression2VC idents e
    mkUnaryMinus x
