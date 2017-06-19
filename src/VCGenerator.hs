module VCGenerator (vcGen) where

import SLParser
import Z3.Monad
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

type Identifiers = TVar (Map.Map String AST)

vcGen :: MonadZ3 z3 => SL -> z3 ([AST],[AST])
vcGen (WithBoth l prec posc pose) = do
    (iMap,attribs,sl') <- genericGen l
    res <- vcGen' iMap (WithBoth sl' prec posc pose)
    decls <- liftIO $ readTVarIO iMap
    return ((Map.elems decls),attribs++res)
vcGen (WithPre l prec) = do
    (iMap,attribs,sl') <- genericGen l
    res <- vcGen' iMap (WithPre sl' prec)
    decls <- liftIO $ readTVarIO iMap
    return ((Map.elems decls),attribs++res)
vcGen (WithPost l posc pose) = do
    (iMap,attribs,sl') <- genericGen l
    res <- vcGen' iMap (WithPost sl' posc pose)
    decls <- liftIO $ readTVarIO iMap
    return ((Map.elems decls),attribs++res)
vcGen (Without l) = do
    (iMap,attribs,sl') <- genericGen l
    res <- vcGen' iMap (Without sl')
    decls <- liftIO $ readTVarIO iMap
    return ((Map.elems decls),attribs++res)
    
genericGen :: MonadZ3 z3 => [Expression] -> z3 (Identifiers,[AST],[Expression])
genericGen sl = do
    let (sls,decls) = foldr foldAux ([],[]) sl
    kvs <- foldr yafa (return []) decls
    imap <- liftIO $ newTVarIO (Map.fromList kvs)
    attribs <- mapM (\((DeclarationStatement _ val),ast) -> do { _val <- expression2VC imap val ;mkEq ast _val} ) $ zip decls (map snd kvs)
    return (imap,attribs,sls)
  where 
      yafa (DeclarationStatement i val) acc = do
          accc <- acc
          newVar <- mkFreshIntVar i
          return $ (i,newVar):accc
      foldAux x@(DeclarationStatement a b) (s,d) = (s,x:d)
      foldAux x (s,d) = (x:s,d)

vcGen' :: MonadZ3 z3 => Identifiers -> SL -> z3 [AST]
vcGen' idents (WithBoth l prec posc _) = do
    x <- condition2VC idents prec
    y <- wp idents l posc
    ys <- vcAux idents l posc
    res <- mkImplies x y
    return (res:ys)
vcGen' idents (Without l) = do
    x <- mkTrue
    y <- wp idents l (Boolean True)
    ys <- vcAux idents l (Boolean True)
    res <- mkImplies x y
    return (res:ys) 
vcGen' idents (WithPre l prec) = do
    x <- condition2VC idents prec
    y <- wp idents l (Boolean True)
    ys <- vcAux idents l (Boolean True)
    res <- mkImplies x y
    return (res:ys) 
vcGen' idents (WithPost l posc _) = do 
    x <- mkTrue
    y <- wp idents l posc
    ys <- vcAux idents l posc
    res <- mkImplies x y
    return (res:ys)

vcAux :: MonadZ3 z3 => Identifiers -> [Expression] -> Condition -> z3 [AST]
vcAux idents [] c = return []
vcAux idents ((AssignmentStatement _ _):[]) c = return []
vcAux idents ((Conditional b ct cf):[]) c = do
    x <- vcAux idents ct c
    y <- vcAux idents cf c
    return (x++y)
vcAux idents ((Cycle b cc):[]) c = do
    inv <- mkTrue
    b' <- condition2VC idents b
    vc1esq <- mkAnd [inv,b']
    vc1dir <- wp idents cc  (Boolean True)
    vc1 <- mkImplies vc1esq vc1dir 
    b'' <- condition2VC idents (Not b)
    vc2esq <- mkAnd [inv,b'']
    vc2dir <- condition2VC idents c 
    vc2 <- mkImplies vc2esq vc2dir
    y <- vcAux idents cc (Boolean True)
    return ([vc1,vc2]++y) 
vcAux idents ((CycleInv b inv cc):[]) c = do
    inv' <- condition2VC idents inv
    b' <- condition2VC idents b
    vc1esq <- mkAnd [inv',b']
    vc1dir <- wp idents cc inv
    vc1 <- mkImplies vc1esq vc1dir 
    b'' <- condition2VC idents (Not b)
    vc2esq <- mkAnd [inv',b'']
    vc2dir <- condition2VC idents c 
    vc2 <- mkImplies vc2esq vc2dir
    y <- vcAux idents cc inv
    return ([vc1,vc2]++y) 
vcAux idents (l:ls) c = do
    x <- vcAux idents [l] (wpcond idents ls c)
    y <- vcAux idents ls c 
    return (x++y)

wp :: MonadZ3 z3 => Identifiers -> [Expression] -> Condition -> z3 AST
wp idents a b = condition2VC idents $ wpcond idents a b

wpcond :: Identifiers -> [Expression] -> Condition -> Condition
wpcond idents [] c = c
wpcond idents ((AssignmentStatement s expr):[]) c = replaceQ idents s expr c
wpcond idents ((Conditional b ct cf):[]) q = And (Or (Not b) (wpcond idents ct q)) (Or (Not (Not b)) (wpcond idents cf q))
wpcond idents ((Cycle b c):[]) q = Boolean True 
wpcond idents ((CycleInv b i c):[]) q = i
wpcond idents (l:ls) q = wpcond idents [l] (wpcond idents ls q)

replaceQ :: Identifiers -> String -> Expression -> Condition -> Condition
replaceQ idents s expr (And c1 c2) = And (replaceQ idents s expr c1) (replaceQ idents s expr c2)
replaceQ idents s expr (Or c1 c2)  = Or (replaceQ idents s expr c1) (replaceQ idents s expr c2)
replaceQ idents s expr (Not c)     = Not (replaceQ idents s expr c)
replaceQ idents s expr (Equal exp1 exp2) = Equal (replaceExp idents s expr exp1) (replaceExp idents s expr exp2)
replaceQ idents s expr (LessThan exp1 exp2) = LessThan (replaceExp idents s expr exp1) (replaceExp idents s expr exp2)
replaceQ idents s expr (LessThanEqual exp1 exp2) = LessThanEqual (replaceExp idents s expr exp1) (replaceExp idents s expr exp2)
replaceQ idents s expr (Boolean b) = Boolean b

replaceExp :: Identifiers -> String -> Expression -> Expression -> Expression
replaceExp idents s expr (Constant i) = Constant i
replaceExp idents s expr (Identifier ss) = if ss==s then expr else (Identifier ss)
replaceExp idents s expr (Addition e1 e2) = Addition (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Subtraction e1 e2) = Subtraction (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Multiplication e1 e2) = Multiplication (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Division e1 e2) = Division (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Modulus e1 e2) = Modulus (replaceExp idents s expr e1) (replaceExp idents s expr e2)
replaceExp idents s expr (Negation e) = Negation (replaceExp idents s expr e) 
replaceExp idents s expr (AssignmentStatement ss ex) = if ss==s then AssignmentStatement ss expr else AssignmentStatement ss ex
replaceExp idents s expr (Cycle c ex) = Cycle (replaceQ idents s expr c) (map (\x -> (replaceExp idents s expr x)) ex)
replaceExp idents s expr (CycleInv c1 c2 ex) = CycleInv (replaceQ idents s expr c1) (replaceQ idents s expr c2) (map (\x -> (replaceExp idents s expr x)) ex)
replaceExp idents s expr (Conditional c ex1 ex2) = Conditional (replaceQ idents s expr c) (map (\x -> (replaceExp idents s expr x)) ex1) (map (\x -> (replaceExp idents s expr x)) ex2)

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
condition2VC idents (Boolean True) = mkTrue
condition2VC idents (Boolean False) = mkFalse

expression2VC :: MonadZ3 z3 => Identifiers -> Expression -> z3 AST
expression2VC idents (Constant i) = mkInteger i
expression2VC idents (Identifier s) = do
    iMap <- liftIO $ readTVarIO idents
    case (Map.lookup s iMap) of
        Nothing -> do
            x <- mkFreshIntVar s
            let newMap = Map.insert s x iMap
            liftIO $ atomically $ writeTVar idents newMap
            return x
        Just x -> return x
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
