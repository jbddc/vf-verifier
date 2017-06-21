module Main where

import SLParser
import Text.Parsec
import VCGenerator
import Z3.Monad
import Data.List

main :: IO ()
main = do 
    input <- getContents
    let res = parse parseSL "" input
    either
      print
      (\x -> putStrLn "AST:" >> print x >> putStrLn "" >> do
        (w,res) <- evalZ3 (script x)
        mapM_ (\(num,(a,b)) -> putStrLn ("VC "++(show num)++": ") >> putStrLn b >> putStr "Result: " >> treatResult a >> putStrLn "") $ zip [1..] $ zip res w
      )
      res
  where
  treatResult Unsat = putStrLn "Valid"
  treatResult Sat   = putStrLn "Invalid"


script :: SL -> Z3 ([String],[Result])
script x = do 
    (decls,vcs) <- vcGen x 
    w <- mapM astToString vcs 
    res <- foldr (\(str,k) accum -> if "(= " `isPrefixOf` str then foldrAux1 str k accum else foldrAux2 str k accum ) (return []) $ zip w vcs
    return (map fst res,map snd res)
  where foldrAux1 str k accum =  solverReset >> solverAssertCnstr k >> solverCheck >>= (\_ -> accum )  
        foldrAux2 str k accum =  solverReset >> mkNot k >>= solverAssertCnstr >> solverCheck >>= (\x -> do { ac <- accum; return ((str,x):ac); } )  
