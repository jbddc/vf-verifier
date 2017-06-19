module Main where

import SLParser
import Text.Parsec
import VCGenerator
import Z3.Monad

main :: IO ()
main = do 
    input <- getContents
    let res = parse parseSL "" input
    either
      print
      (\x -> putStrLn "AST:" >> print x >> putStrLn "" >> do
        (w,res) <- evalZ3 (script x)
        mapM_ (\(num,(a,b)) -> putStrLn ("VC "++(show num)++": ") >> putStrLn b >> putStr "Result: " >> print a >> putStrLn "") $ zip [1..] $ zip res w
      )
      res


script :: SL -> Z3 ([String],[Result])
script x = do 
  (decls,vcs) <- vcGen x 
  w <- mapM astToString vcs 
  res <- mapM (\k -> reset >> assert k >> check) vcs
  return (w,res)