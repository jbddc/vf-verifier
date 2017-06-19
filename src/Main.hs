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
      (\x -> (\(w,res) -> mapM_ print res >> mapM_ putStrLn w) =<< evalZ3 (do { (decls,vcs) <- vcGen x ; w <- mapM astToString vcs ; {-mapM_ assert decls;-} res <- mapM (\k -> solverAssertCnstr k >> solverCheck) vcs; return (w,res) }))
      res