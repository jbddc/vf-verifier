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
      (\x -> mapM_ putStrLn =<< evalZ3 (do { vcs <- vcGen x ; w <- mapM astToString vcs ; return w }) )
      res