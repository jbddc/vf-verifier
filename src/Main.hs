module Main where

import SLParser
import Text.Parsec

main :: IO ()
main = getContents >>= parseTest parseSL