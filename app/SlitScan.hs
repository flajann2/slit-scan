-- main for SlitScan, cli.

module Main where

import CommandLine
import Scan
         
main :: IO ()
main = do
  parms <- commandline
  scanFromParms parms  
