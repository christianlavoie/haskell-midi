module Main (main) where

import qualified System.IO as IO

import Parser

main :: IO ()
main = Parser.process IO.stdin
