module Main where

import Lambda (Tree (Branch, Leaf), fringe)

main :: IO ()
main = print (fringe (Branch (Leaf 1) (Leaf 2)))
