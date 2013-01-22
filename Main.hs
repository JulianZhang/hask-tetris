module Main where

import Layout
import Signal
import Logic

main :: IO ()
main = initTetrisLayout >>= initFieldData >>= registerSignals >>= runTetris >> return ()
     