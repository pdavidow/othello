module Main where

import Board ( board_DisplayString, initialBoard )


main :: IO ()
main = putStr $ board_DisplayString True initialBoard
