-- http://documentup.com/feuerbach/tasty

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Board ( board_DisplayString, initialBoard )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] -- [properties, unitTests]
{- 
properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

  -} 
unitTests = testGroup "Unit tests"
  [ testCase "board_DisplayString True initialBoard" $
    board_DisplayString True initialBoard @?= "   C0 C1 C2 C3 C4 C5 C6 C7 \nR0  -  -  -  -  -  -  -  - \nR1  -  -  -  -  -  -  -  - \nR2  -  -  -  -  -  -  -  - \nR3  -  -  -  o  x  -  -  - \nR4  -  -  -  x  o  -  -  - \nR5  -  -  -  -  -  -  -  - \nR6  -  -  -  -  -  -  -  - \nR7  -  -  -  -  -  -  -  - \n"
  ]
