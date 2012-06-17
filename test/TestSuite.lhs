\begin{code}
module Main where

import Test.Framework (defaultMain, testGroup)

import qualified GotoTests
import qualified RecTests

main :: IO ()
main
  = defaultMain
  [ testGroup "GotoTests" GotoTests.tests
  , testGroup "RecTests"  RecTests.tests
  ]
\end{code}
