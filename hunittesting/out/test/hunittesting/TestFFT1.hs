module TestFFT1 where

import Test.HUnit
import FunForTest
import FunForTest3

testFFT1:: Test
testFFT1 =
    TestCase $ assertEqual "true" "fft1" fft1

testFFT3:: Test
testFFT3 =
    TestCase $ assertEqual "true" "111" fft3

main :: IO Counts
main = runTestTT $ TestList [TestLabel "t1" testFFT1,TestLabel "t2" testFFT3]

