module TestFFT1 where

import Test.HUnit
import FunForTest
import fun.FunForTest3

testSafeHeadForEmptyList :: Test
testSafeHeadForEmptyList = 
    TestCase $ assertEqual "true" "fft1" fft1

main :: IO Counts
main = runTestTT $ TestList [testSafeHeadForEmptyList]

