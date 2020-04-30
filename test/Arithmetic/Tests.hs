{-# NoImplicitPrelude #-}

module Arithmetic.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Arithmetic
import Data.Bool (Bool(..))

arithmeticTests = testGroup "arithmeticTests" [
      halfAdderTests
    , fullAdderTests
    ]

halfAdderTests = testGroup "hallAdderTests" [
      testCase "halfAdder False False" $ 
        halfAdder False False @?= (False, False)
    , testCase "halfAdder False True" $ 
        halfAdder False True @?= (False, True)
    , testCase "halfAdder True False" $ 
        halfAdder True False @?= (False, True)
    , testCase "halfAdder True True" $ 
        halfAdder True True @?= (True, False) 
    ]

fullAdderTests = testGroup "fullAdderTests" [
      testCase "fullAdder False False False" $ 
        fullAdder False False False @?= (False, False)

    , testCase "fullAdder False False True" $ 
        fullAdder False False True @?= (False, True)

    , testCase "fullAdder False True False" $ 
        fullAdder False True False @?= (False, True)

    , testCase "fullAdder False True True" $ 
        fullAdder False True True @?= (True, False)

    , testCase "fullAdder True False False" $ 
        fullAdder True False False @?= (False, True)

    , testCase "fullAdder True False False" $ 
        fullAdder True False False @?= (False, True)

    , testCase "fullAdder True True False" $ 
        fullAdder True True False @?= (True, False)

    , testCase "fullAdder True True True" $ 
        fullAdder True True True @?= (True, True)
    ]