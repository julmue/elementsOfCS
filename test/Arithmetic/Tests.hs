{-# NoImplicitPrelude #-}

module Arithmetic.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Vector
import Logic
import Arithmetic

arithmeticTests = testGroup "arithmeticTests" [
      halfAdderTests
    , fullAdderTests
    ]

halfAdderTests = testGroup "hallAdderTests" [
      testCase "halfAdder O O" $ 
        halfAdder O O @?= (V2 O O)
    , testCase "halfAdder O I" $ 
        halfAdder O I @?= (V2 O I)
    , testCase "halfAdder I O" $ 
        halfAdder I O @?= (V2 O I)
    , testCase "halfAdder I I" $ 
        halfAdder I I @?= (V2 I O) 
    ]

fullAdderTests = testGroup "fullAdderTests" [
      testCase "fullAdder O O O" $ 
        fullAdder O O O @?= (V2 O O)

    , testCase "fullAdder O O I" $ 
        fullAdder O O I @?= (V2 O I)

    , testCase "fullAdder O I O" $ 
        fullAdder O I O @?= (V2 O I)

    , testCase "fullAdder O I I" $ 
        fullAdder O I I @?= (V2 I O)

    , testCase "fullAdder I O O" $ 
        fullAdder I O O @?= (V2 O I)

    , testCase "fullAdder I O O" $ 
        fullAdder I O O @?= (V2 O I)

    , testCase "fullAdder I I O" $ 
        fullAdder I I O @?= (V2 I O)

    , testCase "fullAdder I I I" $ 
        fullAdder I I I @?= (V2 I I)
    ]