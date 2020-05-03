{-# LANGUAGE NoImplicitPrelude #-}

module Logic.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Vector
import Logic

import Prelude (($))

logicTests = testGroup "logicTests" [
      notTests
    , andTests
    , orTests
    , xorTests
    , muxTests
    , dmuxTests
    ]

notTests = testGroup "notTests" [
      testCase "not I" $ 
        not I @=? O
    , testCase "not O" $ 
        not O @=? I
    ]

andTests = testGroup "andTests" [
      testCase "and I I" $
        and I I @=? I
    , testCase "and I O" $
        and I O @=? O
    , testCase "and O I" $
        and O I @=? O
    , testCase "and O O" $
        and O O @=? O
    ]

orTests = testGroup "orTests" [
      testCase "or I I" $
        or I I @=? I
    , testCase "or O I" $
        or O I @=? I
    , testCase "or I O" $
        or I O @=? I
    , testCase "or O O" $
        or O O @=? O
    ]

xorTests = testGroup "xorTests" [
      testCase "xor I I" $
        xor I I @=? O
    , testCase "xor O I" $
        xor O I @=? I
    , testCase "xor I O" $
        xor I O @=? I
    , testCase "xor O O" $
        xor O O @=? O
    ]

muxTests = testGroup "muxTests" [
      testCase "mux I I O" $
        mux I I O @=? I
    , testCase "mux I O O" $
        mux I O O @=? I
    , testCase "mux O I O" $
        mux O I O @=? O
    , testCase "mux O O O" $
        mux O O O @=? O

    , testCase "mux I I I" $
        mux I I I @=? I
    , testCase "mux I O I" $
        mux I O I @=? O
    , testCase "mux O I I" $
        mux O I I @=? I
    , testCase "mux O O I" $
        mux O O I @=? O
    ]

dmuxTests = testGroup "dmuxTests" [
      testCase "dmux I I" $
        dmux I I @=? (V2 O I)
    , testCase "dmux O I" $
        dmux O I @=? (V2 O O)
    , testCase "dmux I O" $
        dmux I O @=? (V2 I O)
    , testCase "dmux I O" $
        dmux O O @=? (V2 O O) 
    ]