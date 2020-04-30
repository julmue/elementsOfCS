{-# LANGUAGE NoImplicitPrelude #-}

module Logic.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Logic
import Data.Bool (Bool(..))

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
      testCase "not True" $ 
        not True @=? False
    , testCase "not False" $ 
        not False @=? True
    ]

andTests = testGroup "andTests" [
      testCase "and True True" $
        and True True @=? True
    , testCase "and True False" $
        and True False @=? False
    , testCase "and False True" $
        and False True @=? False
    , testCase "and False False" $
        and False False @=? False
    ]

orTests = testGroup "orTests" [
      testCase "or True True" $
        or True True @=? True
    , testCase "or False True" $
        or False True @=? True
    , testCase "or True False" $
        or True False @=? True
    , testCase "or False False" $
        or False False @=? False
    ]

xorTests = testGroup "xorTests" [
      testCase "xor True True" $
        xor True True @=? False
    , testCase "xor False True" $
        xor False True @=? True
    , testCase "xor True False" $
        xor True False @=? True
    , testCase "xor False False" $
        xor False False @=? False
    ]

muxTests = testGroup "muxTests" [
      testCase "mux True True False" $
        mux True True False @=? True
    , testCase "mux True False False" $
        mux True False False @=? True
    , testCase "mux False True False" $
        mux False True False @=? False
    , testCase "mux False False False" $
        mux False False False @=? False

    , testCase "mux True True True" $
        mux True True True @=? True
    , testCase "mux True False True" $
        mux True False True @=? False
    , testCase "mux False True True" $
        mux False True True @=? True
    , testCase "mux False False True" $
        mux False False True @=? False
    ]

dmuxTests = testGroup "dmuxTests" [
      testCase "dmux True True" $
        dmux True True @=? (False, True)
    , testCase "dmux False True" $
        dmux False True @=? (False, False)
    , testCase "dmux True False" $
        dmux True False @=? (True, False)
    , testCase "dmux True False" $
        dmux False False @=? (False, False) 
    ]