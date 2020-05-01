{-# NoImplicitPrelude #-}

module LogicSeq.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Logic
import LogicSeq

import qualified Data.Stream as S
import Control.Monad.State

-- signal generators
i :: Signal Bit
i = S.repeat I 

o :: Signal Bit
o = S.repeat O

alternating :: Signal Bit
alternating = S.interleave i o

listToSignal :: [a] -> Signal a
listToSignal = S.fromList

signalToList :: Signal a -> [a]
signalToList = S.toList
-- 

run :: s -> State s a -> a
run = flip evalState

logicSeqTests = testGroup "logicSeqTests" [
      dffTests
    , bitTests
    , registerTests
    ]

dffTests = testGroup "dffTests" [
      testCase "dff 1" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run O (dff i) 
        in [t0, t1, t2, t3, t4] @?= [O,I,I,I,I] 
    , testCase "dff 2" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run O (dff o) 
        in [t0, t1, t2, t3, t4] @?= [O,O,O,O,O]
    , testCase "dff 3" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run I (dff i) 
        in [t0, t1, t2, t3, t4] @?= [I,I,I,I,I]
    , testCase "dff 4" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run I (dff o) 
        in [t0, t1, t2, t3, t4] @?= [I,O,O,O,O] 
    , testCase "dff 5" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run O (dff alternating) 
        in [t0, t1, t2, t3, t4] @?= [O,I,O,I,O] 
    , testCase "dff 5" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run I (dff alternating) 
        in [t0, t1, t2, t3, t4] @?= [I,I,O,I,O] 
    ]

bitTests = testGroup "bitTests" [
      testCase "bit 1" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run O (bit alternating o)
        in [t0, t1, t2, t3, t4] @?= [O,O,O,O,O]
    , testCase "bit 2" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run I (bit alternating o)
        in [t0, t1, t2, t3, t4] @?= [I,I,I,I,I]
    , testCase "bit 3" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run I (bit o i)
        in [t0, t1, t2, t3, t4] @?= [I,O,O,O,O]
    , testCase "bit 4" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run O (bit i i)
        in [t0, t1, t2, t3, t4] @?= [O,I,I,I,I]
    , testCase "bit 5" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run O (bit alternating i)
        in [t0, t1, t2, t3, t4] @?= [O,I,O,I,O]
    , testCase "bit 6" $
        let [t0, t1, t2, t3, t4] = S.take 5 $ run I (bit alternating i)
        in [t0, t1, t2, t3, t4] @?= [I,I,O,I,O] 
 
    ]

registerTests = testGroup "registerTests" [

    ]
