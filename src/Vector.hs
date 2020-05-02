{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vector where

class IsV1 v where 
    _1 :: v a -> a
class IsV1 v => IsV2 v where 
    _2 :: v a -> a
class IsV2 v => IsV3 v where 
    _3 :: v a -> a
class IsV3 v => IsV4 v where 
    _4 :: v a -> a
class IsV4 v => IsV5 v where 
    _5 :: v a -> a
class IsV5 v => IsV6 v where 
    _6 :: v a -> a
class IsV6 v => IsV7 v where 
    _7 :: v a -> a
class IsV7 v => IsV8 v where 
    _8 :: v a -> a
class IsV8 v => IsV9 v where 
    _9 :: v a -> a
class IsV9 v => IsV10 v where 
    _10 :: v a -> a
class IsV10 v => IsV11 v where 
    _11 :: v a -> a
class IsV11 v => IsV12 v where 
    _12 :: v a -> a
class IsV12 v => IsV13 v where 
    _13 :: v a -> a
class IsV13 v => IsV14 v where 
    _14 :: v a -> a
class IsV14 v => IsV15 v where 
    _15 :: v a -> a
class IsV15 v => IsV16 v where 
    _16 :: v a -> a


data V1 a = V1 {
    _V1_1 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V1 where 
    _1 = _V1_1


data V2 a = V2 {
    _V2_1 :: a,
    _V2_2 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V2 where 
    _1 = _V2_1
instance IsV2 V2 where 
    _2 = _V2_2


data V3 a = V3 {
    _V3_1 :: a,
    _V3_2 :: a,
    _V3_3 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V3 where 
    _1 = _V3_1
instance IsV2 V3 where 
    _2 = _V3_2
instance IsV3 V3 where 
    _3 = _V3_3


data V4 a = V4 {
    _V4_1 :: a,
    _V4_2 :: a,
    _V4_3 :: a,
    _V4_4 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V4 where 
    _1 = _V4_1
instance IsV2 V4 where 
    _2 = _V4_2
instance IsV3 V4 where 
    _3 = _V4_3
instance IsV4 V4 where 
    _4 = _V4_4


data V5 a = V5 {
  _V5_1 :: a,
  _V5_2 :: a,
  _V5_3 :: a,
  _V5_4 :: a,
  _V5_5 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V5 where 
    _1 = _V5_1
instance IsV2 V5 where 
    _2 = _V5_2
instance IsV3 V5 where 
    _3 = _V5_3
instance IsV4 V5 where 
    _4 = _V5_4
instance IsV5 V5 where 
    _5 = _V5_5


data V6 a = V6 {
  _V6_1 :: a,
  _V6_2 :: a,
  _V6_3 :: a,
  _V6_4 :: a,
  _V6_5 :: a,
  _V6_6 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V6 where 
    _1 = _V6_1
instance IsV2 V6 where 
    _2 = _V6_2
instance IsV3 V6 where 
    _3 = _V6_3
instance IsV4 V6 where 
    _4 = _V6_4
instance IsV5 V6 where 
    _5 = _V6_5
instance IsV6 V6 where 
    _6 = _V6_6


data V8 a = V8 {
  _V8_1 :: a,
  _V8_2 :: a,
  _V8_3 :: a,
  _V8_4 :: a,
  _V8_5 :: a,
  _V8_6 :: a,
  _V8_7 :: a,
  _V8_8 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V8 where 
    _1 = _V8_1
instance IsV2 V8 where 
    _2 = _V8_2
instance IsV3 V8 where 
    _3 = _V8_3
instance IsV4 V8 where 
    _4 = _V8_4
instance IsV5 V8 where 
    _5 = _V8_5
instance IsV6 V8 where 
    _6 = _V8_6
instance IsV7 V8 where 
    _7 = _V8_7
instance IsV8 V8 where 
    _8 = _V8_8


data V9 a = V9 {
  _V9_01 :: a,
  _V9_02 :: a,
  _V9_03 :: a,
  _V9_04 :: a,
  _V9_05 :: a,
  _V9_06 :: a,
  _V9_07 :: a,
  _V9_08 :: a,
  _V9_09 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V9 where 
    _1 = _V9_01
instance IsV2 V9 where 
    _2 = _V9_02
instance IsV3 V9 where 
    _3 = _V9_03
instance IsV4 V9 where 
    _4 = _V9_04
instance IsV5 V9 where 
    _5 = _V9_05
instance IsV6 V9 where 
    _6 = _V9_06
instance IsV7 V9 where 
    _7 = _V9_07
instance IsV8 V9 where 
    _8 = _V9_08
instance IsV9 V9 where 
    _9 = _V9_09


data V12 a = V12 {
  _V12_01 :: a,
  _V12_02 :: a,
  _V12_03 :: a,
  _V12_04 :: a,
  _V12_05 :: a,
  _V12_06 :: a,
  _V12_07 :: a,
  _V12_08 :: a,
  _V12_09 :: a,
  _V12_10 :: a,
  _V12_11 :: a,
  _V12_12 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V12 where 
    _1 = _V12_01
instance IsV2 V12 where 
    _2 = _V12_02
instance IsV3 V12 where 
    _3 = _V12_03
instance IsV4 V12 where 
    _4 = _V12_04
instance IsV5 V12 where 
    _5 = _V12_05
instance IsV6 V12 where 
    _6 = _V12_06
instance IsV7 V12 where 
    _7 = _V12_07
instance IsV8 V12 where 
    _8 = _V12_08
instance IsV9 V12 where 
    _9 = _V12_09
instance IsV10 V12 where 
    _10 = _V12_10
instance IsV11 V12 where 
    _11 = _V12_11
instance IsV12 V12 where 
    _12 = _V12_12


data V14 a = V14 {
  _V14_01 :: a,
  _V14_02 :: a,
  _V14_03 :: a,
  _V14_04 :: a,
  _V14_05 :: a,
  _V14_06 :: a,
  _V14_07 :: a,
  _V14_08 :: a,

  _V14_09 :: a,
  _V14_10 :: a,
  _V14_11 :: a,
  _V14_12 :: a,
  _V14_13 :: a,
  _V14_14 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsV1 V14 where 
    _1 = _V14_01
instance IsV2 V14 where 
    _2 = _V14_02
instance IsV3 V14 where 
    _3 = _V14_03
instance IsV4 V14 where 
    _4 = _V14_04
instance IsV5 V14 where 
    _5 = _V14_05
instance IsV6 V14 where 
    _6 = _V14_06
instance IsV7 V14 where 
    _7 = _V14_07
instance IsV8 V14 where 
    _8 = _V14_08
instance IsV9 V14 where 
    _9 = _V14_09
instance IsV10 V14 where 
    _10 = _V14_10
instance IsV11 V14 where 
    _11 = _V14_11
instance IsV12 V14 where 
    _12 = _V14_12
instance IsV13 V14 where 
    _13 = _V14_13
instance IsV14 V14 where 
    _14 = _V14_14


data V16 a = V16 {
  _V16_01 :: a,
  _V16_02 :: a,
  _V16_03 :: a,
  _V16_04 :: a,
  _V16_05 :: a,
  _V16_06 :: a,
  _V16_07 :: a,
  _V16_08 :: a,

  _V16_09 :: a,
  _V16_10 :: a,
  _V16_11 :: a,
  _V16_12 :: a,
  _V16_13 :: a,
  _V16_14 :: a,
  _V16_15 :: a,
  _V16_16 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

zipWithV16 :: (a -> b -> c) -> V16 a -> V16 b -> V16 c
zipWithV16 f
    (V16 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16)
    (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16) =
        (V16 
            (f a01 b01)
            (f a02 b02)
            (f a03 b03)
            (f a04 b04)

            (f a05 b05)
            (f a06 b06)
            (f a07 b07)
            (f a08 b08)

            (f a09 b09)
            (f a10 b10)
            (f a11 b11)
            (f a12 b12)

            (f a13 b13)
            (f a14 b14)
            (f a15 b15)
            (f a16 b16))

zipWith3V16 :: (a -> b -> c -> d) -> V16 a -> V16 b -> V16 c -> V16 d
zipWith3V16 f
    (V16 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16)
    (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16)
    (V16 c01 c02 c03 c04 c05 c06 c07 c08 c09 c10 c11 c12 c13 c14 c15 c16) =
        (V16 
            (f a01 b01 c01)
            (f a02 b02 c02)
            (f a03 b03 c03)
            (f a04 b04 c04)

            (f a05 b05 c05)
            (f a06 b06 c06)
            (f a07 b07 c07)
            (f a08 b08 c08)

            (f a09 b09 c01)
            (f a10 b10 c10)
            (f a11 b11 c11)
            (f a12 b12 c12)

            (f a13 b13 c13)
            (f a14 b14 c14)
            (f a15 b15 c15)
            (f a16 b16 c16))

instance IsV1 V16 where 
    _1 = _V16_01
instance IsV2 V16 where 
    _2 = _V16_02
instance IsV3 V16 where 
    _3 = _V16_03
instance IsV4 V16 where 
    _4 = _V16_04
instance IsV5 V16 where 
    _5 = _V16_05
instance IsV6 V16 where 
    _6 = _V16_06
instance IsV7 V16 where 
    _7 = _V16_07
instance IsV8 V16 where 
    _8 = _V16_08
instance IsV9 V16 where 
    _9 = _V16_09
instance IsV10 V16 where 
    _10 = _V16_10
instance IsV11 V16 where 
    _11 = _V16_11
instance IsV12 V16 where 
    _12 = _V16_12
instance IsV13 V16 where 
    _13 = _V16_13
instance IsV14 V16 where 
    _14 = _V16_14
instance IsV15 V16 where 
    _15 = _V16_15
instance IsV16 V16 where 
    _16 = _V16_16

replicateV1 :: a -> V1 a
replicateV1 = V1

replicateV2 :: a -> V2 a
replicateV2 a = V2 a a 

replicateV3 :: a -> V3 a
replicateV3 a = V3 a a a 

replicateV4 :: a -> V4 a
replicateV4 a = V4 a a a a

replicateV5 :: a -> V5 a
replicateV5 a = V5 a a a a a

replicateV6 :: a -> V6 a
replicateV6 a = V6 a a a a a a

replicateV8 :: a -> V8 a
replicateV8 a = V8 a a a a  a a a a 

replicateV16 :: a -> V16 a
replicateV16 a = V16 a a a a  a a a a  a a a a  a a a a 