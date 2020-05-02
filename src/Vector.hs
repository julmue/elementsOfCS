{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vector where

class IsVector1 v where 
    _1 :: v a -> a
class IsVector1 v => IsVector2 v where 
    _2 :: v a -> a
class IsVector2 v => IsVector3 v where 
    _3 :: v a -> a
class IsVector3 v => IsVector4 v where 
    _4 :: v a -> a
class IsVector4 v => IsVector5 v where 
    _5 :: v a -> a
class IsVector5 v => IsVector6 v where 
    _6 :: v a -> a
class IsVector6 v => IsVector7 v where 
    _7 :: v a -> a
class IsVector7 v => IsVector8 v where 
    _8 :: v a -> a
class IsVector8 v => IsVector9 v where 
    _9 :: v a -> a
class IsVector9 v => IsVector10 v where 
    _10 :: v a -> a
class IsVector10 v => IsVector11 v where 
    _11 :: v a -> a
class IsVector11 v => IsVector12 v where 
    _12 :: v a -> a
class IsVector12 v => IsVector13 v where 
    _13 :: v a -> a
class IsVector13 v => IsVector14 v where 
    _14 :: v a -> a
class IsVector14 v => IsVector15 v where 
    _15 :: v a -> a
class IsVector15 v => IsVector16 v where 
    _16 :: v a -> a


data Vector1 a = Vector1 {
    _V1_1 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector1 where 
    _1 = _V1_1


data Vector2 a = Vector2 {
    _V2_1 :: a,
    _V2_2 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector2 where 
    _1 = _V2_1
instance IsVector2 Vector2 where 
    _2 = _V2_2


data Vector3 a = Vector3 {
    _V3_1 :: a,
    _V3_2 :: a,
    _V3_3 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector3 where 
    _1 = _V3_1
instance IsVector2 Vector3 where 
    _2 = _V3_2
instance IsVector3 Vector3 where 
    _3 = _V3_3


data Vector4 a = Vector4 {
    _V4_1 :: a,
    _V4_2 :: a,
    _V4_3 :: a,
    _V4_4 :: a
    } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector4 where 
    _1 = _V4_1
instance IsVector2 Vector4 where 
    _2 = _V4_2
instance IsVector3 Vector4 where 
    _3 = _V4_3
instance IsVector4 Vector4 where 
    _4 = _V4_4


data Vector5 a = Vector5 {
  _V5_1 :: a,
  _V5_2 :: a,
  _V5_3 :: a,
  _V5_4 :: a,
  _V5_5 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector5 where 
    _1 = _V5_1
instance IsVector2 Vector5 where 
    _2 = _V5_2
instance IsVector3 Vector5 where 
    _3 = _V5_3
instance IsVector4 Vector5 where 
    _4 = _V5_4
instance IsVector5 Vector5 where 
    _5 = _V5_5


data Vector6 a = Vector6 {
  _V6_1 :: a,
  _V6_2 :: a,
  _V6_3 :: a,
  _V6_4 :: a,
  _V6_5 :: a,
  _V6_6 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector6 where 
    _1 = _V6_1
instance IsVector2 Vector6 where 
    _2 = _V6_2
instance IsVector3 Vector6 where 
    _3 = _V6_3
instance IsVector4 Vector6 where 
    _4 = _V6_4
instance IsVector5 Vector6 where 
    _5 = _V6_5
instance IsVector6 Vector6 where 
    _6 = _V6_6



data Vector8 a = Vector8 {
  _V8_1 :: a,
  _V8_2 :: a,
  _V8_3 :: a,
  _V8_4 :: a,
  _V8_5 :: a,
  _V8_6 :: a,
  _V8_7 :: a,
  _V8_8 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector8 where 
    _1 = _V8_1
instance IsVector2 Vector8 where 
    _2 = _V8_2
instance IsVector3 Vector8 where 
    _3 = _V8_3
instance IsVector4 Vector8 where 
    _4 = _V8_4
instance IsVector5 Vector8 where 
    _5 = _V8_5
instance IsVector6 Vector8 where 
    _6 = _V8_6
instance IsVector7 Vector8 where 
    _7 = _V8_7
instance IsVector8 Vector8 where 
    _8 = _V8_8


data Vector12 a = Vector12 {
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
  _V12_12 :: a,
  _V12_13 :: a,
  _V12_14 :: a,
  _V12_15 :: a,
  _V12_16 :: a
  } deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance IsVector1 Vector12 where 
    _1 = _V12_01
instance IsVector2 Vector12 where 
    _2 = _V12_02
instance IsVector3 Vector12 where 
    _3 = _V12_03
instance IsVector4 Vector12 where 
    _4 = _V12_04
instance IsVector5 Vector12 where 
    _5 = _V12_05
instance IsVector6 Vector12 where 
    _6 = _V12_06
instance IsVector7 Vector12 where 
    _7 = _V12_07
instance IsVector8 Vector12 where 
    _8 = _V12_08
instance IsVector9 Vector12 where 
    _9 = _V12_09
instance IsVector10 Vector12 where 
    _10 = _V12_10
instance IsVector11 Vector12 where 
    _11 = _V12_11
instance IsVector12 Vector12 where 
    _12 = _V12_12


data Vector16 a = Vector16 {
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

instance IsVector1 Vector16 where 
    _1 = _V16_01
instance IsVector2 Vector16 where 
    _2 = _V16_02
instance IsVector3 Vector16 where 
    _3 = _V16_03
instance IsVector4 Vector16 where 
    _4 = _V16_04
instance IsVector5 Vector16 where 
    _5 = _V16_05
instance IsVector6 Vector16 where 
    _6 = _V16_06
instance IsVector7 Vector16 where 
    _7 = _V16_07
instance IsVector8 Vector16 where 
    _8 = _V16_08
instance IsVector9 Vector16 where 
    _9 = _V16_09
instance IsVector10 Vector16 where 
    _10 = _V16_10
instance IsVector11 Vector16 where 
    _11 = _V16_11
instance IsVector12 Vector16 where 
    _12 = _V16_12
instance IsVector13 Vector16 where 
    _13 = _V16_13
instance IsVector14 Vector16 where 
    _14 = _V16_14
instance IsVector15 Vector16 where 
    _15 = _V16_15
instance IsVector16 Vector16 where 
    _16 = _V16_16