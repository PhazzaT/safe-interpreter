{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# Language RankNTypes #-}

module TypeMagic where

import Data.Kind()
-- import Data.Proxy
-- import Data.Type.Equality
import Data.Typeable


data Nat = Z | S Nat deriving (Eq, Ord, Typeable)

data SNat n where
    SZ :: SNat 'Z
    SS :: SNat n -> SNat ('S n)
deriving instance Typeable (SNat n)

data SmallerThan n m where
    STBase      :: SmallerThan n ('S n)
    STInduction :: SmallerThan n ('S m) -> SmallerThan n ('S ('S m))

data SmallerThan' n m where
    STBase'      :: SmallerThan' 'Z ('S m)
    STInduction' :: SmallerThan' n ('S m) -> SmallerThan' ('S n) ('S ('S m))

stToST' :: SNat n -> SNat ('S m) -> SmallerThan n ('S m) -> SmallerThan' n ('S m)
stToST' SZ (SS _) _ = STBase'
stToST' (SS n) (SS m) STBase = STInduction' $ stToST' n m STBase
stToST' (SS n) (SS (SS m)) (STInduction pf) =
    STInduction' $ stToST' n (SS m) (stTransitivity STBase pf)

-- stpToST :: SNat n -> SNat n -> SmallerThan' n ('S m) -> SmallerThan n ('S m)
-- stpToST SZ (SS m) STBase' = STBase
-- stpToST SZ (SS m) (STInduction' pf) = STInduction' $ stpToST SZ m pf
-- stpToST (SS n) (SS m) = 

st'Transitivity :: SmallerThan' k l
                -> SmallerThan' l m
                -> SmallerThan' k m
st'Transitivity STBase' (STInduction' _) = STBase'
st'Transitivity (STInduction' pf1) (STInduction' pf2)
    = STInduction' $ st'Transitivity pf1 pf2

stTransitivity :: SmallerThan k l
               -> SmallerThan l m
               -> SmallerThan k m
stTransitivity STBase STBase = STInduction STBase
stTransitivity (STInduction pf) STBase = STInduction $ STInduction pf
stTransitivity pf1 (STInduction pf2) =
    let pfx = stTransitivity pf1 pf2
    in STInduction pfx

stGreaterThanZero :: SNat ('S n) -> SmallerThan 'Z ('S n)
stGreaterThanZero (SS SZ) = STBase
stGreaterThanZero (SS ss'@(SS _)) = STInduction $ stGreaterThanZero ss'

stPlusOne :: SmallerThan n ('S m) -> SmallerThan ('S n) ('S ('S m))
stPlusOne STBase = STBase
stPlusOne (STInduction pf) = STInduction $ stPlusOne pf

data Compare n m where
    CompLT :: SmallerThan n m -> Compare n m
    CompEQ :: Compare n n
    CompGT :: SmallerThan m n -> Compare n m

-- compareSNats :: SNat n -> SNat m -> Compare n m
-- compareSNats SZ SZ = CompEQ
-- compareSNats SZ (SS _) = CompLT STBase'
-- compareSNats (SS _) SZ = CompGT STBase'
-- compareSNats (SS n) (SS m) = case compareSNats n m of
--     CompEQ -> CompEQ
--     CompLT STBase' -> CompLT $ STInduction' STBase'
--     CompLT pf@STInduction'{} -> CompLT $ STInduction' pf
--     CompGT STBase' -> CompGT $ STInduction' STBase'
--     CompGT pf@STInduction'{} -> CompGT $ STInduction' pf

compareSNats :: SNat n -> SNat m -> Compare n m
compareSNats SZ SZ = CompEQ
compareSNats SZ m@(SS _) = CompLT $ stGreaterThanZero m
compareSNats n@(SS _) SZ = CompGT $ stGreaterThanZero n
compareSNats (SS n) (SS m) = case compareSNats n m of
    CompEQ -> CompEQ
    CompLT STBase -> CompLT STBase
    CompLT pf@STInduction{} -> CompLT $ stPlusOne pf
    CompGT STBase -> CompGT STBase
    CompGT pf@STInduction{} -> CompGT $ stPlusOne pf

data Vector a (n :: Nat) where
    VNil  :: Vector a 'Z
    VCons :: a -> Vector a n -> Vector a ('S n)

data Ordinal (n :: Nat) where
    OZ :: Ordinal ('S n)
    OS :: Ordinal n -> Ordinal ('S n)

stpToOrdinal :: SmallerThan' n ('S m) -> Ordinal ('S m)
stpToOrdinal STBase' = OZ
stpToOrdinal (STInduction' pf) = OS $ stpToOrdinal pf

vreplicate :: SNat n -> a -> Vector a n
vreplicate SZ _     = VNil
vreplicate (SS n) a = VCons a $ vreplicate n a

vget :: Ordinal m -> Vector a m -> a
vget OZ (VCons a _) = a
vget (OS o) (VCons _ as) = vget o as

vput :: Ordinal m -> a -> Vector a m -> Vector a m
vput OZ a (VCons _ as) = VCons a as
vput (OS o) a (VCons a' as) = VCons a' (vput o a as)
