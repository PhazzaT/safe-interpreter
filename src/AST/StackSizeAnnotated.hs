{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AST.StackSizeAnnotated where

import TH
import TypeMagic


data Program (ss :: Nat) = Program (SNat ss) (SSList Command ss)

data SSList se (ss :: Nat) where
    SSNil  :: (StackExtendable se) => SNat ss -> SSList se ss
    SSCons :: (StackExtendable se)
           => SNat ss -> se ss -> SSList se ss -> SSList se ss

sslistToList :: SSList se ss -> [se ss]
sslistToList (SSNil _) = []
sslistToList (SSCons _ l ls) = l : sslistToList ls

ssCons :: (StackExtendable se) => se ss -> SSList se ss -> SSList se ss
ssCons se = SSCons (getStackSize se) se

data Command (ss :: Nat) where
    CSkip    :: SNat ss -> Command ss
    CAssign  :: SNat ss -> LValue ss -> RValue ss -> Command ss
    CDeclare :: SNat ss -> VarRef ss -> RValue ss -> Command ss
    CScope   :: SNat ss -> SSList Command ss -> Command ss

data LValue (ss :: Nat) where
    LVVariable :: SNat ss -> VarRef ss -> LValue ss

data RValue (ss :: Nat) where
    RVFromLV  :: SNat ss -> LValue ss -> RValue ss
    RVLiteral :: SNat ss -> Literal -> RValue ss
    RVOp      :: SNat ss -> Op -> RValue ss -> RValue ss -> RValue ss

data VarRef (ss :: Nat) where
    VR :: SNat ('S ss) -> SNat cell
       -> SmallerThan cell ('S ss) -> VarRef ('S ss)
newtype Literal = LInteger Integer
type Op = Integer -> Integer -> Integer

class StackExtendable se where
    getStackSize :: se ss -> SNat ss
    extendStackOnce :: se ss -> se ('S ss)

instance StackExtendable (SSList se) where
    getStackSize (SSNil ss) = ss
    getStackSize (SSCons ss _ _) = ss
    extendStackOnce (SSNil ss) = SSNil (SS ss)
    extendStackOnce (SSCons ss se ses)
        = SSCons (SS ss) (extendStackOnce se) (extendStackOnce ses)

makeStackExtendableInstance ''Program
makeStackExtendableInstance ''Command
makeStackExtendableInstance ''LValue
makeStackExtendableInstance ''RValue

instance StackExtendable VarRef where
    getStackSize (VR ss _ _) = ss
    extendStackOnce (VR ss cell st) = case st of
        STBase -> VR (SS ss) cell (STInduction STBase)
        st'@(STInduction _) -> VR (SS ss) cell (STInduction st')

extendStack :: (StackExtendable se)
            => SmallerThan ss1 ss2 -> se ss1 -> se ss2
extendStack STBase se = extendStackOnce se
extendStack (STInduction pf) se = extendStackOnce $ extendStack pf se

data GenericSEPair se1 se2 where
    GenericSEPair :: (StackExtendable se1, StackExtendable se2)
                  => se1 ss -> se2 ss -> GenericSEPair se1 se2

equalizeStacks :: (StackExtendable se1, StackExtendable se2)
               => se1 ss1 -> se2 ss2 -> GenericSEPair se1 se2
equalizeStacks se1 se2 =
    case compareSNats (getStackSize se1) (getStackSize se2) of
        CompEQ -> GenericSEPair se1 se2
        CompLT pf -> GenericSEPair (extendStack pf se1) se2
        CompGT pf -> GenericSEPair se1 (extendStack pf se2)
