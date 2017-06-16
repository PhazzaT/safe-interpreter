{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE GADTs #-}


module ReferencesCheck where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import AST.Basic as B
import AST.StackSizeAnnotated as S
import TypeMagic


data RCState = RCState (M.Map B.VarName (G SNat)) (G SNat)
type M = StateT RCState (Except String)
type MG a = M (G a)
data G a where G :: a (b :: Nat) -> G a

emptyRCState :: RCState
emptyRCState = RCState M.empty (G SZ)

refCheck :: B.Program -> Either String (G S.Program, RCState)
refCheck p = runExcept $ runStateT (rcProgram p) emptyRCState

rcProgram :: B.Program -> MG S.Program
rcProgram (B.Program cmds) = do
    G cmds' <- rcCmds cmds
    return $ G $ S.Program (getStackSize cmds') cmds'

rcCmds :: [B.Command] -> MG (SSList S.Command)
rcCmds = foldr
    (\c -> (<*>) (mapG2 SSCons <$> rcCommand c))
    (return $ G $ SSNil SZ)

rcCommand :: B.Command -> MG S.Command
rcCommand B.CSkip = return $ G $ S.CSkip SZ
rcCommand (B.CAssign lv rv) = mapG2 S.CAssign <$> rcLValue lv <*> rcRValue rv
rcCommand (B.CDeclare vr@(B.VR name) rv) = do
    -- Order is important here - or we could be able to
    -- access the variable before it is assigned
    rv' <- rcRValue rv
    RCState vars (G mx) <- get
    case M.lookup name vars of
        Nothing -> do
            put $ RCState (M.insert name (G mx) vars) (G $ SS mx)
            mapG2 S.CDeclare <$> rcVarRef vr <*> pure rv'
        Just _ -> throwError $
            "Variable redeclared in the same scope: " ++ name
rcCommand (B.CScope cmds) = preserveScope $ mapG S.CScope <$> rcCmds cmds
rcCommand (B.CIf rv c1 c2) = do
    G rv' <- rcRValue rv
    G c1' <- preserveScope $ rcCommand c1
    G c2' <- preserveScope $ rcCommand c2
    case equalizeStacks c1' c2' of
        GenericSEPair c1'' c2'' ->
            case equalizeStacks rv' (SEPair (getStackSize c1'') c1'' c2'') of
                GenericSEPair rv'' (SEPair ss cc1 cc2) ->
                    return $ G $ S.CIf ss rv'' cc1 cc2
rcCommand (B.CWhile rc c) =
    mapG2 S.CWhile <$> rcRValue rc <*> preserveScope (rcCommand c)

rcLValue :: B.LValue -> MG S.LValue
rcLValue (B.LVVariable vr) = mapG S.LVVariable <$> rcVarRef vr

rcRValue :: B.RValue -> MG S.RValue
rcRValue (B.RVFromLV lv) = mapG S.RVFromLV <$> rcLValue lv
rcRValue (B.RVLiteral lit) = return $ G $ S.RVLiteral SZ $ rcLiteral lit
rcRValue (B.RVOp op rv1 rv2) =
    mapG2 (`S.RVOp` op) <$> rcRValue rv1 <*> rcRValue rv2

rcVarRef :: B.VarRef -> MG S.VarRef
rcVarRef (B.VR name) = do
    RCState vars _ <- get
    case M.lookup name vars of
        Just (G sn) -> return $ G $ S.VR (SS sn) sn STBase
        Nothing -> throwError $
            "Variable not in scope: " ++ name

rcLiteral :: B.Literal -> S.Literal
rcLiteral (B.LInteger i) = S.LInteger i

preserveScope :: M a -> M a
preserveScope m = do
    s <- get
    r <- m
    put s
    return r

natToSNat :: Nat -> G SNat
natToSNat Z = G SZ
natToSNat (S n) = case natToSNat n of G n' -> G $ SS n'

snatToNat :: G SNat -> Nat
snatToNat (G SZ) = Z
snatToNat (G (SS n)) = S $ snatToNat (G n)

mapG :: (StackExtendable se1, StackExtendable se2)
     => (forall ss. SNat ss -> se1 ss -> se2 ss) -> G se1 -> G se2
mapG f (G g) = G $ f (getStackSize g) g

mapG2 :: (StackExtendable se1, StackExtendable se2, StackExtendable se3)
      => (forall ss. SNat ss -> se1 ss -> se2 ss -> se3 ss)
      -> G se1 -> G se2 -> G se3
mapG2 f (G g1) (G g2) = case equalizeStacks g1 g2 of
    GenericSEPair g1' g2' -> G $ f (getStackSize g1') g1' g2'
