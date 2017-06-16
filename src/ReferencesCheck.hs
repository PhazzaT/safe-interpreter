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
type M = ExceptT String (State RCState)
type MG a = M (G a)
data G a where G :: a (b :: Nat) -> G a

emptyRCState :: RCState
emptyRCState = RCState M.empty (G SZ)

refCheck :: B.Program -> Either String (G S.Program)
refCheck p = evalState (runExceptT $ rcProgram p) emptyRCState

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

rcLValue :: B.LValue -> MG S.LValue
rcLValue (B.LVVariable vr) = mapG S.LVVariable <$> rcVarRef vr

rcRValue :: B.RValue -> MG S.RValue
rcRValue (B.RVFromLV lv) = mapG S.RVFromLV <$> rcLValue lv
rcRValue (B.RVLiteral lit) = return $ G $ S.RVLiteral SZ $ rcLiteral lit
rcRValue (B.RVAdd rv1 rv2) = mapG2 S.RVAdd <$> rcRValue rv1 <*> rcRValue rv2

rcVarRef :: B.VarRef -> MG S.VarRef
rcVarRef (B.VR name) = do
    RCState vars _ <- get
    case M.lookup name vars of
        Just (G sn) -> return $ G $ S.VR (SS sn) sn STBase
        Nothing -> throwError $
            "Variable not in scope: " ++ name

rcLiteral :: B.Literal -> S.Literal
rcLiteral (B.LInteger i) = S.LInteger i

natToSNat :: Nat -> G SNat
natToSNat Z = G SZ
natToSNat (S n) = case natToSNat n of G n' -> G $ SS n'

mapG :: (StackExtendable se1, StackExtendable se2)
     => (forall ss. SNat ss -> se1 ss -> se2 ss) -> G se1 -> G se2
mapG f (G g) = G $ f (getStackSize g) g

mapG2 :: (StackExtendable se1, StackExtendable se2, StackExtendable se3)
      => (forall ss. SNat ss -> se1 ss -> se2 ss -> se3 ss)
      -> G se1 -> G se2 -> G se3
mapG2 f (G g1) (G g2) = case equalizeStacks g1 g2 of
    GenericSEPair g1' g2' -> G $ f (getStackSize g1') g1' g2'
