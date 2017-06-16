{-# LANGUAGE GADTs #-}

module Execute where

import Control.Monad.State

import AST.StackSizeAnnotated
import TypeMagic


type EState ss = Vector Integer ss
type EMonad ss = State (EState ss)


execute :: Program ss -> Vector Integer ss
execute (Program ss cmds) =
    execState (mapM exeCommand $ sslistToList cmds) (vreplicate ss 0)

exeCommand :: Command ss -> EMonad ss ()
exeCommand (CSkip _) = return ()
exeCommand (CAssign _ lv rv) = do
    let addr = getLValueAddress lv
    val <- calcRValue rv
    modify $ vput addr val
exeCommand (CDeclare ss vr rv) = exeCommand (CAssign ss (LVVariable ss vr) rv)

calcRValue :: RValue ss -> EMonad ss Integer
calcRValue (RVFromLV _ lv) = gets $ vget (getLValueAddress lv)
calcRValue (RVLiteral _ lit) = return $ calcLiteral lit
calcRValue (RVAdd _ rv1 rv2) = (+) <$> calcRValue rv1 <*> calcRValue rv2

calcLiteral :: Literal -> Integer
calcLiteral (LInteger i) = i

getLValueAddress :: LValue ss -> Ordinal ss
getLValueAddress (LVVariable _ vr) = getVarRefAddress vr

getVarRefAddress :: VarRef ss -> Ordinal ss
getVarRefAddress (VR ss cell st)
    = stpToOrdinal $ stToST' cell ss st
