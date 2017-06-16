{-# LANGUAGE GADTs #-}

module Execute where

import Control.Monad.State

import AST.StackSizeAnnotated
import TypeMagic


type EState ss = Vector Integer ss
type EMonad ss = State (EState ss)


execute :: Program ss -> Vector Integer ss
execute (Program ss cmds) =
    execState (exeCmds cmds) (vreplicate ss 0)

exeCmds :: SSList Command ss -> EMonad ss ()
exeCmds cmds = mapM_ exeCommand $ sslistToList cmds

exeCommand :: Command ss -> EMonad ss ()
exeCommand (CSkip _) = return ()
exeCommand (CAssign _ lv rv) = do
    let addr = getLValueAddress lv
    val <- calcRValue rv
    modify $ vput addr val
exeCommand (CDeclare ss vr rv) = exeCommand (CAssign ss (LVVariable ss vr) rv)
exeCommand (CScope _ cmds) = exeCmds cmds
exeCommand (CIf _ rv c1 c2) = do
    val <- calcRValue rv
    if val > 0
        then exeCommand c1
        else exeCommand c2
exeCommand c@(CWhile _ rv c') = do
    val <- calcRValue rv
    when (val > 0) $ do
        exeCommand c'
        exeCommand c

calcRValue :: RValue ss -> EMonad ss Integer
calcRValue (RVFromLV _ lv) = gets $ vget (getLValueAddress lv)
calcRValue (RVLiteral _ lit) = return $ calcLiteral lit
calcRValue (RVOp _ op rv1 rv2) = op <$> calcRValue rv1 <*> calcRValue rv2

calcLiteral :: Literal -> Integer
calcLiteral (LInteger i) = i

getLValueAddress :: LValue ss -> Ordinal ss
getLValueAddress (LVVariable _ vr) = getVarRefAddress vr

getVarRefAddress :: VarRef ss -> Ordinal ss
getVarRefAddress (VR ss cell st)
    = stpToOrdinal $ stToST' cell ss st
