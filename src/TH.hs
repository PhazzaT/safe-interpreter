-- {-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH


makeStackExtendableInstance :: Name -> Q [Dec]
makeStackExtendableInstance name = do
    ssName <- newName "ss"
    let stackExtendableName = mkName "StackExtendable"
        getStackSizeName = mkName "getStackSize"
        extendStackOnceName = mkName "extendStackOnce"
        bigSSName = mkName "SS"
        makeCase (NormalC cnm (_:bts)) = do
            esoOtherParams <- mapM (const $ newName "x") bts
            let gss = Clause
                        [ConP cnm $ VarP ssName : map (const WildP) bts]
                        (NormalB (VarE ssName))
                        []
                esoFirstVar = AppE (ConE bigSSName) (VarE ssName)
                esoConvert x (_, AppT _ _) =
                    AppE (VarE extendStackOnceName) (VarE x)
                esoConvert x _ = VarE x
                esoOtherVars = zipWith esoConvert esoOtherParams bts
                esoAllVars = esoFirstVar : esoOtherVars
                eso = Clause
                        [ConP cnm $ VarP ssName : map VarP esoOtherParams]
                        (NormalB $
                            foldl AppE (ConE cnm) esoAllVars)
                        []
            return (gss, eso)
        makeCase (ForallC _ _ con) = makeCase con
        makeCase con = fail $ "Can't make a case for constructor: "
                ++ pprint con
    info <- reify name
    case info of
        TyConI (DataD _ nm [_] _ cons _) -> do
            (gsss, esos) <- unzip <$> mapM makeCase cons
            return . (:[]) $ InstanceD Nothing []
                                (AppT (ConT stackExtendableName)
                                (ConT nm))
                [ FunD getStackSizeName gsss
                , FunD extendStackOnceName esos ]
        _ -> fail $ "Invalid argument passed to makeStackExtendableInstance: "
                    ++ show name
