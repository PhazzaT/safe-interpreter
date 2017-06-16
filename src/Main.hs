module Main where

import AST.StackSizeAnnotated
import Execute
import Lexer
import Parser
import ReferencesCheck
import TypeMagic

import Data.List
import qualified Data.Map as M
import System.Exit


main :: IO ()
main = do
    input <- getContents
    toks <- errorIO $ lexer input
    ast1 <- errorIO $ parse toks
    (G ast2, RCState vars visSize) <- errorIO $ refCheck ast1

    let showGSNat n = show $ natToInteger $ snatToNat n
    let Program ss _ = ast2
    putStrLn $ "Global variables: " ++ showGSNat visSize
    putStrLn $ "Inferred stack size: " ++ showGSNat (G ss)

    let vec = execute ast2
    let assocs = associateVars vars $ vecToList vec
    putStrLn $ intercalate ", " $ map (\(v, i) -> v ++ " = " ++ show i) assocs

associateVars :: M.Map String (G SNat) -> [Integer] -> [(String, Integer)]
associateVars vars = zip names
    where names = map fst $ sortOn (snatToNat . snd) $ M.toList vars

errorIO :: (Show e) => Either e a -> IO a
errorIO (Left e) = die $ show e
errorIO (Right a) = return a
