module Main where

import Execute
import Lexer
import Parser
import ReferencesCheck
import TypeMagic

import System.Exit


main :: IO ()
main = do
    input <- getContents
    toks <- errorIO $ lexer input
    ast1 <- errorIO $ parse toks
    G ast2 <- errorIO $ refCheck ast1
    let vec = execute ast2
    print $ vecToList vec

errorIO :: (Show e) => Either e a -> IO a
errorIO (Left e) = die $ show e
errorIO (Right a) = return a
