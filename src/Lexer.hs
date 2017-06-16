{-# LANGUAGE TemplateHaskell #-}

module Lexer where

import Control.Lens
import Data.Char
import Text.Parsec hiding (token)

type PMonad = Parsec String ()

data Token = Token { tokenPos :: SourcePos, tokenData :: TokenData } deriving Show

data TokenData
    = TDIntegerLiteral Integer
    | TDName String
    | TDSemicolon
    | TDEquals
    | TDPlus
    | TDMinus
    | TDMul
    | TDDiv
    | TDOpenParen
    | TDCloseParen
    | TDOpenCurly
    | TDCloseCurly
    | TDVar
    deriving (Eq, Ord, Show)

makePrisms ''TokenData

lexer :: String -> Either ParseError [Token]
lexer = runP (token `sepBy` spaces) () "<stdin>" . trim

trim :: String -> String
trim = trimSuffix . trimPrefix

trimPrefix :: String -> String
trimPrefix (c:cs)
    | isSpace c = trimPrefix cs
    | otherwise = c:cs
trimPrefix [] = []

trimSuffix :: String -> String
trimSuffix = reverse . trimPrefix . reverse

token :: PMonad Token
token =
    Token <$> getPosition <*> choice
        [ TDIntegerLiteral <$> intLiteral
        , stringyThing
        , char ';' >> return TDSemicolon
        , char '=' >> return TDEquals
        , char '+' >> return TDPlus
        , char '-' >> return TDMinus
        , char '*' >> return TDMul
        , char '/' >> return TDDiv
        , char '(' >> return TDOpenParen
        , char ')' >> return TDCloseParen
        , char '{' >> return TDOpenCurly
        , char '}' >> return TDCloseCurly
        ]

intLiteral :: PMonad Integer
intLiteral = read <$> many1 digit

stringyThing :: PMonad TokenData
stringyThing = do
    str <- (:) <$> letter <*> many alphaNum
    case str of
        "var" -> return TDVar
        _ -> return $ TDName str
