{-# LANGUAGE TemplateHaskell #-}

module Lexer where

import Control.Lens
import Text.Parsec hiding (token)
import Text.Parsec.String

type PMonad = Parsec String ()

data Token = Token { tokenPos :: SourcePos, tokenData :: TokenData } deriving Show

data TokenData
    = TDIntegerLiteral Integer
    | TDName String
    | TDSemicolon
    | TDColon
    | TDEquals
    | TDPlus
    | TDSquareOpen
    | TDSquareClose
    | TDVar
    deriving (Eq, Ord, Show)

makePrisms ''TokenData

lexer :: String -> Either ParseError [Token]
lexer = runP (token `sepBy` spaces) () "<stdin>"

token :: PMonad Token
token =
    Token <$> getPosition <*> choice
        [ TDIntegerLiteral <$> intLiteral
        , stringyThing
        , char ';' >> return TDSemicolon
        , char ':' >> return TDColon
        , char '=' >> return TDEquals
        , char '+' >> return TDPlus
        , char '[' >> return TDSquareOpen
        , char ']' >> return TDSquareClose
        ]

intLiteral :: PMonad Integer
intLiteral = read <$> many1 digit

stringyThing :: PMonad TokenData
stringyThing = do
    str <- (:) <$> letter <*> many alphaNum
    case str of
        "var" -> return TDVar
        _ -> return $ TDName str
