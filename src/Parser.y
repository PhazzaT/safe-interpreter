{
module Parser where

import Lexer
import AST.Basic

import Control.Monad.Except
}

%name parse
%tokentype { Token }
%monad { Either String }
%error { parseError }

%token
    var    { Token _ TDVar }
    '('    { Token _ TDOpenParen }
    ')'    { Token _ TDCloseParen }
    ';'    { Token _ TDSemicolon }
    '='    { Token _ TDEquals }
    '+'    { Token _ TDPlus }
    '-'    { Token _ TDMinus }
    '*'    { Token _ TDMul }
    '/'    { Token _ TDDiv }
    ident  { Token _ (TDName $$) }
    number { Token _ (TDIntegerLiteral $$) }

%%

Program :: { Program }
Program : CommandList { Program $1 }

CommandList :: { [Command] }
CommandList : Command ';' CommandList { $1 : $3 }
            |                         { [] }

Command :: { Command }
Command : var VarRef '=' RValue0 { CDeclare $2 $4 }
        | LValue0 '=' RValue0    { CAssign $1 $3 }
        |                        { CSkip }

LValue0 :: { LValue }
LValue0 : VarRef { LVVariable $1 }

RValue0 :: { RValue }
RValue0 : RValue1 '+' RValue0 { RVOp (+) $1 $3 }
        | RValue1 '-' RValue0 { RVOp (-) $1 $3 }
        | RValue1             { $1 }

RValue1 :: { RValue }
RValue1 : RValue2 '*' RValue1 { RVOp (*) $1 $3 }
        | RValue2 '/' RValue1 { RVOp div $1 $3 }
        | RValue2             { $1 }

RValue2 :: { RValue }
RValue2 : LValue0         { RVFromLV $1 }
        | Literal         { RVLiteral $1 }
        | '(' RValue0 ')' { $2 }

VarRef :: { VarRef }
VarRef : ident { VR $1 }

Literal :: { Literal }
Literal : number { LInteger $1 }

listEnded(a, s) : a s listEnded(a, s) { $1 : $3 }
                |                     { [] }

{
parseError :: [Token] -> Either String a
parseError (tok : _) =
    throwError $ "Parse error at token " ++ show tok
parseError []     = throwError "Parse error - unexpected end of file"
}
