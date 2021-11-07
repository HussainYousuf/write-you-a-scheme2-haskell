-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module LispVal.Par where
import LispVal.Abs
import LispVal.Lex
import LispVal.ErrM

}

%name pExp Exp
%name pExp1 Exp1
%name pExp2 Exp2
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  '-' { PT _ (TS _ 5) }
  '/' { PT _ (TS _ 6) }

L_integ  { PT _ (TI $$) }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Exp :: { Exp }
Exp : Exp '+' Exp1 { LispVal.Abs.EAdd $1 $3 }
    | Exp '-' Exp1 { LispVal.Abs.ESub $1 $3 }
    | Exp1 { $1 }
Exp1 :: { Exp }
Exp1 : Exp1 '*' Exp2 { LispVal.Abs.EMul $1 $3 }
     | Exp1 '/' Exp2 { LispVal.Abs.EDiv $1 $3 }
     | Exp2 { $1 }
Exp2 :: { Exp }
Exp2 : Integer { LispVal.Abs.EInt $1 } | '(' Exp ')' { $2 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

