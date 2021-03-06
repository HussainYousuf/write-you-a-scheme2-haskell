-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.3).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module LispVal.Par
  ( happyError
  , myLexer
  , pProgram
  , pLispVal
  , pListLispVal
  ) where

import Prelude

import qualified LispVal.Abs
import LispVal.Lex
import qualified Data.Text

}

%name pProgram Program
%name pLispVal LispVal
%name pListLispVal ListLispVal
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '\''         { PT _ (TS _ 1)          }
  '('          { PT _ (TS _ 2)          }
  ')'          { PT _ (TS _ 3)          }
  L_Ident      { PT _ (TV $$)           }
  L_quoted     { PT _ (TL $$)           }
  L_LispBool   { PT _ (T_LispBool $$)   }
  L_LispNumber { PT _ (T_LispNumber $$) }

%%

Ident :: { LispVal.Abs.Ident }
Ident  : L_Ident { LispVal.Abs.Ident $1 }

String  :: { String }
String   : L_quoted { (Data.Text.unpack $1) }

LispBool :: { LispVal.Abs.LispBool }
LispBool  : L_LispBool { LispVal.Abs.LispBool $1 }

LispNumber :: { LispVal.Abs.LispNumber }
LispNumber  : L_LispNumber { LispVal.Abs.LispNumber $1 }

Program :: { LispVal.Abs.Program }
Program : ListLispVal { LispVal.Abs.Prog $1 }

LispVal :: { LispVal.Abs.LispVal }
LispVal
  : Ident { LispVal.Abs.Atom $1 }
  | String { LispVal.Abs.String $1 }
  | LispNumber { LispVal.Abs.Number $1 }
  | LispBool { LispVal.Abs.Bool $1 }
  | '\'' LispVal { LispVal.Abs.Quote $2 }
  | '(' ListLispVal ')' { LispVal.Abs.List $2 }

ListLispVal :: { [LispVal.Abs.LispVal] }
ListLispVal
  : {- empty -} { [] } | LispVal ListLispVal { (:) $1 $2 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: Data.Text.Text -> [Token]
myLexer = tokens

}

