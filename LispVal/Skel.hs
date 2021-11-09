-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LispVal.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified LispVal.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: LispVal.Abs.Ident -> Result
transIdent x = case x of
  LispVal.Abs.Ident string -> failure x

transLispBool :: LispVal.Abs.LispBool -> Result
transLispBool x = case x of
  LispVal.Abs.LispBool string -> failure x

transLispNumber :: LispVal.Abs.LispNumber -> Result
transLispNumber x = case x of
  LispVal.Abs.LispNumber string -> failure x

transLispVal :: LispVal.Abs.LispVal -> Result
transLispVal x = case x of
  LispVal.Abs.Atom ident -> failure x
  LispVal.Abs.String string -> failure x
  LispVal.Abs.Number lispnumber -> failure x
  LispVal.Abs.Bool lispbool -> failure x
  LispVal.Abs.Nil -> failure x
  LispVal.Abs.SExp lispvals -> failure x
  LispVal.Abs.Quote lispval -> failure x
  LispVal.Abs.Prog lispvals -> failure x
