

module LispVal.Abs where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype LispBool = LispBool String deriving (Eq, Ord, Show, Read)
newtype LispNumber = LispNumber String
  deriving (Eq, Ord, Show, Read)
data LispVal
    = Atom Ident
    | String String
    | Number LispNumber
    | Bool LispBool
    | Nil
    | SExp [LispVal]
    | List [LispVal]
  deriving (Eq, Ord, Show, Read)

