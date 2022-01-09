{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import Control.Monad.Reader (MonadIO, MonadReader (ask, local), ReaderT)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified LispVal.Abs as A
import LispVal.ErrM
import LispVal.Par (myLexer, pProgram)

main :: IO ()
main = do
    args <- TIO.getContents
    case pProgram $ myLexer args of
        Bad s -> do
            putStrLn "\nParse Failed...\n"
            putStrLn s
        Ok tree -> do
            putStrLn "\nParse Successful!"
            putStrLn $ "\n[Abstract Syntax]\n" ++ show tree
        _ -> undefined
    putStrLn ""

-- eval :: A.LispVal -> Eval A.LispVal
-- eval (A.Quote i) = return i
-- eval (A.String i) = return $ A.String i
-- eval (A.Number i) = return $ A.Number i
-- eval (A.Bool i) = return $ A.Bool i
-- eval (A.List i) = return $ A.List i
-- eval (A.Atom (A.Ident i)) = do
--     env <- ask
--     case M.lookup i env of
--         Just x -> return x
--         _ -> return $ A.String "error"
-- eval (A.List [A.Atom (A.Ident "if"), pred, truExp, flsExp]) = do
--     x <- eval pred
--     case x of
--         (A.Bool (A.LispBool "#f")) -> eval truExp
--         (A.Bool (A.LispBool "#t")) -> eval flsExp
--         _ -> return $ A.String "error"
-- eval (A.List [A.Atom (A.Ident "let"), A.List pairs, expr]) = do
--     env <- ask
--     if all isValid pairs
--         then
--             let env' = M.fromList (map extractPair pairs) <> env
--              in local (const env') $ eval expr
--         else return $ A.String "error"
--   where
--     isValid (A.List [A.Atom (A.Ident i), _]) = True
--     isValid _ = False
--     extractPair (A.List [A.Atom (A.Ident i), val]) = (i, val)

type EnvCtx = M.Map T.Text LispVal

-- newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}
type IFunc = [LispVal] -> Eval LispVal

data LispVal
    = Atom T.Text
    | String T.Text
    | Number Integer
    | Bool Bool
    | Nil
    | List [LispVal]
    | Fun IFunc
    | Lambda IFunc EnvCtx

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal (Atom a) = a
showVal (String a) = T.pack $ show a
showVal (Number a) = T.pack $ show a
showVal (Bool a) = if a then "#t" else "#f"
showVal Nil = "Nil"
showVal (List a) = "(" <> T.unwords (map showVal a) <> ")"
showVal (Fun _) = "(internal function)"
showVal (Lambda _ _) = "(lambda function)"

newtype Eval a = Eval
    {unEval :: ReaderT EnvCtx IO a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader EnvCtx
        , MonadIO
        )

toLispVal :: A.LispVal -> LispVal
toLispVal (A.Atom (A.Ident i)) = Atom i
toLispVal (A.String i) = String $ T.pack i
toLispVal (A.Number (A.LispNumber i)) = Number $ read $ T.unpack i
toLispVal (A.Bool (A.LispBool "#t")) = Bool True
toLispVal (A.Bool (A.LispBool "#f")) = Bool False
toLispVal (A.Bool (A.LispBool _)) = error "not possible; parser error"
toLispVal (A.Quote (A.List [])) = Nil
toLispVal (A.Quote i) = List [Atom "quote", toLispVal i]
toLispVal (A.List []) = Nil
toLispVal (A.List xs) = List $ map toLispVal xs

extractVar :: LispVal -> T.Text
extractVar (Atom i) = i
extractVar i = error $ "expected identifier found: " <> show i

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", i]) = return i
eval (String i) = return $ String i
eval (Number i) = return $ Number i
eval (Bool i) = return $ Bool i
eval Nil = return Nil
eval (List []) = return Nil
eval (Atom i) = do
    env <- ask
    case M.lookup i env of
        Just x -> return x
        Nothing -> error $ "identifier not found: " <> T.unpack i
eval (List [Atom "write", rest]) = return . String . T.pack $ show rest --2 args eg (write '())
eval (List ((:) (Atom "write") rest)) = return . String . T.pack . show $ List rest --more than 2 args
eval (List [Atom "if", pred, truExp, flsExp]) = do
    pred' <- eval pred
    case pred' of
        (Bool True) -> eval truExp
        (Bool True) -> eval flsExp
        _ -> error $ "invalid predicate: " <> show pred'
eval (List [Atom "let", List pairs, expr]) = do
    env <- ask
    let env' = M.fromList (map extractPair pairs) <> env in local (const env') $ eval expr
  where
    extractPair (List [Atom i, val]) = (i, val)
    extractPair i = error $ "expected (identifier, value) found: " <> show i
    ------
eval (List [Atom "begin", rest]) = eval rest
eval (List ((:) (Atom "begin") rest)) = eval $ List rest
eval (List [Atom "define", varExpr, expr]) = do
    evalVal <- eval expr
    env <- ask
    let envFn = const $ M.insert (extractVar varExpr) evalVal env in local envFn $ return varExpr
eval (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env <- ask
    local (const $ M.insert var evalVal env) $ eval rest
eval (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    env <- ask
    let envFn = const $ M.insert var evalVal env in local envFn $ eval $ List rest
eval (List _) = return Nil
eval (Fun _) = return Nil
eval (Lambda _ _) = return Nil