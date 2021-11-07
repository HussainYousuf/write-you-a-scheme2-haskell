{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import qualified Data.Map as M
import qualified Data.Text as T
import LispVal.Abs
import LispVal.ErrM
import LispVal.Lex
import LispVal.Par
import LispVal.Print
import LispVal.Skel
import System.Environment
import System.Exit

showTree :: (Show a, Print a) => a -> IO ()
showTree tree =
    do
        putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
        putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
    args <- getArgs
    let ts = myLexer $ head args
    case pExp ts of
        Bad s -> do
            putStrLn "\nParse Failed...\n"
            putStrLn s
            exitFailure
        Ok tree -> do
            putStrLn "\nParse Successful!"
            showTree tree
            calc tree
            exitSuccess
    putStrLn ""

calc :: Exp -> IO ()
calc e = print $ transExp e

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