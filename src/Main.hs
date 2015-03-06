{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad

import System.IO
import System.Environment
import System.Console.Readline

import Parser
import Types
import Primitives
import Eval
import Env

main :: IO ()
main = do
       args <- getArgs
       if null args then runRepl else runOne args

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readline "Lisp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
        env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
        (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ pred prompt action = do
        result <- prompt
        case result of
             Just result' -> if pred result'
                                then return ()
                                else addHistory result' >> action result' >> until_ pred prompt action
             Nothing -> return ()

