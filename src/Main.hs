{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad
import Env
import Eval
import Parser
import Primitives
import System.Environment
import System.IO
import Types

main :: IO ()
main = do
       args <- getArgs
       if null args then runRepl else runOne args

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (liftM return $ putStr "Lisp>>> " >> hFlush stdout >> getLine) . evalAndPrint


runOne :: [String] -> IO ()
runOne args = do
        env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
        runIOThrows (liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m (Maybe t) -> (t -> m a) -> m ()
until_ pred prompt action = do
        result <- prompt
        case result of
             Nothing      -> return ()
             Just result' -> unless (pred result') $
                                      action result' >> until_ pred prompt action

