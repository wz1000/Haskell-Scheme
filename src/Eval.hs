module Eval ( eval
            , apply
            , applyProc
            ) where

import Control.Monad
import Control.Monad.Except
import Data.Maybe (isNothing)

import Types
import Env
import Parser

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool   _) = return val
eval env (Atom  "else") = return $ Bool True
eval env (Atom id     ) = getVar env id
-- Special Forms --
eval env (List [Atom "quote", val ]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
     result <- eval env pred
     case result of
       Bool True  -> eval env conseq
       Bool False -> eval env alt
       _          -> throwError $ TypeMismatch "boolean" pred
eval env (List (Atom "cond" : conds)) = case conds of
     (x:xs) -> case x of
                 (List (pred:conseqs)) -> do 
                    result <- eval env pred
                    case result of
                      Bool True ->  do 
                        results <- mapM (eval env) conseqs
                        case results of
                          [] -> throwError $ 
                                  BadSpecialForm "Need at least one expression to evalute cond" x
                          _  -> return $ last results
                      Bool False -> eval env (List (Atom "cond" : xs))
                      _          -> throwError $ TypeMismatch "boolean" pred
                 _                     -> throwError $ TypeMismatch "non-empty list" x
     []     -> return $ List []
eval env (List [Atom "set!"   , Atom var, form]) =
        eval env form >>= setVar env var
eval env (List (Atom "define" : List       (Atom var : params)         : body)) =
        makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
        makeVarArgs varargs env params body >>= defineVar env var
eval env (List [Atom "define" , Atom var    , form]) =
        eval env form >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
        makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
        makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
        makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
        load filename >>= liftM last . mapM (eval env)
eval env (List (function : args))    = do
        func <- eval env function
        if isMacro function
           then apply env func args
           else do argVals <- mapM (eval env) args
                   apply env func argVals
eval env badForm                     = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Monad m => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: Monad m => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeFunc . Just . showVal

apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _   (PrimitiveFunc func) args = liftThrows $ func args
apply _   (IOFunc        func) args = func args
apply _   (Macro         func) args = func args
apply env (EnvFunc       func) args = func env args
apply _   (Func params varargs body closure) args =
        if num params /= num args && isNothing varargs
           then throwError $ NumArgs (num params) args
           else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
        where remainingArgs = drop (length params) args
              num = toInteger . length
              evalBody env = liftM last $ mapM (eval env) body
              bindVarArgs arg env = case arg of
                                         Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                                         Nothing      -> return env

applyProc :: Env -> [LispVal] -> IOThrowsError LispVal
applyProc env [func, List args] = apply env func args
applyProc env (func : args    ) = apply env func args

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList
