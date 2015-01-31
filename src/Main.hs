{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad
import System.Environment
import Control.Monad.Error
import Parser
import Types

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

main :: IO ()
main = do
       args <- getArgs
       evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
       putStrLn $ extractValue $ trapError evaled


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val ]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
                                             result <- eval pred
                                             case result of
                                               Bool False -> eval alt
                                               Bool True  -> eval conseq
                                               otherwise  -> throwError $ TypeMismatch "boolean" pred
eval (List (Atom func   : args)) = mapM eval args >>= apply func
eval badForm                     = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs ]            = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv $ [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2)
                                                           && (all eqvPair $ zip arg1 arg2)
                                                 where eqvPair (x1, x2) = case eqv [x1,x2] of
                                                                            Left err -> False
                                                                            Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+"              , numericBinop (+) )
             , ("-"              , numericBinop (-) )
             , ("*"              , numericBinop (*) )
             , ("/"              , numericBinop div )
             , ("mod"            , numericBinop mod )
             , ("quotient"       , numericBinop quot)
             , ("remainder"      , numericBinop rem )
             , ("="              , numBoolBinop (==))
             , ("<"              , numBoolBinop (<))
             , (">"              , numBoolBinop (>))
             , ("/="             , numBoolBinop (/=))
             , (">="             , numBoolBinop (>=))
             , ("<="             , numBoolBinop (<=))
             , ("&&"             , boolBoolBinop (&&))
             , ("||"             , boolBoolBinop (||))
             , ("string=?"       , strBoolBinop (==))
             , ("string<?"       , strBoolBinop (<))
             , ("string>?"       , strBoolBinop (>))
             , ("string<=?"      , strBoolBinop (<=))
             , ("string>=?"      , strBoolBinop (>=))
             , ("car"            , car)
             , ("cdr"            , cdr)
             , ("cons"           , cons)
             , ("eq?"            , eqv)
             , ("eqv?"           , eqv)
             , ("equal?"         , equal)
             ] ++ map (\(name, func) -> (name, return . func))
             [ ("string?"        , (\(x:_) -> case x of String _ -> Bool True; _ -> Bool False))
             , ("bool?"          , (\(x:_) -> case x of Bool   _ -> Bool True; _ -> Bool False))
             , ("number?"        , (\(x:_) -> case x of Number _ -> Bool True; _ -> Bool False))
             , ("char?"          , (\(x:_) -> case x of Character _ -> Bool True; _ -> Bool False))
             , ("symbol?"        , (\(x:_) -> case x of Atom   _ -> Bool True; _ -> Bool False))
             , ("pair?"          , (\(x:_) -> case x of List   _ -> Bool True; DottedList _ _ -> Bool True; _ -> Bool False))
             , ("symbol->string" , (\(x:_) -> case x of Atom   x -> String x))
             , ("string->symbol" , (\(x:_) -> case x of String x -> Atom x))
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notStr     = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
          do unpacked1 <- unpacker arg1
             unpacked2 <- unpacker arg2
             return $ unpacked1 == unpacked2
        `catchError` (const $ return False)
