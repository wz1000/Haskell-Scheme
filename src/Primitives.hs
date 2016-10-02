module Primitives ( primitiveBindings ) where 

import Control.Monad.Except
import System.IO
import Types
import Env
import Parser
import Eval

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                             ++ map (makeFunc IOFunc       ) ioPrimitives
                                             ++ map (makeFunc EnvFunc      ) envPrimitives)
                           where makeFunc constructor (var, func) = (var, constructor func)

envPrimitives :: [(String, Env -> [LispVal] -> IOThrowsError LispVal)]
envPrimitives = [ ("eval", eval')
                , ("unquote", eval')
                , ("apply", applyProc)
                ]
                  where eval' env [x]    = eval env x
                        eval' env (x:xs) = eval env x >> eval' env xs

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("open-input-file"   , makePort ReadMode     )
               , ("open-output-file"  , makePort WriteMode    )
               , ("open-file"         , makePort ReadWriteMode)
               , ("open-file-append"  , makePort AppendMode   )
               , ("close-port"        , closePort             )
               , ("read"              , readProc              )
               , ("write"             , writeProc             )
               , ("read-contents"     , readContents          )
               , ("read-all"          , readAll               )
               ]

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


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]           = return $ List xs
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
eqv = fix eqvGen

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqvGen equal [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqvGen :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvGen _ [ Bool arg1,     Bool   arg2 ]         = return $ Bool $ arg1 == arg2
eqvGen _ [ Number arg1 ,  Number arg2 ]         = return $ Bool $ arg1 == arg2
eqvGen _ [ String arg1 ,  String arg2 ]         = return $ Bool $ arg1 == arg2
eqvGen _ [ Atom arg1   ,  Atom   arg2 ]         = return $ Bool $ arg1 == arg2
eqvGen f [ DottedList xs x , DottedList ys y]   = f $ [List $ xs ++ [x], List $ ys ++ [y]]
eqvGen f [ List arg1, List arg2]                = return $ Bool $ (length arg1 == length arg2)
                                                           && (all eqvPair $ zip arg1 arg2)
                                                     where eqvPair (x1, x2) = case eqvGen f [x1,x2] of
                                                                                Left err         -> False
                                                                                Right (Bool val) -> val
eqvGen _ [_, _]                                 = return $ Bool False
eqvGen _ badArgList                             = throwError $ NumArgs 2 badArgList



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
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
