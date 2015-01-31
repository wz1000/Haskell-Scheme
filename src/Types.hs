module Types where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Number     Integer
             | Character  Char
             | String     String
             | Bool       Bool -- deriving (Show)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError


instance Show LispVal where show = showVal

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

showVal :: LispVal -> String
showVal (String     s     ) = "\"" ++ s ++ "\""
showVal (Character  ch    ) = "#\\" ++ (show ch)
showVal (Atom       a     ) = a
showVal (Number     n     ) = show n
showVal (Bool       True  ) = "#t"
showVal (Bool       False ) = "#f"
showVal (List       l     ) = "(" ++ unwordsList l                   ++ ")"
showVal (DottedList h t   ) = "(" ++ unwordsList h ++ "." ++ showVal t ++ ")"

showError :: LispError -> String
showError (UnboundVar     message  varname ) = message ++ ": " ++ varname
showError (BadSpecialForm message  form    ) = message ++ ": " ++ show form
showError (NotFunction    message  func    ) = message ++ ": " ++ show func
showError (Parser         parseErr         ) = "Parse error at " ++ show parseErr
showError (NumArgs        expected found   ) = "Expected " ++ show expected
                                             ++ " args; found values " ++ unwordsList found
showError (TypeMismatch   expected found   ) = "Invalid type: expected " ++ expected
                                             ++ ", found " ++ show found

unwordsList :: [LispVal]  -> String
unwordsList = unwords . map showVal
