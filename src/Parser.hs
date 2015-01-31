module Parser ( readExpr
              , parseExpr
              ) where


import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error (throwError)
import Types

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError $ Parser err
    Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> try parseMultiChar
        <|> parseChar
        <|> parseNumber
        <|> parseQuoted
        <|> parseBackQuoted
        <|> parseAtom
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

symbol :: Parser Char
symbol = oneOf "!#$&*|+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


escapedChars :: [(String, Char)]
escapedChars = [("n"   ,'\n')
               ,("\""  ,'"' )
               ,("t"   ,'\t')
               ,("\\"  ,'\\')]

escapedChar :: Parser Char
escapedChar = choice $ map (\(x, ch) -> try $ string ("\\" ++ x) >> return ch) escapedChars

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (try escapedChar <|> noneOf "\"" )
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest  <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseMultiChar :: Parser LispVal
parseMultiChar = do
                   char '#'
                   char '\\'
                   word <- string "space" <|> string "newline"
                   case word of
                       "space"   -> return $ Character ' '
                       "newline" -> return $ Character '\n'
                       _         -> return $ Atom "Nil"

parseChar :: Parser LispVal
parseChar = do
              char '#'
              char '\\'
              ch <- letter <|> digit <|> symbol <|> space
              return $ Character ch

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
            head <- endBy parseExpr spaces
            tail <- char '.' >> spaces >> parseExpr
            return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

parseBackQuoted :: Parser LispVal
parseBackQuoted = do
                    char ','
                    x <- parseExpr
                    return $ List [Atom "unquote",x]

