module SimpleParser
    (
      readExpr
    , eval
    ) where
import Text.ParserCombinators.Parsec
import System.Environment

import Control.Monad
import qualified Control.Exception as CE

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> SimpleLispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val


spacesSimple :: Parser ()
spacesSimple  = skipMany1 space

data SimpleLispVal = Atom String
                   | List [SimpleLispVal]
                   | DottedList [SimpleLispVal] SimpleLispVal
                   | Number Integer
                   | String String
                   | Bool Bool


showVal :: SimpleLispVal -> String
showVal (String contents) = "\"" ++ contents ++"\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "("++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "("++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [SimpleLispVal] -> String
unwordsList  = unwords . map showVal

instance Show SimpleLispVal
    where show = showVal



parseString :: Parser SimpleLispVal
parseString = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                return $ String x


parseAtom :: Parser SimpleLispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser SimpleLispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser SimpleLispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


parseList :: Parser SimpleLispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser SimpleLispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser SimpleLispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

eval :: SimpleLispVal -> SimpleLispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [SimpleLispVal] -> SimpleLispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String,[SimpleLispVal]-> SimpleLispVal)]
primitives = [
            ("+", numericBinop (+)),
            ("-", numericBinop (-)),
            ("*", numericBinop (*)),
            ("/", numericBinop div),
            ("mod", numericBinop mod),
            ("quotient", numericBinop quot),
            ("remaineder", numericBinop rem)
            ]

numericBinop :: (Integer -> Integer -> Integer) -> [SimpleLispVal] -> SimpleLispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
    where unpackNum :: SimpleLispVal -> Integer
          unpackNum (Number n) = n
          unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
              if null parsed then 0 else fst . head $ parsed
          unpackNum (List [n]) = unpackNum n
          unpackNum _ = 0

data LispError = NumArgs Integer [SimpleLispVal]
               | TypeMismatch String SimpleLispVal
               | Parser ParseError
               | BadSpecialForm String SimpleLispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ " : " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Excepted " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Inavlid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)  = "Parse error at " ++ show parseErr

instance Show LispError
    where show = showError

instance CE.Exception LispError

type ThrowsError = Either LispError
