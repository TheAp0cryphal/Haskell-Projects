
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}









--  Prelude, Data.Char, System.IO, System.Environment, Control.Applicative


import System.IO
import System.Directory.Internal.Prelude
import Control.Applicative hiding (Const)
import Data.Char

data Prop
    = Const Bool
    | Var String
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Show, Read)

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P (\input -> case input of
             [] -> []
             (x:xs) -> [(x, xs)])

instance Functor Parser where
  fmap f p = P (\input -> case parse p input of
                        [] -> []
                        [(v, out)] -> [(f v, out)])


instance Applicative Parser where
  pure v = P (\input -> [(v, input)])
  pf <*> px = P (\input -> case parse pf input of
                      [] -> []
                      [(f, out)] -> parse (fmap f px) out)

instance Monad Parser where
  p >>= f = P (\input -> case parse p input of
                  [] -> []
                  [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\input -> case parse p input of
                  [] -> parse q input
                  [(v,out)] -> [(v,out)])


main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let result = map parseFormula ls
  mapM_ putStrLn result



constant :: Parser Prop
constant = constantT <|> constantF
constantT :: Parser Prop
constantT = symbol "T" >> pure (Const True)
constantF :: Parser Prop
constantF = symbol "F" >> pure (Const False)


parseFormula :: String -> String
parseFormula s = case parse formula s of
                          [(v, "")] -> show v
                          _ -> "Parse Error"

var :: Parser Prop
var = fmap Var identifier

formula :: Parser Prop
formula = do
            i <- imply
            symbol "<->"
            Iff i <$> formula
          <|> imply

imply :: Parser Prop
imply = do
          o <- orTerm
          symbol "->"
          Imply o <$> imply
        <|> orTerm

orTerm :: Parser Prop
orTerm = do
           a <- andTerm
           symbol "\\/"
           Or a <$> orTerm
         <|> andTerm

andTerm :: Parser Prop
andTerm = do
            n <- notTerm
            symbol "/\\"
            And n <$> andTerm
          <|> notTerm

notTerm :: Parser Prop
notTerm = do
            symbol "!"
            Not <$> notTerm
          <|> factor

factor :: Parser Prop
factor = do symbol "("
            f <- formula
            symbol ")"
            return f
          <|> constant
          <|> var

sat :: (Char -> Bool) -> Parser Char
sat p =  do x <- item
            if p x then return x else empty

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol ::  String -> Parser String
symbol xs = token (string xs)

ident :: Parser String
ident = do
          x <- lower
          xs <- many alphanum
          return (x:xs)

identifier :: Parser String
identifier = token ident