{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Map.Strict hiding (map)
import Prelude hiding (lookup)
import System.Directory.Internal.Prelude hiding (lookup, CInt)

data Type = TInt
    |TBool
    |TArr Type Type
    deriving (Eq, Ord, Read, Show)

type VarId = String

data Expr =
      CInt Int
    | CBool Bool
    | Var VarId
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | ITE Expr Expr Expr
    | Abs VarId Type Expr
    | App Expr Expr
    | LetIn VarId Type Expr Expr
    deriving (Eq, Ord, Read, Show)

type Env = Map VarId Type

typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith (Just TInt) (Just TInt) = Just TInt
typingArith _ _ = Nothing

typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq (Just TInt) (Just TInt) = Just TBool
typingEq (Just TBool) (Just TBool) = Just TBool
typingEq _ _ = Nothing

typing :: Env -> Expr -> Maybe Type

typing _ (CInt v) = Just TInt

typing _ (CBool b) = Just TBool

typing e (Var x) = lookup x e

typing _ (Plus (CInt v1) (CInt v2)) = Just TInt
typing e (Plus v1 v2) = typingArith (typing e v1) (typing e v2)

typing _ (Minus (CInt v1) (CInt v2)) = Just TInt
typing e (Minus v1 v2) = typingArith (typing e v1) (typing e v2)

typing _ (Equal (CInt v1) (CInt v2)) = Just TBool
typing _ (Equal (CBool b1) (CBool b2)) = Just TBool
typing e (Equal v1 v2) = typingEq (typing e v1) (typing e v2)

typing e (ITE c t f) = case typing e c of
                        Just TBool -> let t1 = typing e t
                                          t2 = typing e f
                                        in if t1 == t2 then t1 else Nothing
                        _ -> Nothing

typing e (Abs x t exp) = case typing (insert x t e) exp of
                          Just y -> Just (TArr t y)
                          _ -> Nothing

typing e (App exp1 exp2) = case typing e exp1 of
                              Just (TArr a b) ->
                                if Just a == typing e exp2 then Just a else Nothing
                              _ -> Nothing

typing e (LetIn x t exp1 exp2) = case typing (insert x t e) exp1 of
                                  Just y ->
                                    if t == y then typing (insert x t e) exp2 else Nothing
                                  _ -> Nothing

readExpr :: String -> Expr
readExpr = read

typeCheck :: Expr -> String
typeCheck expr = maybe "Type Error" show (typing empty expr)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let strings = lines contents
  let expressions = map readExpr strings
  let result = map typeCheck expressions
  mapM_ putStrLn result


