import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import System.Directory.Internal.Prelude hiding (CInt)

type VarId = String

data Type = TInt
    | TBool
    | TError
    | TVar Int
    | TArr Type Type
    deriving (Eq, Ord, Read, Show)

data Constraint = CEq Type Type
    | CError
    deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

type Env = Map.Map VarId Type

type InferState a = State Int a

data Expr = CInt Int
    | CBool Bool
    | Var VarId
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | ITE Expr Expr Expr
    | Abs VarId Expr
    | App Expr Expr
    | LetIn VarId Expr Expr
    deriving (Eq, Ord, Read, Show)

getFreshTVar :: InferState Type
getFreshTVar = do n <- get
                  put (n + 1)
                  return (TVar n)

infer :: Env -> Expr -> InferState (Type, ConstraintSet)

infer g (CInt _) = return (TInt, Set.empty)
infer g (CBool _) = return (TBool, Set.empty)

infer g (Var x) = case Map.lookup x g of
                      Just t -> return (t, Set.empty)
                      _ -> return (TError, Set.insert CError Set.empty)

infer g (Plus exp1 exp2) = do
                            (t1, c1) <- infer g exp1
                            (t2, c2) <- infer g exp2
                            return (TInt, Set.union
                                   (Set.fromList [CEq t1 TInt, CEq t2 TInt])
                                   (Set.union c1 c2))

infer g (Minus exp1 exp2) = do
                            (t1, c1) <- infer g exp1
                            (t2, c2) <- infer g exp2
                            return (TInt, Set.union
                                   (Set.fromList [CEq t1 TInt, CEq t2 TInt])
                                   (Set.union c1 c2))

infer g (Equal exp1 exp2) = do  (t1, c1) <- infer g exp1
                                (t2, c2) <- infer g exp2
                                return (TBool, Set.union
                                       (Set.fromList[CEq t1 t2])
                                       (Set.union c1 c2))

infer g (ITE c exp1 exp2) = do
                            (t1, c1) <- infer g c
                            (t2, c2) <- infer g exp1
                            (t3, c3) <- infer g exp2
                            return (t2, Set.union
                                   (Set.fromList[CEq t1 TBool, CEq t2 t3])
                                   (Set.union (Set.union c1 c2) c3))

infer g (Abs x e) = do y <- getFreshTVar
                       (t, c) <- infer (Map.insert x y g) e
                       return (TArr y t, c)

infer g (App exp1 exp2) = do
                            x1 <- getFreshTVar
                            x2 <- getFreshTVar
                            (t1, c1) <- infer g exp1
                            (t2, c2) <- infer g exp2
                            return (x2, Set.union
                                   (Set.fromList[CEq t1 (TArr x1 x2), CEq t2 x1])
                                   (Set.union c1 c2))

infer g (LetIn v exp1 exp2) = do
                                x <- getFreshTVar
                                (t1, c1) <- infer (Map.insert v x g) exp1
                                (t2, c2) <- infer (Map.insert v x g) exp2
                                return (t2, Set.union
                                       (Set.fromList[CEq x t1])
                                       (Set.union c1 c2))

type Substitution = Map.Map Type Type

inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr expr  = evalState (infer mempty expr) 0

toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList

--snd to convert (a, b) -> b

applySub :: Substitution -> Type -> Type
applySub subs TInt = TInt
applySub subs TBool = TBool
applySub subs (TVar x) = case Map.lookup (TVar x) subs of
                         Just t -> t
                         _ -> TVar x
applySub subs (TArr t1 t2) = TArr (applySub subs t1) (applySub subs t2)
applySub subs _ = TError



modifyTVar :: Substitution -> Constraint -> Constraint
modifyTVar subs (CEq t1 t2) = CEq (applySub subs t1) (applySub subs t2)
modifyTVar subs CError = CError


applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList subs = map (modifyTVar subs)

composeSub :: Substitution -> Substitution -> Substitution
composeSub sub1 sub2 =  Map.union
                       (Map.map (applySub sub1) sub2)
                       (Map.withoutKeys sub1  (Map.keysSet sub2))

tvars :: Type -> Set.Set Type
tvars (TVar x) = Set.singleton (TVar x)
tvars (TArr a b) = Set.union (tvars a) (tvars b)
tvars _ = Set.empty

unify :: ConstraintList -> Maybe Substitution
unify [] = Just Map.empty
unify ((CEq t1 t2) : xs)
    | t1 == t2 = unify xs
    | checkIfVar t1 && Set.notMember t1 (tvars t2) = do
                                            let subst = Map.singleton t1 t2
                                            c <- unify (applySubToCstrList subst xs)
                                            return (composeSub c subst)
    |checkIfVar t2 && Set.notMember t2 (tvars t1) = do
                                            let subst = Map.singleton t2 t1
                                            c <- unify (applySubToCstrList subst xs)
                                            return (composeSub c subst)
    |otherwise = case (t1, t2) of
                 (TArr a b, TArr c d) -> unify (Set.toList (Set.fromList (CEq a c : CEq b d : xs)))
                 _ -> Nothing
unify _ = Nothing

checkIfVar :: Type -> Bool
checkIfVar (TVar _) = True
checkIfVar _ = False


typing :: Expr -> Maybe Type
typing expr = do
                 let (t, cset) = inferExpr expr
                 u <- unify (Set.toList cset)
                 return (relabel (applySub u t))

typeInfer :: Expr -> String
typeInfer expr = maybe "Type Error" show (typing expr)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let strings = lines contents
    let expressions = map readExpr strings
    let result = map typeInfer expressions
    mapM_ putStrLn result

readExpr :: String -> Expr
readExpr = read

type RelabelState a = State (Map.Map Int Int) a

relabel :: Type -> Type
relabel t = evalState (go t) Map.empty
    where
        go :: Type -> RelabelState Type
        go TInt = return TInt
        go TBool = return TBool
        go TError = return TError
        go (TVar x) = do m <- get
                         case Map.lookup x m of
                            Just v -> return (TVar v)
                            Nothing -> do let n = 1 + Map.size m
                                          put (Map.insert x n m)
                                          return (TVar n)
        go (TArr t1 t2) = do t1' <- go t1
                             t2' <- go t2
                             return (TArr t1' t2')