
-- Prelude, System.IO, System.Environment, Data.Map.Strict.
import System.IO
import System.Directory.Internal.Prelude

data BExpr
  = Var String
  | Not BExpr
  | And BExpr BExpr
  | Imply BExpr BExpr
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let strings = lines contents
  let expressions = map read strings :: [BExpr]
  let result = map satisfiable expressions
  let end = map boolSAT result
  print end

satisfiable :: BExpr -> Bool
satisfiable expr = any (replaceAndResolve expr) possibilities
  where
    variables = extractVariables expr
    possibilities = generatePossibilities variables

imply :: Bool -> Bool -> Bool
imply True False = False
imply _ _ = True

replaceAndResolve :: BExpr -> [(String, Bool)] -> Bool
replaceAndResolve expr state = go expr
  where
    go (Var v) = getValue $ lookup v state
    go (Not e) = not (go e)
    go (And a b) = go a && go b
    go (Imply a b) = imply (go a) (go b)

generatePossibilities :: [String] -> [[(String, Bool)]]
generatePossibilities = traverse (\var -> [(var,True), (var, False)])

extractVariables :: BExpr -> [String]
extractVariables expr = unique (extract expr) where
    extract (Var v) = [v]
    extract (Not e) = extract e
    extract (And a b) = extract a ++ extract b
    extract (Imply a b) = extract a ++ extract b

unique :: (Ord a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

getValue :: Maybe a -> a
getValue Nothing = error "Has Nothing"
getValue (Just x) = x

boolSAT :: Bool -> String
boolSAT True = "SAT"
boolSAT False = "UNSAT" 
   


