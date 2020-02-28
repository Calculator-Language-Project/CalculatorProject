--Calculex
module Project where

import Prelude hiding (not, and, or, div, mod)
import Data.Maybe

-- * Syntax *

--Abstract Syntax
type Var = String
type Funct = String
type List = String

data Expr = LitN Int
          | LitB Bool
          | LitS String
          | Ref Var
          | Add Expr Expr
          | Mult Expr Expr
          | GtrThn Expr Expr
          | ConcatStr Expr Expr
          | If Expr Expr Expr
          | Call Funct [Expr]
          | Index List Int
  deriving(Eq, Show)

data Stmt = Set Var Expr
          | While Expr Stmt
          | Begin [Stmt]
          | Define Funct [Var] Stmt Expr
          | Create List [Expr]
          | ConcatLsts List List List
  deriving(Eq, Show)

--Syntactic Sugar
neg :: Expr -> Expr
neg e = Mult (LitN (-1)) e

absval :: Expr -> Expr
absval e = If (lsthn e (LitN 0)) (Mult e (LitN (-1))) e

sub :: Expr -> Expr -> Expr
sub e f = Add e (neg f)

div :: Expr -> Expr -> Expr
div e f = If (not (eq f (LitN 0))) (If (and (gtrthneq e (LitN 0)) (gtrthneq f (LitN 0))) (If (gtrthneq e f) (Add (LitN 1) (div (sub e f) f)) (LitN 0)) (If (and (lsthn e (LitN 0)) (lsthn f (LitN 0))) (If (gtrthneq (absval e) (absval f)) (Add (LitN 1) (div (sub e f) f)) (LitN 0)) (Mult (LitN (-1)) (If (gtrthneq (absval e) (absval f)) (Add (LitN 1) (div (sub (absval e) (absval f)) (absval f))) (LitN 0))))) (Add (LitN 1) (LitB True))

not :: Expr -> Expr
not e = If e (LitB False) (LitB True)

and :: Expr -> Expr -> Expr
and e f = If e f (LitB False)

or :: Expr -> Expr -> Expr
or e f = If e (LitB True) f

lsthn :: Expr -> Expr -> Expr
lsthn e f = not (gtrthneq e f)

gtrthneq :: Expr -> Expr -> Expr
gtrthneq e f = GtrThn e (sub f (LitN 1))

lsthneq :: Expr -> Expr -> Expr
lsthneq e f = lsthn e (Add f (LitN 1))

eq :: Expr -> Expr -> Expr
eq e f = and (not (GtrThn e f)) (not (lsthn e f))

--Library
mod :: Stmt
mod = Define "mod" ["x","m"] (While (gtrthneq (Ref "x") (Ref "m")) (Set "x" (sub (Ref "x") (Ref "m")))) (Ref "x")

expnts :: Stmt
expnts = Define "expnts" ["x","e"] (Begin [Set "result" (LitN 1), While (GtrThn (Ref "e") (LitN 0)) (Begin [Set "result" (Mult (Ref "result") (Ref "x")), Set "e" (sub (Ref "e") (LitN 1))])]) (Ref "result")

-- * Example Programs *
p1 :: Stmt
p1 = Begin [mod,
            (Set "x" (Add (LitN 5) (LitN 6))),
            (Set "r" (Add (LitN 23) (LitN 8))),
            (Set "z" (Add (LitN 2) (LitN 4))),
            (Set "y" (Call "mod" [(Ref "r"), (LitN 7)]))]

p2 :: Stmt
p2 = Begin [Set "x" (LitN 100),
            Set "x" (div (Ref "x") (LitN (-10)))]

p3 :: Stmt
p3 = Begin [expnts, Set "x" (LitN 5), Set "z" (Call "expnts" [Ref "x", LitN 3])]

p7 :: Stmt
p7 = Begin [mod, expnts, Create "list" [Add (LitN 9) (LitB True),LitN 2,LitN 6,LitN 5,LitN 0,LitN 84], Create "list2" [LitN 15,LitN 76,LitN 34,LitN 16,LitN 91], ConcatLsts "newlist" "list" "list2", Set "x" (Index "newlist" 7)]

--Infinite Loop
p4 :: Stmt
p4 = Begin [Set "x" (LitN 5),
            While (GtrThn (LitN 5) (LitN 3)) (Begin [Set "x" (Add (Ref "x") (LitN 1))])]

-- program to increment and find first integer ("d") that 511 divides by with zero remainder ("r")
p5 :: Stmt
p5 = Begin [mod,
            Set "d" (LitN 2),
            Set "r" (LitN 1),
            While (GtrThn (Ref "r") (LitN 0))
              (Begin [Set "r" (Call "modulus" [(LitN 511), (Ref "d")]),
                      Set "d" (Add (Ref "d") (LitN 1))])]

-- program with type error between String and Bool
p6 :: Stmt
p6 = Begin [Set "s1" (LitS "Hello"),
            Set "x" (lsthn (LitN 4) (LitN 5)),
            Set "s2" (ConcatStr (Ref "s1") (Ref "x"))]

-- * Semantics *
data Value = I Int
           | B Bool
           | S String
           | L [Value]
           | F [Var] Stmt Expr
           | Error
  deriving(Eq, Show)

data Name = Va Var
          | Fu Funct
          | Li List
  deriving(Eq, Show)

type Element = (Name, Value)

type State = [Element]

--State Helper Functions
getvar :: Var -> State -> Value
getvar x [] = Error
getvar x ((n, val):es) = case n of
                             Va var -> if x == var then val else getvar x es
                             _ -> getvar x es

setvar :: Var -> Value -> State -> State
setvar x v s = (Va x, v) : s

getfunct :: Funct -> State -> Value
getfunct f [] = Error
getfunct f ((n, val):es) = case n of
                             Fu funct -> if f == funct then val else getfunct f es
                             _ -> getfunct f es

setfunct :: Funct -> Value -> State -> State
setfunct f v s = (Fu f, v) : s

createfunctstate :: [Var] -> [Expr] -> State -> State -> State
createfunctstate [] [] s fs = fs
createfunctstate (v:vs) (e:es) s fs = createfunctstate vs es s (setvar v (expr e s) fs)
createfunctstate _ _ s fs = fs

getlist :: List -> State -> Value
getlist l [] = Error
getlist l ((n, val):es) = case n of
                             Li list -> if l == list then val else getlist l es
                             _ -> getlist l es

setlist :: List -> Value -> State -> State
setlist l v s = (Li l, v) : s

listexpstovals :: [Expr] -> State -> [Value]
listexpstovals [] s = []
listexpstovals (e:es) s = case expr e s of
                             I i -> (I i):(listexpstovals es s)
                             _ -> [Error]

listexpstoval :: [Expr] -> State -> Value
listexpstoval es s = case listexpstovals es s of
                        (Error:[]) -> Error
                        v -> L v

--Semantic Functions
expr :: Expr -> State -> Value
expr (LitN i) s = I i
expr (LitB b) s = B b
expr (LitS st) s = S st
expr (Ref x) s = getvar x s
expr (Add e f) s = case (expr e s, expr f s) of
                      (I i, I j) -> I (i + j)
                      _ -> Error
expr (Mult e f) s = case (expr e s, expr f s) of
                       (I i, I j) -> I (i * j)
                       _ -> Error
expr (GtrThn e f) s = case (expr e s, expr f s) of
                       (I i, I j) -> B (i > j)
                       _ -> Error
expr (ConcatStr e f) s = case (expr e s, expr f s) of
                       (S i, S j) -> S (i ++ j)
                       _ -> Error
expr (If c t e) s = case expr c s of
                       B True -> expr t s
                       B False -> expr e s
                       _ -> Error
expr (Call f exps) s = case getfunct f s of
                          (F vs st re) -> case stmt st (createfunctstate vs exps s []) of
                                             Just s -> expr re s
                                             Nothing -> Error
                          _ -> Error
expr (Index l i) s = case getlist l s of
                        L is -> is!!i
                        _ -> Error

stmt :: Stmt -> State -> Maybe State
stmt (Set x v) s = case expr v s of
                      Error -> Nothing
                      _ -> Just (setvar x (expr v s) s)
stmt (While e st) s = case expr e s of
                         B True -> case stmt st s of
                                      Just s -> stmt (While e st) s
                                      Nothing -> Nothing
                         B False -> Just s
                         _ -> Nothing
stmt (Begin st) s = case st of
                       [] -> Just s
                       (stm:ss) -> case stmt stm s of
                                      Just s -> stmt (Begin ss) s
                                      Nothing -> Nothing
stmt (Define f vs st re) s = Just (setfunct f (F vs st re) s)
stmt (Create l exps) s = case listexpstoval exps s of
                            (L is) -> Just (setlist l (L is) s)
                            _ -> Nothing
stmt (ConcatLsts nl l1 l2) s = case (getlist l1 s, getlist l2 s) of
                                  (L fl, L sl) -> Just (setlist nl (L (fl ++ sl)) s)
                                  _ -> Nothing

eval :: Stmt -> Maybe State
eval p = stmt p []
