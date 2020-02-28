--Project
module Project where

import Prelude hiding (not, and, or, div)

-- * Syntax *

--Abstract Syntax
type Var = String
type Funct = String

data Expr = LitN Int
          | LitB Bool
          | LitS String
          | Ref Var
          | Add Expr Expr
          | Mult Expr Expr
          | GtrThn Expr Expr
          | Concat Expr Expr
          | If Expr Expr Expr
          | Call Funct [Expr]
  deriving(Eq, Show)

data Stmt = Set Var Expr
          | While Expr Stmt
          | Begin [Stmt]
          | Define Funct [Var] Stmt Expr
  deriving(Eq, Show)

--Syntactic Sugar
neg :: Expr -> Expr
neg e = Mult (LitN (-1)) e

sub :: Expr -> Expr -> Expr
sub e f = Add e (neg f)

div :: Expr -> Expr -> Expr
div e f = If (gtrthneq e f) (Add (LitN 1) (div (sub e f) f)) (LitN 0)

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
modulus :: Stmt
modulus = Define "modulus" ["x","m"] (While (gtrthneq (Ref "x") (Ref "m")) (Set "x" (sub (Ref "x") (Ref "m")))) (Ref "x")

expnts :: Stmt
expnts = Define "expnts" ["x","e"] (Begin [Set "result" (LitN 1), While (GtrThn (Ref "e") (LitN 0)) (Begin [Set "result" (Mult (Ref "result") (Ref "x")), Set "e" (sub (Ref "e") (LitN 1))])]) (Ref "result")

-- * Example Programs *
p1 :: Stmt
p1 = Begin [modulus,
            (Set "x" (Add (LitN 5) (LitN 6))),
            (Set "r" (Add (LitN 23) (LitN 8))),
            (Set "z" (Add (LitN 2) (LitN 4))),
            (Set "y" (Call "modulus" [(Ref "r"), (LitN 7)]))]

p2 :: Stmt
p2 = Begin [Set "x" (LitN 100),
            Set "x" (div (Ref "x") (LitN 6))]

p3 :: Stmt
p3 = Begin [expnts, Set "x" (LitN 5), Set "z" (Call "expnts" [Ref "x", LitN 3])]

--Infinite Loop
p4 :: Stmt
p4 = Begin [Set "x" (LitN 5),
            While (GtrThn (LitN 5) (LitN 3)) (Begin [Set "x" (Add (Ref "x") (LitN 1))])]

-- program to increment and find first integer ("d") that 511 divides by with zero remainder ("r")
p5 :: Stmt
p5 = Begin [modulus,
            Set "d" (LitN 2),
            Set "r" (LitN 1),
            While (GtrThn (Ref "r") (LitN 0))
              (Begin [Set "r" (Call "modulus" [(LitN 511), (Ref "d")]),
                      Set "d" (Add (Ref "d") (LitN 1))])]

-- program with type error between String and Bool
p6 :: Stmt
p6 = Begin [Set "s1" (LitS "Hello"),
            Set "x" (lsthn (LitN 4) (LitN 5)),
            Set "s2" (Concat (Ref "s1") (Ref "x"))]

fib :: Stmt
fib = Begin [Set "n" (LitN 3),
             Set "t1" (LitN 0),
             Set "t2" (LitN 1),
             Set "nextTerm" (LitN 0),
             While (lsthneq (Ref "nextTerm") (Ref "n"))
                    (Begin
                          [Set "nextTerm" (Add (Ref "t1") (Ref "t2")),
                           Set "t1" (Add (Ref "t1") (Ref "t2")),
                           Set "t2" (Add (Ref "t2") (Ref "nextTerm"))]
            )]

-- * Semantics *
data Value = I Int
           | B Bool
           | S String
           | F [Var] Stmt Expr
           | Error
  deriving(Eq, Show)

type Element = (Either Var Funct, Value)

type State = [Element]

--State Helper Functions
getvar :: Var -> State -> Value
getvar x [] = Error
getvar x ((vf, val):es) = case vf of
                             Left var -> if x == var then val else getvar x es
                             _ -> getvar x es

setvar :: Var -> Value -> State -> State
setvar x v s = (Left x, v) : s

getfunct :: Funct -> State -> Value
getfunct f [] = Error
getfunct f ((vf, val):es) = case vf of
                             Right funct -> if f == funct then val else getfunct f es
                             _ -> getfunct f es

setfunct :: Funct -> Value -> State -> State
setfunct f v s = (Right f, v) : s

createfunctstate :: [Var] -> [Expr] -> State -> State -> State
createfunctstate [] [] s fs = fs
createfunctstate (v:vs) (e:es) s fs = createfunctstate vs es s (setvar v (expr e s) fs)

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
expr (Concat e f) s = case (expr e s, expr f s) of
                       (S i, S j) -> S (i ++ j)
                       _ -> Error
expr (If c t e) s = case expr c s of
                       B True -> expr t s
                       B False -> expr e s
                       _ -> Error
expr (Call f exps) s = case getfunct f s of
                          (F vs st re) -> expr re (stmt st (createfunctstate vs exps s []))
                          _ -> Error

stmt :: Stmt -> State -> State
stmt (Set x v) s = setvar x (expr v s) s
stmt (While e st) s = case expr e s of
                         B True -> stmt (While e st) (stmt st s)
                         _ -> s
stmt (Begin st) s = case st of
                       [] -> s
                       (stm:ss) -> stmt (Begin ss) (stmt stm s)
stmt (Define f vs st re) s = setfunct f (F vs st re) s
