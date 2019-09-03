-- an implementation of the lambda calculus using higher-order abstract syntax

module Lambda (
  Lam (Abs,App),
  freevar,
  eval,
  evals,
  value,
  closed
  ) where

-- a typical data structure for implementing lambda expressions 
-- that we won't be using because we are lazy and don't want to
-- implement substitution:

data NotLam =
    NVar String            -- variables
  | NAbs String NotLam     -- \"x". e
  | NApp NotLam NotLam     -- e1 e2

-- HOAS (Higher-Order Abstract Syntax) Lambda Calculus data type follows.  
-- Notice there is no ordinary (bound) variable clause!
-- Only a clause for free variables!
-- Invariant:  all FreeVar Strings should start with "!"
-- To ensure this, only use function freevar to create free variable terms

data Lam =            
    Abs (Lam -> Lam)  -- \x.e
  | App Lam Lam       -- e1 e2
  | FreeVar String    -- a free variable named String  
    
freevar :: String -> Lam
freevar s = FreeVar ("!" ++ s)
    
-- zero or one step evaluation --
eval :: Lam -> Lam
eval (App (Abs f) (Abs g)) = f (Abs g) 
eval (App (Abs f) e)       = App (Abs f) (eval e)
eval (App e1 e2)           = App (eval e1) e2
eval (Abs f)               = Abs f
eval (FreeVar x)           = error "Stuck!"

-- is a value? --
value (Abs f)     = True
value (App e1 e2) = False
value (FreeVar s) = False

-- multi-step evaluation --
evals :: Lam -> Lam
evals e = if value e then e else evals (eval e)

-- closed --
boundname = "bound"

closed :: Lam -> Bool
closed (Abs f)     = closed (f (FreeVar boundname))
closed (App e1 e2) = closed e1 && closed e2
closed (FreeVar s) = s == boundname

-- printing --

may :: Bool -> String -> String
may b s = if b then s else ""

left, right :: Bool -> String
left b    = may b "("
right b   = may b ")"

-- Bool is true if expression is within an argument position
-- Int is the next variable name to use
showlam :: Bool -> Int -> Lam -> String
showlam b n (FreeVar s) = s 
showlam b n (Abs f) = 
  let var = "x" ++ show n in
  "(" ++ "\\" ++ var ++ "." ++ showlam False (n+1) (f (FreeVar var)) ++ ")"
showlam b n (App e1 e2) =
  left b ++ showlam False n e1 ++ " " ++ showlam True n e2 ++ right b
    
instance Show Lam where
  show x = showlam False 1 x
