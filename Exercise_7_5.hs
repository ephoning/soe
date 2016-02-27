{-# LANGUAGE UnicodeSyntax #-}

module Exercise_7_5 where

import Prelude.Unicode

data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr |
  Let String Expr Expr | V String

type VarStore = [(String,Expr)]

evaluate ∷ Expr → Float
evaluate e = evaluate' e []

evaluate' ∷ Expr → VarStore → Float
evaluate' (C x) _ = x
evaluate' (e1 :+ e2) vs = evaluate' e1 vs + evaluate' e2 vs
evaluate' (e1 :- e2) vs = evaluate' e1 vs - evaluate' e2 vs
evaluate' (e1 :* e2) vs = evaluate' e1 vs * evaluate' e2 vs
evaluate' (e1 :/ e2) vs = evaluate' e1 vs / evaluate' e2 vs
evaluate' (Let n e1 e2) vs = evaluate' e2 (storeVar n e1 vs)
evaluate' (V n) vs = evaluate' (lookupVar n vs) vs

storeVar ∷ String → Expr → VarStore → VarStore
storeVar n e vs = (n,e):vs

lookupVar ∷ String → VarStore → Expr
lookupVar n [] = error ("variable '" ++ n ++ "' not found")
lookupVar n ((n',e):vs) = if n ≡ n'
                         then e
                         else lookupVar n vs




  

