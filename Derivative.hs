module Derivative where

data Expr = X             -- variable
          | S Integer     -- scalar
          | Expr :+: Expr -- sum
          | Expr :*: Expr -- product
          | Expr :^: Integer  -- exponent
          deriving (Show, Eq)


derivative' :: Expr -> Expr
derivative' X = S 1
derivative' (S _) = S 0
derivative' (p :+: q) = derivative p :+: derivative q
derivative' (p :*: q) = (derivative p :*: q) :+: (p :*: derivative q)
derivative' (p :^: i) = (S i :*: (p :^: (i - 1))) :*: derivative p

simplify :: Expr -> Expr
simplify (S 0 :*: _) = S 0
simplify (_ :*: S 0) = S 0
simplify (S 1 :*: p) = p
simplify (p :*: S 1) = p
simplify (S x :*: S y) = S (x * y)
simplify (S 0 :+: p) = p
simplify (p :+: S 0) = p
simplify (S x :+: S y) = S (x + y)
simplify (p :^: 0) = S 1 
simplify (p :^: 1) = p
simplify (p :+: q) = simplify p :+: simplify q
simplify (p :*: q) = simplify p :*: simplify q
simplify (p :^: q) = simplify p :^: q
simplify p = p

derivative :: Expr -> Expr
derivative = simplify . simplify . derivative'

