module September18 where

data Nat = Zero | Succ Nat deriving (Show, Eq)

data Lambda = Var Nat | App Lambda Lambda | Lam Lambda deriving (Show, Eq)

subst :: (Nat -> Lambda) -> Lambda -> Lambda
subst f (Var i) = f i
subst f (App t u) = App (subst f t) (subst f u)
subst f (Lam t) = Lam (subst g t)
  where g :: Nat -> Lambda
        g Zero = Var Zero
        g (Succ i) = rename Succ (f i)

        rename :: (Nat -> Nat) -> Lambda -> Lambda
        rename f (Var i) = Var (f i)
        rename f (App m n) = App (rename f m) (rename f n)
        rename f (Lam m) = Lam (rename f' m)
          where f' :: Nat -> Nat
                f' Zero = Zero
                f' (Succ i) = Succ (f i)


subst0 :: Lambda -> Lambda -> Lambda
subst0 t = subst f
  where f :: Nat -> Lambda
        f Zero = t
        f (Succ i) = Var i


betaNormal :: Lambda -> Lambda
betaNormal v@(Var _) = v
betaNormal (App t u) =
  case betaNormal t of
    Lam t' -> betaNormal (subst0 (betaNormal u) t')
    t' -> App t' (betaNormal u)
betaNormal (Lam t) = Lam (betaNormal t)


-- >>> betaNormal (App (Lam (Lam (Var (Succ Zero)))) (Var (Succ Zero)))
-- Lam (Var (Succ (Succ Zero)))


freshVars :: [String]
freshVars = concat (map (\s -> map (\c -> c:s) "xyzuvw") appendMe)
  where ints = 1 : map (\x -> x + 1) ints :: [Integer]
        appendMe = "" : map show ints


printLambda' :: (Nat -> String) -> [String] -> Lambda -> String
printLambda' f fvs (Var i) = f i
printLambda' f fvs (App t u) = "(" ++ printLambda' f fvs t ++ " " ++ printLambda' f fvs u ++ ")"
printLambda' f (fv:s) (Lam t) = "(λ" ++ fv ++ "." ++ printLambda' f' s t ++ ")"
  where f' Zero = fv
        f' (Succ i) = f i

printLambda :: Lambda -> String
printLambda = printLambda' (\_ -> "") freshVars

-- >>> take 50 freshVars
-- ["x","y","z","u","v","w","x1","y1","z1","u1","v1","w1","x2","y2","z2","u2","v2","w2","x3","y3","z3","u3","v3","w3","x4","y4","z4","u4","v4","w4","x5","y5","z5","u5","v5","w5","x6","y6","z6","u6","v6","w6","x7","y7","z7","u7","v7","w7","x8","y8"]


-- >>> printLambda (Lam (Lam (Var (Succ Zero))))
-- "(\955x.(\955y.x))"

-- >>> :set -XUnicodeSyntax


-- >>> printLambda (App (Lam (Var Zero)) (Lam (Lam (Var (Succ Zero)))))
-- "((\955x.x) (\955x.(\955y.x)))"
