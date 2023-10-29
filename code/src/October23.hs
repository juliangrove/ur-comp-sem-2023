{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module October23 where

import Prelude hiding (Word, Either(..))

-- | Applicative categorial grammar

data Cat = NP | N | S
         | Cat :\: Cat
         | Cat :/: Cat

data Word (c :: Cat) = Word String (IsA c)

data IsA (c :: Cat) where
  IsAnNP :: IsA NP
  IsAnN :: IsA N
  IsAnS :: IsA S
  (::\::) :: IsA c1 -> IsA c2 -> IsA (c1 :\: c2)
  (::/::) :: IsA c1 -> IsA c2 -> IsA (c1 :/: c2)

instance Show (IsA c) where
  show IsAnNP = "np"
  show IsAnN = "n"
  show IsAnS = "s"
  show (c1 ::\:: c2) = "(" ++ show c1 ++ "\\" ++ show c2 ++ ")"
  show (c1 ::/:: c2) = "(" ++ show c1 ++ "/" ++ show c2 ++ ")"

instance Show (Word c) where
  show (Word s c) = "(" ++ s ++ " ⊢ " ++ show c ++ ")"

data Expr (c :: Cat) where
  Lex :: Word c -> Expr c
  AppL :: Expr c1 -> Expr (c1 :\: c2) -> Expr c2
  AppR :: Expr (c2 :/: c1) -> Expr c1 -> Expr c2

instance Show (Expr c) where
  show (Lex w) = show w
  show (AppL e1 e2) = "(" ++ show e1 ++ " ◃ " ++ show e2 ++ ")"
  show (AppR e1 e2) = "(" ++ show e1 ++ " ▹ " ++ show e2 ++ ")"

julianSlept :: Expr S
julianSlept = (AppL (Lex (Word "julian" IsAnNP)) (Lex (Word "slept" (IsAnNP ::\:: IsAnS))))

julianTaughtCarina :: Expr S
julianTaughtCarina = (AppL (Lex (Word "julian" IsAnNP)) (AppR (Lex (Word "taught" ((IsAnNP ::\:: IsAnS) ::/:: IsAnNP))) (Lex (Word "carina" IsAnNP))))


-- | Simply typed λ-calculus

-- Types (a.k.a. formulae)
data Type = E | T         -- Atomic types
          | Type :/\ Type -- Conjunctions
          | Type :\/ Type -- Disjunctions
          | Type :-> Type -- Implications
          | Taut          -- Tautology
          | Contr         -- Contradiction

-- Contexts
data Context = Empty | Cons Type Context

-- Ways of being in a context
data In (φ :: Type) (γ :: Context) where
  First :: In φ (Cons φ γ)
  Next :: In φ γ -> In φ (Cons ψ γ)

data Constant (φ :: Type) where
  Dog :: Constant (E :-> T)
  Sleep :: Constant (E :-> T)
  Teach :: Constant (E :-> (E :-> T))
  C :: Constant E
  J :: Constant E

-- Simply typed λ-terms (a.k.a. proofs in the implicational fragment of
-- propsitional logic)
data Term (γ :: Context) (φ :: Type) where
  Var :: In φ γ -> Term γ φ                        -- a.k.a. Ax
  Lam :: Term (Cons φ γ) ψ -> Term γ (φ :-> ψ)     -- a.k.a. →I
  App :: Term γ (φ :-> ψ) -> Term γ φ -> Term γ ψ  -- a.k.a. →E
  Con :: Constant φ -> Term γ φ

reorder :: forall γ δ ψ. (forall φ. In φ γ -> In φ δ) -> Term γ ψ -> Term δ ψ
reorder f (Var i) = Var (f i)
reorder f (Lam t) = Lam (reorder g t)
  where g :: (forall χ. In χ (Cons φ γ) -> In χ (Cons φ δ))
        g First = First
        g (Next i) = Next (f i)
reorder f (App t u) = App (reorder f t) (reorder f u)

weaken :: Term γ φ -> Term (Cons ψ γ) φ
weaken = reorder Next

subst :: forall γ δ ψ. (forall φ. In φ γ -> Term δ φ) -> Term γ ψ -> Term δ ψ
subst f (Var i) = f i
subst f (Lam t) = Lam (subst g t)
  where g :: forall φ χ. In φ (Cons χ γ) -> Term (Cons χ δ) φ
        g First = Var First
        g (Next i) = weaken (f i)
subst f (App t u) = App (subst f t) (subst f u)

subst0 :: forall γ φ ψ. Term γ φ -> Term (Cons φ γ) ψ -> Term γ ψ
subst0 t = subst f
  where f :: forall χ. In χ (Cons φ γ)-> Term γ χ
        f First = t
        f (Next i) = Var i

normalForm :: Term γ φ -> Term γ φ
normalForm v@(Var _) = v                -- Variables are already in normal form.
normalForm (Lam t) = Lam (normalForm t) -- Abstractions are in normal form just in case their bodies are in normal form.
normalForm (App t u) =
  case normalForm t of
    Lam t' -> normalForm (subst0 (normalForm u) t') -- If the normal form of t is an abstraction, then we need to substitute and further normalize.
    t' -> App t' (normalForm u)                     -- Otherwise, we just need to take the normal form of the argument.

-- | Interpretation

type family SemType (c :: Cat) where
  SemType NP = E
  SemType S = T
  SemType N = E :-> T
  SemType (c1 :\: c2) = SemType c1 :-> SemType c2
  SemType (c2 :/: c1) = SemType c1 :-> SemType c2

interpWord :: Word c -> Term Empty (SemType c)
interpWord (Word "carina" IsAnNP) = Con C                       -- ⟦'carina'⟧ = c
interpWord (Word "julian" IsAnNP) = Con J                       -- ⟦'julian'⟧ = j
interpWord (Word "dog" IsAnN) = Lam (App (Con Dog) (Var First)) -- ⟦'dog'⟧ = λx.dog(x)
interpWord (Word "slept" (IsAnNP ::\:: IsAnS)) =                -- ⟦'slept'⟧ = λx.sleep(x)
  Lam (App (Con Sleep) (Var First))
interpWord (Word "taught" ((IsAnNP ::\:: IsAnS) ::/:: IsAnNP)) = -- ⟦'taught'⟧ = λx, y.teach(x)(y)
  Lam (Lam (App (App (Con Teach) (Var (Next First))) (Var First)))


interpExpr :: Expr c -> Term Empty (SemType c)
interpExpr (Lex w) = interpWord w
interpExpr (AppL e1 e2) = App (interpExpr e2) (interpExpr e1)
interpExpr (AppR e1 e2) = App (interpExpr e1) (interpExpr e2)
