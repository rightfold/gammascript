module GammaScript.Syntax
( TopLevel(..)

, Expr(..)

, prettyExpr
) where

import Control.Comonad.Cofree (Cofree, tail)
import Prelude


data TopLevel a = TopLevel (Cofree Expr a)


data Expr a
  = EVar String
  | EApp a a
  | EAbs String a
  | ELet String a a
  | EFix String a

instance functorExpr :: Functor Expr where
  map _ (EVar n) = EVar n
  map f (EApp e1 e2) = EApp (f e1) (f e2)
  map f (EAbs x e) = EAbs x (f e)
  map f (ELet x e1 e2) = ELet x (f e1) (f e2)
  map f (EFix x e) = EFix x (f e)


prettyExpr :: forall a. Cofree Expr a -> String
prettyExpr = tail >>> go
  where
  go (EVar n) = n
  go (EApp e1 e2) = e1' <> " " <> e2'
    where e1' | safeL (tail e1) =        go (tail e1)
              | otherwise       = "(" <> go (tail e1) <> ")"
          e2' | safeR (tail e2) =        go (tail e2)
              | otherwise       = "(" <> go (tail e2) <> ")"
          safeL (EVar _) = true
          safeL (EApp _ _) = true
          safeL (EAbs _ _) = false
          safeL (ELet _ _ _) = false
          safeL (EFix _ _) = false
          safeR (EVar _) = true
          safeR (EApp _ _) = false
          safeR (EAbs _ _) = true
          safeR (ELet _ _ _) = true
          safeR (EFix _ _) = true
  go (EAbs x e) = "Λ" <> x <> ". " <> go (tail e)
  go (ELet x e1 e2) = "Let " <> x <> " = " <> go (tail e1) <> " In " <> go (tail e2)
  go (EFix x e) = "Μ" <> x <> ". " <> go (tail e)
