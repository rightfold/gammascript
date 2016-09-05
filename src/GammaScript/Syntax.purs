module GammaScript.Syntax
( Expr(..)

, prettyExpr
) where

import Prelude


data Expr
  = EVar String
  | EApp Expr Expr
  | EAbs String Expr
  | ELet String Expr Expr


prettyExpr :: Expr -> String
prettyExpr (EVar n) = n
prettyExpr (EApp e1 e2) = e1' <> " " <> e2'
  where e1' | safeL e1  =        prettyExpr e1
            | otherwise = "(" <> prettyExpr e1 <> ")"
        e2' | safeR e2  =        prettyExpr e2
            | otherwise = "(" <> prettyExpr e2 <> ")"
        safeL (EVar _) = true
        safeL (EApp _ _) = true
        safeL (EAbs _ _) = false
        safeL (ELet _ _ _) = false
        safeR (EVar _) = true
        safeR (EApp _ _) = false
        safeR (EAbs _ _) = true
        safeR (ELet _ _ _) = true
prettyExpr (EAbs x e) = "λ" <> x <> ". " <> prettyExpr e
prettyExpr (ELet x e1 e2) = "let " <> x <> " = " <> prettyExpr e1 <> " in " <> prettyExpr e2
