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
prettyExpr (EApp e1 e2) = "(" <> prettyExpr e1 <> ") (" <> prettyExpr e2 <> ")"
prettyExpr (EAbs x e) = "λ" <> x <> ". " <> prettyExpr e
prettyExpr (ELet x e1 e2) = "let " <> x <> " = " <> prettyExpr e1 <> " in " <> prettyExpr e2
