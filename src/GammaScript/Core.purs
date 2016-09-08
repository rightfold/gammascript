module GammaScript.Core
( Core(..)
, fromExpr
) where

import Control.Comonad.Cofree (Cofree, tail)
import GammaScript.Syntax (Expr(..))
import Prelude


data Core
  = CVar String
  | CApp Core Core
  | CAbs String Core

fromExpr :: forall a. Cofree Expr a -> Core
fromExpr = tail >>> go
  where go (EVar n) = CVar n
        go (EApp e1 e2) = CApp (fromExpr e1) (fromExpr e2)
        go (EAbs x e) = CAbs x (fromExpr e)
        go (ELet x e1 e2) = CApp (CAbs x (fromExpr e2)) (fromExpr e1)
        go (EFix x e) = CApp y (CAbs x (fromExpr e))
          where y  = CAbs "f" (CApp y' y')
                y' = CAbs "g" (CApp f (CAbs "z" (CApp (CApp g g) z)))
                f  = CVar "f"
                g  = CVar "g"
                z  = CVar "z"
        go (EMat _ _) = CAbs "N" (CAbs "Y" (CAbs "I" (CVar "x")))
