module Main
( main
) where

import Control.Monad.Eff.Console (log)
import Data.Map as Map
import Data.Either (Either(..))
import GammaScript.Check (infer, runCheck)
import GammaScript.Syntax (Expr(..), prettyExpr)
import GammaScript.Type (prettyScheme)
import Prelude

main = do
  let x = EVar "x"
      f = EVar "f"

  example $ x
  example $ EAbs "x" x
  example $ EAbs "x" (EAbs "y" x)
  example $ EAbs "f" (EApp (EAbs "x" (EApp f (EApp x x))) (EAbs "x" (EApp f (EApp x x))))
  example $ EAbs "x" (EApp x x)
  example $ EApp (EAbs "x" x) (EAbs "x" x)
  example $ ELet "x" (EAbs "x" x) (EApp x x)

  where example e = do
          log $ prettyExpr e
          case runCheck (infer Map.empty e) of
            Left  e -> log $ "  : (type error: " <> e <> ")"
            Right τ -> log $ "  : " <> prettyScheme τ
