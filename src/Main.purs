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
  example $ EVar "x"
  example $ EAbs "x" (EVar "x")
  example $ EAbs "x" (EApp (EVar "x") (EVar "x"))
  example $ EApp (EAbs "x" (EVar "x")) (EAbs "x" (EVar "x"))
  example $ ELet "x" (EAbs "x" (EVar "x")) (EApp (EVar "x") (EVar "x"))

  where example e = do
          log $ prettyExpr e
          case runCheck (infer Map.empty e) of
            Left  e -> log $ "  : (type error: " <> e <> ")"
            Right τ -> log $ "  : " <> prettyScheme τ
