module Main
( main
) where

import Control.Comonad.Cofree (mkCofree)
import Control.Monad.Eff.Console (log)
import Data.Map as Map
import Data.Either (Either(..))
import GammaScript.Check (infer, runCheck)
import GammaScript.Syntax (Expr(..), prettyExpr)
import GammaScript.Type (prettyScheme)
import Prelude

main = do
  let x = cf (EVar "x")
      f = cf (EVar "f")

  example $ x
  example $ cf (EAbs "x" x)
  example $ cf (EAbs "x" (cf (EAbs "y" x)))
  example $ cf (EAbs "f" (cf (EApp (cf (EAbs "x" (cf (EApp f (cf (EApp x x)))))) (cf (EAbs "x" (cf (EApp f (cf (EApp x x)))))))))
  example $ cf (EAbs "x" (cf (EApp x x)))
  example $ cf (EApp (cf (EAbs "x" x)) (cf (EAbs "x" x)))
  example $ cf (ELet "x" (cf (EAbs "x" x)) (cf (EApp x x)))
  example $ cf (EAbs "f" (cf (EAbs "x" (cf (EApp f (cf (EApp f x)))))))

  where example e = do
          log $ prettyExpr e
          case runCheck (infer Map.empty e) of
            Left  e -> log $ "type error: " <> e
            Right τ -> log $ "  : " <> prettyScheme τ
        cf = mkCofree unit
