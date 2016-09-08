module Main
( main
) where

import Control.Monad.Eff.Console (log)
import Data.Lazy (force)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Eulalie.Error (print)
import Data.Eulalie.Parser (eof, parse)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (stream)
import Data.String (toCharArray)
import GammaScript.Check (infer, runCheck)
import GammaScript.Parse (expr)
import GammaScript.Syntax (prettyExpr)
import GammaScript.Type (prettyScheme)
import Prelude

main = do
  example "x"
  example "Λx. x"
  example "Fun x. x"
  example "Λx. Λy. x"
  example "Λf. (Λx. f (x x)) (Λx. f (x x))"
  example "Λx. x x"
  example "(Λx. x) (Λx. x)"
  example "Let x = Λx. x In x x"
  example "Λf. Λx. f (f x)"
  example "Μf. f (Λx. x)"
  example "Μf. Λf. f"
  example "Let x = Λa. a In Μf. x"

  where example s =
          case parse (force expr <* eof) (stream (toCharArray s)) of
            Error e -> log $ "parse error: " <> print e
            Success {value: e} -> do
              log $ prettyExpr e
              case runCheck (infer Map.empty e) of
                Left er -> log $ "type error: " <> er
                Right τ -> log $ "  : " <> prettyScheme τ
