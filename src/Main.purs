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
import GammaScript.Check (check, runCheck)
import GammaScript.Core as Core
import GammaScript.Lua as Lua
import GammaScript.Parse (topLevel)
import GammaScript.Syntax (prettyTopLevel, TopLevel(..))
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
  example """
    Data void
    End
    Λx. x
  """
  example """
    Data bool
      | true
      | false
    End
    Data unit
      | unit
    End
    Λf. f true false unit
  """
  example """
    Data bool
      | true
      | false
    End
    Data unit
      | unit
    End
    Λf. Λg. f (g true) (g unit)
  """
  example """
    Data nat
      | z
      | s nat
    End
    Λx. s (s (s x))
  """
  example """
    Data nat
      | z
      | s nat
    End
    Let add = Μadd. Λa.
      Match a
        | z. Λb. b
        | s x. Λb. s (add x b)
      End
    In
    add
  """
  example """
    Data bool
      | true
      | false
    End
    Data nat
      | z
      | s nat
    End
    Let is_z = Λa.
      Match a
        | z. true
        | s x. false
      End
    In
    is_z
  """

  where example s = do
          case parse (force topLevel <* eof) (stream (toCharArray s)) of
            Error e -> log $ "parse error: " <> print e
            Success {value: tl@(TopLevel _ e)} -> do
              log $ prettyTopLevel tl
              case runCheck (check Map.empty tl) of
                Left er -> log $ "type error: " <> er
                Right τ -> do
                  log $ "  : " <> prettyScheme τ
                  log $ "--[[\n" <> Lua.fromCore (Core.fromExpr e) <> "\n--]]"
          log ""
