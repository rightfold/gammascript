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
  example "Data nat End\nΛx. x"
  example "Data bool\n  | true\n  | false\nEnd\nData unit\n  | unit\nEnd\nΛf. f true false unit"
  example "Data bool\n  | true\n  | false\nEnd\nData unit\n  | unit\nEnd\nΛf. Λg. f (g true) (g unit)"
  example "Data nat\n  | z\n  | s nat\nEndΛx. s (s (s x))"

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
