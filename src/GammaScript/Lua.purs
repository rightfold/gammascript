module GammaScript.Lua
( fromCore
) where

import GammaScript.Core (Core(..))
import Prelude


fromCore :: Core -> String
fromCore (CVar n) = "v" <> n
fromCore (CApp e1 e2) = "(" <> fromCore e1 <> ")(" <> fromCore e2 <> ")"
fromCore (CAbs x e) = "function(v" <> x <> ")\nreturn " <> fromCore e <> "\nend"
