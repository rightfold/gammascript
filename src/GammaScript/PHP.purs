module GammaScript.PHP
( fromCore
) where

import Data.Array as Array
import GammaScript.Core (Core(..))
import Data.Set as Set
import Data.String as String
import Prelude


fromCore :: Core -> String
fromCore = go Set.empty
  where go _  (CVar n) = "$" <> n
        go vs (CApp e1 e2) = "(" <> go vs e1 <> ")(" <> go vs e2 <> ")"
        go vs (CAbs x e) =
          "function($" <> x <> ")"
          <> use vs
          <> " {\nreturn "
          <> go (Set.insert x vs) e
          <> ";\n}"

        use vs | Set.isEmpty vs = ""
               | otherwise = " use(" <> String.joinWith ", " (map ("$" <> _) (Array.fromFoldable vs)) <> ")"
