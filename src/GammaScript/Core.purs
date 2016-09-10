module GammaScript.Core
( Core(..)
, fromTopLevel
, fromADT
, fromExpr
) where

import Control.Comonad.Cofree (Cofree, tail)
import Data.List ((:), List(..))
import Data.List as List
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl, foldr)
import GammaScript.Syntax (ADT(..), Expr(..), TopLevel(..))
import Prelude


-- `(..)` from `Data.List` is broken.
range :: Int -> Int -> List Int
range a b
  | a <= b = a : range (a + 1) b
  | otherwise = Nil


data Core
  = CVar String
  | CApp Core Core
  | CAbs String Core

fromTopLevel :: forall a. TopLevel a -> Core
fromTopLevel (TopLevel adts e) = foldr fromADT (fromExpr e) adts

--| Turn an ADT into lambda terms using Church encoding. For example:
--|
--|     Data nat
--|       | z
--|       | s z
--|     End
--|
--| turns into:
--|
--|     z = λa. λb. a
--|     s = λn. λa. λb. b n
--|
--| Terminology:
--|
--|     s = λn. λa. λb. b n
--|         ^^              constructor
--|             ^^^^^^^     deconstructor
--|                     ^^^ selector
fromADT :: ADT -> Core -> Core
fromADT (ADT _ ctors) e = foldr fromCtor e (Map.toList ctors)
  where fromCtor (Tuple name params) e = CApp (CAbs name e) (constructor params)
          where constructor params = foldr CAbs deconstructor params'
                  where params' = map (("__v" <> _) <<< show) (1 `range` List.length params)
                        deconstructor = foldr CAbs selector (Map.keys ctors)
                        selector = foldl CApp (CVar name) (map CVar params')

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
