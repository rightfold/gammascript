module GammaScript.Syntax
( TopLevel(..)
, prettyTopLevel

, ADT(..)
, prettyADT

, Expr(..)
, freeEVars
, prettyExpr
) where

import Control.Comonad.Cofree (Cofree, tail)
import Data.Foldable (fold)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import GammaScript.Type (prettyType, Type)
import Prelude


data TopLevel a = TopLevel (List ADT) (Cofree Expr a)

prettyTopLevel :: forall a. TopLevel a -> String
prettyTopLevel (TopLevel adts e) =
  fold (map (\a -> prettyADT a <> "\n") adts) <> prettyExpr e


data ADT = ADT String (Map String (List Type))

prettyADT :: ADT -> String
prettyADT (ADT n ctors) =
  "Data " <> n <> "\n" <> fold (map ctor (Map.toList ctors)) <> "End"
  where ctor (Tuple ctorN params) = "  | " <> ctorN <> fold (map param params) <> "\n"
        param τ = " " <> prettyType τ


data Expr a
  = EVar String
  | EApp a a
  | EAbs String a
  | ELet String a a
  | EFix String a
  | EMat a (Map String {fields :: List String, value :: a})

instance functorExpr :: Functor Expr where
  map _ (EVar n) = EVar n
  map f (EApp e1 e2) = EApp (f e1) (f e2)
  map f (EAbs x e) = EAbs x (f e)
  map f (ELet x e1 e2) = ELet x (f e1) (f e2)
  map f (EFix x e) = EFix x (f e)
  map f (EMat e cs) = EMat (f e) (map (\c -> c { value = f c.value }) cs)

freeEVars :: forall a. Cofree Expr a -> Set String
freeEVars = tail >>> go
  where go (EVar n) = Set.singleton n
        go (EApp e1 e2) = freeEVars e1 <> freeEVars e2
        go (EAbs x e) = Set.delete x (freeEVars e)
        go (ELet x e1 e2) = freeEVars e1 <> Set.delete x (freeEVars e2)
        go (EFix x e) = Set.delete x (freeEVars e)
        go (EMat e cs) = freeEVars e <> fold (map freeCVars cs)
          where freeCVars {fields, value} =
                  freeEVars value `Set.difference` Set.fromFoldable fields

prettyExpr :: forall a. Cofree Expr a -> String
prettyExpr = tail >>> go
  where
  go (EVar n) = n
  go (EApp e1 e2) = e1' <> " " <> e2'
    where e1' | safeL (tail e1) =        go (tail e1)
              | otherwise       = "(" <> go (tail e1) <> ")"
          e2' | safeR (tail e2) =        go (tail e2)
              | otherwise       = "(" <> go (tail e2) <> ")"
          safeL (EVar _) = true
          safeL (EApp _ _) = true
          safeL (EAbs _ _) = false
          safeL (ELet _ _ _) = false
          safeL (EFix _ _) = false
          safeL (EMat _ _) = true
          safeR (EVar _) = true
          safeR (EApp _ _) = false
          safeR (EAbs _ _) = true
          safeR (ELet _ _ _) = true
          safeR (EFix _ _) = true
          safeR (EMat _ _) = true
  go (EAbs x e) = "Λ" <> x <> ". " <> go (tail e)
  go (ELet x e1 e2) = "Let " <> x <> " = " <> go (tail e1) <> " In " <> go (tail e2)
  go (EFix x e) = "Μ" <> x <> ". " <> go (tail e)
  go (EMat e cs) = "Match " <> go (tail e) <> "\n" <> fold cs' <> "End"
    where cs' = Map.toList cs # map \(Tuple ctor {fields, value}) ->
                  let fields' = fold (map (" " <> _) fields)
                   in "  | " <> ctor <> fields' <> ". " <> go (tail value) <> "\n"
