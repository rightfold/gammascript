module GammaScript.Type
( Type(..)
, Scheme(..)

, class Subst
, freeTVars
, subst
, composeSubst

, prettyType
, prettyScheme
) where

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Prelude


data Type
  = TVar String
  | TFun Type Type
  | TCon String

data Scheme = Scheme (Set String) Type


class Subst a where
  freeTVars :: a -> Set String
  subst     :: Map String Type -> a -> a

instance substType :: Subst Type where
  freeTVars (TVar n)   = Set.singleton n
  freeTVars (TFun τ σ) = freeTVars τ <> freeTVars σ
  freeTVars (TCon _) = Set.empty
  subst s (TVar n)   = fromMaybe (TVar n) (Map.lookup n s)
  subst s (TFun τ σ) = TFun (subst s τ) (subst s σ)
  subst _ (TCon n) = TCon n

instance substScheme :: Subst Scheme where
  freeTVars (Scheme quantis τ) = freeTVars τ `Set.difference` quantis
  subst s (Scheme quantis τ) = Scheme quantis (subst (foldr Map.delete s quantis) τ)

composeSubst :: Map String Type -> Map String Type -> Map String Type
composeSubst s1 s2 = map (subst s1) s2 <> s1


prettyType :: Type -> String
prettyType (TVar n) = n
prettyType (TFun τ σ) = τ' <> " → " <> σ'
  where τ' | safeL τ   =        prettyType τ
           | otherwise = "(" <> prettyType τ <> ")"
        σ' | safeR σ   =        prettyType σ
           | otherwise = "(" <> prettyType σ <> ")"
        safeL (TVar _) = true
        safeL (TFun _ _) = false
        safeL (TCon _) = true
        safeR (TVar _) = true
        safeR (TFun _ _) = true
        safeR (TCon _) = true
prettyType (TCon n) = n

prettyScheme :: Scheme -> String
prettyScheme (Scheme quantis τ)
  | Set.isEmpty quantis = prettyType τ
  | otherwise = "∀ " <> String.joinWith " " (Array.fromFoldable quantis) <> ". " <> prettyType τ
