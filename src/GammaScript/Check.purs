module GammaScript.Check
( Check
, runCheck
, freshTVar
, checkError

, Γ

, generalize
, instantiate

, unify
, infer
) where

import Control.Comonad.Cofree (Cofree, tail)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.State.Class as State
import Control.Monad.State.Trans (StateT, evalStateT)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.List ((:), List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import GammaScript.Syntax (Expr(..), prettyExpr)
import GammaScript.Type (composeSubst, freeTVars, Scheme(..), subst, Type(..))
import Prelude


type Check = ReaderT {stack :: List (Cofree Expr Unit)} (StateT {supply :: Int} (Either String))

runCheck :: forall a. Check a -> Either String a
runCheck chk = evalStateT (runReaderT chk r) s
  where s = {supply: 0}
        r = {stack: Nil}

freshTVar :: Check Type
freshTVar = do
  i <- _.supply <$> (State.get :: Check {supply :: Int})
  State.modify \s -> s { supply = s.supply + 1 }
  pure $ TVar ("a" <> show i)

checkError :: forall a. String -> Check a
checkError msg = do
  {stack} <- (ask :: Check {stack :: List (Cofree Expr Unit)})
  throwError $ msg <> fold (map entry stack)
  where entry e = "\nin " <> prettyExpr e

localStack :: forall a r. Cofree Expr a -> Check r -> Check r
localStack e chk = local (\r -> r {stack = void e : r.stack}) chk


type Γ = Map String Scheme


generalize :: Γ -> Type -> Scheme
generalize γ τ = Scheme qs τ
  where qs = freeTVars τ `Set.difference` fold (map freeTVars γ)

instantiate :: Scheme -> Check Type
instantiate (Scheme quantis τ) = do
  let quantis' = List.fromFoldable quantis
  unis <- traverse (const freshTVar) quantis'
  pure $ subst (Map.fromList (List.zip quantis' unis)) τ


unify :: Type -> Type -> Check (Map String Type)
unify (TVar n) τ = solve n τ
unify τ (TVar n) = solve n τ
unify (TFun τ1 σ1) (TFun τ2 σ2) = do
  s1 <- unify τ1 τ2
  s2 <- unify (subst s1 σ1) (subst s1 σ2)
  pure $ s1 `composeSubst` s2

solve :: String -> Type -> Check (Map String Type)
solve n (TVar m) | n == m = pure Map.empty
solve n τ
  | n `Set.member` freeTVars τ = checkError "infinite type"
  | otherwise = pure $ Map.singleton n τ

infer :: forall a. Γ -> Cofree Expr a -> Check Scheme
infer γ e = do
  {subst: s, type: τ} <- infer' γ e
  pure $ generalize γ (subst s τ)

infer' :: forall a. Γ -> Cofree Expr a -> Check {subst :: Map String Type, type :: Type}
infer' = \γ e -> localStack e (go γ (tail e))
  where
  go γ (EVar n) = case Map.lookup n γ of
    Just τ -> {subst: Map.empty :: Map String Type, type: _} <$> instantiate τ
    Nothing -> checkError $ "unknown: " <> n
  go γ (EApp e1 e2) = do
    ρ <- freshTVar
    {subst: s1, type: τ1} <- infer' γ e1
    {subst: s2, type: τ2} <- infer' (map (subst s1) γ) e2
    s3 <- unify (subst s2 τ1) (TFun τ2 ρ)
    pure {subst: s3 `composeSubst` s2 `composeSubst` s1, type: subst s3 ρ}
  go γ (EAbs x e) = do
    π <- freshTVar
    let γ' = Map.delete x γ
        γ'' = Map.insert x (Scheme Set.empty π) γ'
    {subst: s, type: τ} <- infer' γ'' e
    pure {subst: s, type: TFun (subst s π) τ}
  go γ (ELet x e1 e2) = do
    {subst: s1, type: τ1} <- infer' γ e1
    let γ' = Map.delete x γ
        τ1' = generalize (map (subst s1) γ) τ1
        γ'' = Map.insert x τ1' γ'
    {subst: s2, type: τ2} <- infer' γ'' e2
    pure {subst: s1 `composeSubst` s2, type: τ2}
  go γ (EFix x e) = do
    φ <- freshTVar
    let γ' = Map.delete x γ
        γ'' = Map.insert x (Scheme Set.empty φ) γ'
    {subst: s1, type: τ} <- infer' γ'' e
    s2 <- unify φ τ
    pure {subst: s1 `composeSubst` s2, type: τ}
    checkError $ "Μ not safe!"
