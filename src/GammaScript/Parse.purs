module GammaScript.Parse
( topLevel
, expr
) where

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Eulalie.Char as C
import Data.Eulalie.Parser as P
import Data.Foldable (foldl)
import Data.Lazy (defer, force, Lazy)
import Data.List ((:))
import GammaScript.Syntax (Expr(..), TopLevel(..))
import Partial.Unsafe (unsafePartial)
import Prelude


topLevel :: Lazy (P.Parser Char (TopLevel Unit))
topLevel = defer \_ -> TopLevel <$> force expr

expr :: Lazy (P.Parser Char (Cofree Expr Unit))
expr = defer \_ -> force absLevel
  where
  absLevel = defer \_ -> force abs <|> force let_ <|> force fix <|> force appLevel
    where
    abs = defer \_ -> do
      lLambda
      x <- lIdent
      lPeriod
      e <- force absLevel
      pure $ mkCofree unit (EAbs x e)
    let_ = defer \_ -> do
      lLet
      x <- lIdent
      lEq
      e1 <- force absLevel
      lIn
      e2 <- force absLevel
      pure $ mkCofree unit (ELet x e1 e2)
    fix = defer \_ -> do
      lFix
      x <- lIdent
      lPeriod
      e <- force absLevel
      pure $ mkCofree unit (EFix x e)

  appLevel = defer \_ -> unsafePartial do
    (e : es) <- P.many1 (force varLevel)
    pure $ foldl (\e1 e2 -> mkCofree unit (EApp e1 e2)) e es

  varLevel = defer \_ -> force var <|> force paren
    where
    var = defer \_ -> mkCofree unit <<< EVar <$> lIdent
    paren = defer \_ -> lLParen >>= \_ -> force expr <* lRParen


lexeme :: forall a. P.Parser Char a -> P.Parser Char a
lexeme p = P.many space *> p <* P.many space
  where space = C.space

lLambda :: P.Parser Char Unit
lLambda = lexeme $ void (C.char 'Λ' <|> C.char '\\')

lLet :: P.Parser Char Unit
lLet = lexeme $ void (C.char 'L' *> C.char 'e' *> C.char 't')

lIn :: P.Parser Char Unit
lIn = lexeme $ void (C.char 'I' *> C.char 'n')

lFix :: P.Parser Char Unit
lFix = lexeme $ void ((C.char 'F' *> C.char 'i' *> C.char 'x') <|> C.char 'Μ')

lIdent :: P.Parser Char String
lIdent = lexeme $ C.many1 (C.lower <|> C.char '_')

lPeriod :: P.Parser Char Unit
lPeriod = lexeme $ void (C.char '.')

lEq :: P.Parser Char Unit
lEq = lexeme $ void (C.char '=')

lLParen :: P.Parser Char Unit
lLParen = lexeme $ void (C.char '(')

lRParen :: P.Parser Char Unit
lRParen = lexeme $ void (C.char ')')
