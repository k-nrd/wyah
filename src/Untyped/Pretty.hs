module Untyped.Pretty where

import Data.Text (unpack)
import Text.PrettyPrint (Doc, char, hsep, parens, render, text, (<+>), (<>))
import Untyped.Parser (Expr (..), Lit (..), Name)

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ n = text (unpack n)

instance Pretty Expr where
  ppr p e =
    case e of
      Lit (LInt a) -> text (show a)
      Lit (LBool b) -> text (show b)
      Var x -> text (unpack x)
      App a b -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
      Lam _ a ->
        parensIf (p > 0) $
          char '\\'
            Text.PrettyPrint.<> hsep (fmap pp (viewVars a))
            <+> "->"
            <+> ppr (p + 1) (viewBody a)

ppexpr :: Expr -> String
ppexpr = render . ppr 0
