module Calculator.Pretty where

import Calculator.Expr (Expr (..))
import Calculator.Type (Type (..))
import Text.PrettyPrint (Doc, parens, render, text, (<+>))

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Expr where
  ppr :: Int -> Expr -> Doc
  ppr p expr = case expr of
    Zero -> text "0"
    Tr -> text "true"
    Fl -> text "false"
    Succ a -> parensIf (p > 0) $ text "succ" <+> ppr (p + 1) a
    Pred a -> parensIf (p > 0) $ text "pred" <+> ppr (p + 1) a
    IsZero a -> parensIf (p > 0) $ text "iszero" <+> ppr (p + 1) a
    If a b c ->
      text "if" <+> ppr p a
        <+> text "then"
        <+> ppr p b
        <+> text "else"
        <+> ppr p c

instance Pretty Type where
  ppr _ TNat = text "Nat"
  ppr _ TBool = text "Bool"

ppexpr :: Expr -> String
ppexpr = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0
