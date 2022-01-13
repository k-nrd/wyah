module Calculator.Check where

import Calculator.Expr (Expr (..))
import Calculator.Pretty (pptype)
import Calculator.Type (Type (..))
import Control.Monad.Except (Except, MonadError (throwError), runExcept)

type Check a = Except TypeError a

data TypeError = TypeMismatch Type Type

instance Show TypeError where
  show (TypeMismatch a b) = "Type mismatch: " ++ pptype a ++ " is not " ++ pptype b

check :: Expr -> Either TypeError Type
check = runExcept . typeof

checkNat :: Expr -> Check Type
checkNat expr = do
  ta <- typeof expr
  case ta of
    TNat -> return TNat
    _ -> throwError $ TypeMismatch ta TNat

checkBool :: Expr -> Check Type
checkBool expr = do
  ta <- typeof expr
  case ta of
    TNat -> return TBool
    _ -> throwError $ TypeMismatch ta TNat

typeof :: Expr -> Check Type
typeof = \case
  Succ a -> checkNat a
  Pred a -> checkNat a
  IsZero a -> checkBool a
  Tr -> return TBool
  Fl -> return TBool
  Zero -> return TNat
  If a b c -> do
    ta <- typeof a
    tb <- typeof b
    tc <- typeof c
    if ta /= TBool
      then throwError $ TypeMismatch ta TBool
      else
        if tb /= tc
          then throwError $ TypeMismatch tb tc
          else return tc
