module Calculator.Parser where

import Calculator.Expr (Expr (..))
import Data.Text (Text)
import Text.Megaparsec (MonadParsec (eof, label), between, choice, errorBundlePretty, parse)
import Utils.Parser (Parser, lxm, parens, prefixOp, reservedName, skipSpace, sym)

true :: Parser Expr
true = reservedName Tr "true"

false :: Parser Expr
false = reservedName Fl "false"

zero :: Parser Expr
zero = reservedName Zero "0"

suc :: Parser Expr
suc = prefixOp Succ "succ" expr

prd :: Parser Expr
prd = prefixOp Pred "pred" expr

isZero :: Parser Expr
isZero = prefixOp IsZero "iszero" expr

ifThen :: Parser Expr
ifThen = label "ifThen" $ do
  _ <- sym "if"
  cond <- expr
  _ <- sym "then"
  tr <- expr
  _ <- sym "else"
  If cond tr <$> expr

expr :: Parser Expr
expr =
  label "expr" $
    lxm $
      choice
        [ true,
          false,
          zero,
          ifThen,
          isZero,
          suc,
          prd,
          parens expr
        ]

contents :: Parser a -> Parser a
contents = between skipSpace eof

parseExpr :: Text -> Either String Expr
parseExpr input =
  case parse (between skipSpace eof expr) "" input of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output
