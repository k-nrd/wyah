module Calculator.Lexer where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, label), Parsec, between, choice, errorBundlePretty, parse)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (lexeme, skipBlockCommentNested, skipLineComment, space, symbol)

type Parser = Parsec Void Text

data Expr
  = Tr
  | Fl
  | Zero
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  deriving stock (Eq, Show)

skipSpace :: Parser ()
skipSpace =
  space
    space1
    (skipLineComment "--")
    (skipBlockCommentNested "{-" "-}")

lxm :: Parser a -> Parser a
lxm = lexeme skipSpace

sym :: Text -> Parser Text
sym = symbol skipSpace

reservedName :: Expr -> String -> Parser Expr
reservedName constructor word =
  label word $
    fmap (const constructor) (string (pack word))

prefixOp :: (Expr -> Expr) -> String -> Parser Expr
prefixOp constructor word = label word $ do
  _ <- sym (pack word)
  fmap constructor expr

parens :: Parser a -> Parser a
parens = between (sym "(") (string ")")

true :: Parser Expr
true = reservedName Tr "true"

false :: Parser Expr
false = reservedName Fl "false"

zero :: Parser Expr
zero = reservedName Zero "0"

suc :: Parser Expr
suc = prefixOp Succ "succ"

prd :: Parser Expr
prd = prefixOp Pred "pred"

isZero :: Parser Expr
isZero = prefixOp IsZero "iszero"

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
