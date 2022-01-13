module Utils.Parser where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (label), Parsec, between)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (lexeme, skipBlockCommentNested, skipLineComment, space, symbol)

type Parser = Parsec Void Text

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

reservedName :: a -> String -> Parser a
reservedName constructor word =
  label word $
    fmap (const constructor) (string (pack word))

prefixOp :: (a -> a) -> String -> Parser a -> Parser a
prefixOp constructor word parser = label word $ do
  _ <- sym (pack word)
  fmap constructor parser

parens :: Parser a -> Parser a
parens = between (sym "(") (string ")")
