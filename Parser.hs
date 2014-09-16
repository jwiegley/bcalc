module Parser where

import Control.Applicative
import Data.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (many)
import Text.Parsec.Text

data Expr
    = EAdd Expr Expr
    | ENum Int
    deriving Show

whiteSpace :: Parser ()
whiteSpace = () <$ many (char ' ')

num :: Parser Expr
num = ENum . read <$> many (oneOf "0123456789")

add :: Parser Expr
add = EAdd <$> (expr <* whiteSpace) <*> (string "+" *> whiteSpace *> expr)

expr :: Parser Expr
expr = choice [ add, num ] <* eof

evalExpr :: Expr -> Int
evalExpr (ENum n) = n
evalExpr (EAdd x y) = evalExpr x + evalExpr y

test :: Text -> IO ()
test str = case parse expr "" str of
    Left e  -> error (show e)
    Right x -> print (evalExpr x)
