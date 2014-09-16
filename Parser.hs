module Parser where

import Control.Applicative
import Control.Monad (forM_)
import Data.Text as T
import Debug.Trace
import Text.Parsec.Char
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim hiding (many, (<|>))
import Text.Parsec.Text
import Text.Parsec.Token hiding (whiteSpace)

data Expr
    = EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | ENum Double
    deriving Show

whiteSpace :: Parser ()
whiteSpace = () <$ many (char ' ')

num :: Parser Expr
num = trace "num" $ ENum . read <$> go (oneOf "0123456789")
  where
    go np = do
        x <- many np
        mres <- optional (char '.')
        case mres of
            Just _ -> (\y -> x ++ "." ++ y) <$> many np
            Nothing -> return x

term :: Parser Expr
term = trace "term" $ choice [ num ] <* whiteSpace

addop :: Parser (Expr -> Expr -> Expr)
addop = trace "addop" $ EAdd <$ (trace "1.." (string "+" <* whiteSpace))
    <|> ESub <$ (trace "2.." (string "-" <* whiteSpace))

mulop :: Parser (Expr -> Expr -> Expr)
mulop = trace "mulop" $ EMul <$ (string "*" <* whiteSpace)
    <|> EDiv <$ (string "/" <* whiteSpace)

expr :: Parser Expr
expr = trace "expr" $ subexpr `chainl1` addop

subexpr :: Parser Expr
subexpr = trace "subexpr" $ factor `chainl1` mulop

factor :: Parser Expr
factor = parens expr <|> term

evalExpr :: Expr -> Double
evalExpr (ENum n) = n
evalExpr (EAdd x y) = evalExpr x + evalExpr y
evalExpr (ESub x y) = evalExpr x - evalExpr y
evalExpr (EMul x y) = evalExpr x * evalExpr y
evalExpr (EDiv x y) = evalExpr x / evalExpr y

massage :: Text -> Maybe Text
massage txt =
    case T.takeWhile (/= '#') txt of
        "" -> Nothing
        x  -> Just x

test :: Text -> IO ()
test str =
    case massage str of
        Nothing -> return ()
        Just input -> case parse expr "" input of
            Left e  -> error (show e)
            Right x -> print (evalExpr x)

testFile :: FilePath -> IO ()
testFile path = do
    str <- readFile path
    forM_ (T.lines (pack str)) $ \l -> test l
    -- case parse (many expr <* eof) path (pack str) of
    --     Left e   -> error (show e)
    --     Right xs -> forM_ xs $ print . evalExpr
