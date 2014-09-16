{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Map
import Data.Text as T
import Data.Text.IO as T
import Debug.Trace
import Text.Parsec.Char
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim hiding (many, (<|>), State)
import Text.Parsec.Text
import Control.Monad.Morph (hoist)

data Expr
    = EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | ELet Text Expr
    | EVar Text
    | ENum Double
    deriving Show

whiteSpace :: Parser ()
whiteSpace = () <$ many (char ' ')

symbol :: Text -> Parser ()
symbol str = () <$ (whiteSpace *> string (unpack str) <* whiteSpace)

num :: Parser Expr
num = trace "num" $ ENum . read <$> go (oneOf "0123456789")
  where
    go np = do
        x <- many np
        mres <- optional (char '.')
        case mres of
            Just _ -> (\y -> x ++ "." ++ y) <$> many np
            Nothing -> return x

varname :: Parser Text
varname = pack <$> some letter

variable :: Parser Expr
variable = EVar <$> varname

term :: Parser Expr
term = trace "term" $ choice terms <* whiteSpace
  where
    terms = [ letBinding
            , variable
            , try num
            ]

addop :: Parser (Expr -> Expr -> Expr)
addop = trace "addop" $ EAdd <$ symbol "+"
    <|> ESub <$ symbol "-"

mulop :: Parser (Expr -> Expr -> Expr)
mulop = trace "mulop" $ EMul <$ symbol "*"
    <|> EDiv <$ string "/"

expr :: Parser Expr
expr = trace "expr" $ subexpr `chainl1` addop

subexpr :: Parser Expr
subexpr = trace "subexpr" $ factor `chainl1` mulop

parenthesis :: Parser Expr -> Parser Expr
parenthesis = between (symbol "(") (symbol ")")

factor :: Parser Expr
factor = parenthesis expr <|> term

letBinding :: Parser Expr
letBinding = trace "letBinding" $ do
    trace "letBinding Parser.hs:81.." $ return ()
    symbol "let"
    trace "letBinding Parser.hs:83.." $ return ()
    name <- varname
    trace "letBinding Parser.hs:85.." $ return ()
    symbol "="
    trace "letBinding Parser.hs:87.." $ return ()
    body <- expr
    return $ ELet name body

newtype Env = Env { getEnv :: Map Text Double }

newEnv :: Env
newEnv = Env Data.Map.empty

eval :: Expr -> Double
eval x = runIdentity $ evalStateT (evalExpr x) newEnv

evalExpr :: Expr -> State Env Double
evalExpr (ENum n)   = return n
evalExpr (EAdd x y) = (+) <$> evalExpr x <*> evalExpr y
evalExpr (ESub x y) = (-) <$> evalExpr x <*> evalExpr y
evalExpr (EMul x y) = (*) <$> evalExpr x <*> evalExpr y
evalExpr (EDiv x y) = (/) <$> evalExpr x <*> evalExpr y

evalExpr (ELet n b) = do
    b' <- evalExpr b
    modify $ Env . insert n b' . getEnv
    return b'

evalExpr (EVar n) = do
    env <- get
    case Data.Map.lookup n (getEnv env) of
        Nothing -> error $ "Unknown variable: " ++ unpack n
        Just b  -> return b

massage :: Text -> Maybe Text
massage txt =
    case T.takeWhile (/= '#') txt of
        "" -> Nothing
        x  -> Just x

evalText :: Text -> Text
evalText str =
    case massage str of
        Nothing -> ""
        Just input -> pack $ case parse expr "" input of
            Left e  -> show e
            Right x -> show (eval x)

test :: Text -> IO ()
test = T.putStrLn . evalText

testFile :: FilePath -> IO ()
testFile path = do
    str <- T.readFile path
    forM_ (T.lines str) test
    -- case parse (many expr <* eof) path (pack str) of
    --     Left e   -> error (show e)
    --     Right xs -> forM_ xs $ print . evalExpr

repl :: IO ()
repl = do
    _ <- flip execStateT newEnv $ forever $ do
        input <- liftIO T.getLine
        case parse expr "" input of
            Left e  -> liftIO $ print e
            Right x -> do
                a <- hoist (return . runIdentity) (evalExpr x)
                liftIO $ print a
    return ()
