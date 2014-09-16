{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Map
import Data.Monoid
import Data.Text as T
import Data.Text.IO as T
import Text.Parsec.Char
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim hiding (many, (<|>), State)
import Text.Parsec.Text
import Control.Monad.Morph (hoist)

data Expr
    = EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EPow Expr Expr
    | EDiv Expr Expr
    | ELet Text Expr
    | ELam Text (Env -> Double -> Double)
    | EApp Text Expr
    | EVar Text
    | ENum Double

instance Show Expr where
    show (EAdd x y) = show x <> " + " <> show y
    show (ESub x y) = show x <> " - " <> show y
    show (EMul x y) = show x <> " * " <> show y
    show (EPow x y) = show x <> " ^ " <> show y
    show (EDiv x y) = show x <> " / " <> show y
    show (ELet n b) = "let " <> unpack n <> " = " <> show b
    show (ELam n _) = "#<function " ++ unpack n ++ ">"
    show (EApp n x) = unpack n ++ "(" ++ show x ++ ")"
    show (EVar n)   = "<var " <> unpack n <> ">"
    show (ENum n)   = show n

whiteSpace :: Parser ()
whiteSpace = () <$ many (oneOf " \t")

symbol :: Text -> Parser ()
symbol str = () <$ (whiteSpace *> string (unpack str) <* whiteSpace)

num :: Parser Expr
num = ENum . read <$> go (oneOf "0123456789")
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
term = choice terms <* whiteSpace
  where
    terms = [ letBinding
            , try funBinding
            , try funCall
            , variable
            , try num
            ]

addop :: Parser (Expr -> Expr -> Expr)
addop = EAdd <$ symbol "+"
    <|> ESub <$ symbol "-"

mulop :: Parser (Expr -> Expr -> Expr)
mulop = EMul <$ symbol "*"
    <|> EPow <$ symbol "^"
    <|> EDiv <$ symbol "/"

expr :: Parser Expr
expr = subexpr `chainl1` addop

subexpr :: Parser Expr
subexpr = factor `chainl1` mulop

parenthesis :: Parser Expr -> Parser Expr
parenthesis = between (symbol "(") (symbol ")")

factor :: Parser Expr
factor = parenthesis expr <|> term

funCall :: Parser Expr
funCall = do
    name <- varname
    symbol "("
    arg  <- expr
    symbol ")"
    return $ EApp name arg

funBinding :: Parser Expr
funBinding = do
    symbol "define"
    name <- varname
    symbol "("
    arg  <- varname
    symbol ")"
    symbol "{"
    body <- sepBy1 expr (symbol ";")
    symbol "}"
    return $ ELam name $ \env x ->
        let env' = Env (insert arg (ENum x) (getEnv env))
        in runIdentity $ flip evalStateT env' $
            foldM (\_ b -> evalExpr b) 0.0 body

letBinding :: Parser Expr
letBinding = do
    symbol "let"
    name <- varname
    symbol "="
    body <- expr
    return $ ELet name body

newtype Env = Env { getEnv :: Map Text Expr }

newEnv :: Env
newEnv = Env $ Data.Map.singleton "sqrt" (ELam "sqrt" (const sqrt))

eval :: Expr -> Double
eval x = runIdentity $ evalStateT (evalExpr x) newEnv

evalExpr :: Expr -> State Env Double
evalExpr (ENum n)   = return n
evalExpr (EAdd x y) = (+) <$> evalExpr x <*> evalExpr y
evalExpr (ESub x y) = (-) <$> evalExpr x <*> evalExpr y
evalExpr (EMul x y) = (*) <$> evalExpr x <*> evalExpr y
evalExpr (EPow x y) = (**) <$> evalExpr x <*> evalExpr y
evalExpr (EDiv x y) = (/) <$> evalExpr x <*> evalExpr y

evalExpr (ELet n b) = do
    b' <- evalExpr b
    modify $ Env . insert n (ENum b') . getEnv
    evalExpr $ ENum b'

evalExpr (EVar n) = do
    env <- get
    case Data.Map.lookup n (getEnv env) of
        Nothing -> error $ "Unknown variable: " ++ unpack n
        Just b  -> evalExpr b

evalExpr f@(ELam n _) = do
    modify $ Env . insert n f . getEnv
    return 0

evalExpr (EApp n x) = do
    x' <- evalExpr x
    env <- get
    case Data.Map.lookup n (getEnv env) of
        Just (ELam _ f) -> return $ f env x'
        _ -> error $ "Unknown function: " ++ unpack n

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
    _ <- flip execStateT newEnv $
        forM_ (T.lines str) $ \input ->
            case massage input of
                Nothing -> return ()
                Just input' -> case parse (expr <* eof) path input' of
                    Left e  -> liftIO $ print e
                    Right x -> do
                        a <- hoist (return . runIdentity) (evalExpr x)
                        liftIO $ print a
    return ()

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

main = repl
