module LLang where

import AST (AST (..), Operator (..))
import Combinators (satisfy, Parser (..))
import Expr (parseExpr, toOperator, parseIdent)
import Control.Applicative
import Control.Monad (guard)

type Expr = AST

type Var = String
data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

removeSpaces :: String -> String
removeSpaces = concat . words

symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

parseString :: String -> Parser String String String
parseString str = foldr (\ch rest -> (:) <$> symbol ch <*> rest) (pure "") str

keywords = ["If", "While", "Read", "Assign", "Write", "Seq"]

parseVar :: Parser String String String
parseVar = do
    var <- parseIdent
    guard (not (elem var keywords))
    return var

parseIf :: Parser String String LAst
parseIf = do 
    parseString "If("
    cond <- parseExpr
    parseString ")("
    b1 <- parseSeq
    parseString ")("
    b2 <- parseSeq
    parseString ")"
    return (If cond b1 b2)

parseWhile :: Parser String String LAst
parseWhile = do
    parseString "While("
    cond <- parseExpr
    parseString ")("
    body <- parseSeq
    parseString ")"
    return (While cond body)

parseAssign :: Parser String String LAst
parseAssign = do
    parseString "Assign"
    var <- parseVar
    parseString "("
    expr <- parseExpr
    parseString ")"
    return (Assign var expr)

parseRead :: Parser String String LAst
parseRead = do
    parseString "Read"
    var <- parseVar
    return (Read var)

parseWrite :: Parser String String LAst
parseWrite = do
    parseString "Write("
    expr <- parseExpr
    parseString ")"
    return (Write expr)

parseSeq :: Parser String String LAst
parseSeq = do
    parseString "Seq{"
    statement <- many (parseStat <* parseString ";")
    parseString "}"
    return (Seq statement)

parseStat :: Parser String String LAst
parseStat = parseIf <|> parseWhile <|> parseRead <|> parseWrite <|> parseAssign <|> parseSeq

parseProg :: Parser String String LAst
parseProg = Parser $ \input -> runParser parseSeq (removeSpaces input)
    
    

