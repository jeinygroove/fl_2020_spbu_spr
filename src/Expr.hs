module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Control.Applicative
import           Data.Char   (digitToInt, isLetter, isDigit)
import           Combinators ( (<?>), Parser (..), runParser, stream, Result (..), fail',
                              word, success, satisfy, symbol, sepBy1)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr _ (Num x) = Just x
evalExpr subst (BinOp op x y) = do l <- evalExpr subst x 
                                   r <- evalExpr subst y
                                   return $ compute (BinOp op (Num l) (Num r))
evalExpr subst (UnaryOp op x) = do r <- evalExpr subst x 
                                   return $ compute (UnaryOp op (Num r))
evalExpr subst (Ident v) = Map.lookup v subst

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr [] p _ _ = p
uberExpr ((opParser, as):ps) p bf uf = 
  let p' = uberExpr ps p bf uf
  in (case as of
    Unary -> uf <$> opParser <*> p'
    Binary LeftAssoc -> do 
      (ini, xs) <- (,) <$> p' <*> (many ((,) <$> opParser <*> p'))
      return $ foldl (\rest (op, ast) -> bf op rest ast) ini xs
    Binary RightAssoc -> do
      (xs, ini) <- (,) <$> (many $ (,) <$> p' <*> opParser) <*> p'
      return $ foldr (\(ast, op) rest -> bf op ast rest) ini xs
    Binary NoAssoc -> do
      ast <- p'
      (\op -> bf op ast) <$> opParser <*> p'
    ) <|> p'

parseWithSep :: (Monoid e) => Parser e i r -> Parser e i a -> Parser e i [a]
parseWithSep s p = ((:) <$> p <*> many (s *> p)) <|> pure []

parseSep = many (symbol ' '<|> symbol '\n' <|> symbol '\t')

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = "Expected expression" <?> uberExpr [(or', Binary RightAssoc), (and', Binary RightAssoc), (not', Unary),
                      (eq' <|> neq' <|> ge' <|> le' <|> gt' <|> lt', Binary NoAssoc),
                      (plus' <|> minus', Binary LeftAssoc), (mult' <|> div', Binary LeftAssoc), (minus', Unary), (pow', Binary RightAssoc)]
                     (Num <$> parseNum <|> 
                      FunctionCall <$> parseIdent <* symbol '(' <*> parseWithSep (symbols ", ") parseExpr <* parseSep <* symbol ')' <* parseSep <|> 
                      Ident <$> parseIdent <|>
                       parseSep *> symbol '(' *> parseExpr <* symbol ')' <* parseSep)
                     BinOp UnaryOp

symbols :: String -> Parser String String String
symbols = word

not'    = Not <$ symbols "!"
plus'   = Plus <$ symbols "+"
minus'  = Minus <$ symbols "-"
mult'   = Mult <$ symbols "*"
pow'    = Pow <$ symbols "^"
eq'     = Equal <$ symbols "=="
neq'    = Nequal <$ symbols "/="
ge'     = Ge <$ symbols ">="
le'     = Le <$ symbols "<="
and'    = And <$ symbols "&&"
or'     = Or <$ symbols "||"
gt'     = Gt <$ symbols ">"
lt'     = Lt <$ symbols "<"
div'    = Div <$ symbols "/"

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = "Expected number" <?> parseSep *> (foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go) <* parseSep
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseNegNum :: Parser String String Int
parseNegNum = "Expected negative number" <?> parseSep *> (foldl f 0 <$> parser) <* parseSep
  where
    parser :: Parser String String String
    parser = some (satisfy isDigit) <|> ((flip (++)) <$> many (symbol '-') <*> some (satisfy isDigit)) 
    f acc ('-') = -acc 
    f acc d     = (digitToInt d) + 10 * acc

parseIdent :: Parser String String String
parseIdent = "Expected ident" <?> parseSep *> ((:) <$> (satisfy isLetter <|> symbol '_') <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')) <* parseSep

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null (stream rest) -> return $ compute ast
    _                                     -> Nothing

compute :: AST -> Int
compute (Num x)            = x
compute (UnaryOp Minus x)  = -(compute x)
compute (UnaryOp Not x)    = fromEnum $ compute x == 0
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Pow x y)    = compute x ^ compute y
compute (BinOp Equal x y)  = fromEnum $ compute x == compute y
compute (BinOp Nequal x y) = fromEnum $ compute x /= compute y
compute (BinOp Ge x y)     = fromEnum $ compute x >= compute y
compute (BinOp Le x y)     = fromEnum $ compute x <= compute y
compute (BinOp Gt x y)     = fromEnum $ compute x > compute y
compute (BinOp Lt x y)     = fromEnum $ compute x < compute y
compute (BinOp Or x y)     = fromEnum $ (compute x /= 0) || (compute y /= 0)
compute (BinOp And x y)    = fromEnum $ (compute x /= 0) && (compute y /= 0)
