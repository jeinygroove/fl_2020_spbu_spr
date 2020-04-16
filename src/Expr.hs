module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Control.Applicative
import qualified Data.Map            as Map
import           Data.Char   (digitToInt, isLetter, isDigit)
import           Combinators (Parser (..), Result (..), elem', fail',
                              fmap', satisfy, some', symbol, sepBy1)

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

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [(or', Binary RightAssoc), (and', Binary RightAssoc), (not', Unary),
                      (eq' <|> neq' <|> ge' <|> le' <|> gt' <|> lt', Binary NoAssoc),
                      (plus' <|> minus', Binary LeftAssoc), (mult' <|> div', Binary LeftAssoc), (minus', Unary), (pow', Binary RightAssoc)]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp UnaryOp

symbols :: String -> Parser String String String
symbols [] = return []
symbols (x:xs) = (:) <$> satisfy (== x) <*> symbols xs

not'    = symbols "!" >>= toOperator
plus'   = symbols "+" >>= toOperator
minus'  = symbols "-" >>= toOperator
mult'   = symbols "*" >>= toOperator
pow'    = symbols "^" >>= toOperator
eq'     = symbols "==" >>= toOperator
neq'    = symbols "/=" >>= toOperator
ge'     = symbols ">=" >>= toOperator
le'     = symbols "<=" >>= toOperator
and'    = symbols "&&" >>= toOperator
or'     = symbols "||" >>= toOperator
gt'     = symbols ">" >>= toOperator
lt'     = symbols "<" >>= toOperator
div'    = symbols "/" >>= toOperator

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseNegNum :: Parser String String Int
parseNegNum = foldl f 0 <$> parser
  where
    parser :: Parser String String String
    parser = some (satisfy isDigit) <|> ((flip (++)) <$> many (symbol '-') <*> some (satisfy isDigit)) 
    f acc ('-') = -acc 
    f acc d     = (digitToInt d) + 10 * acc

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLetter <|> symbol '_')) <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = plus' <|> minus' <|> mult' <|> pow' <|> eq' <|> neq' <|> ge' <|> le' <|> and' <|> or' <|> gt' <|> lt' <|> div'

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "!"  = return Not
toOperator "+"  = return Plus
toOperator "*"  = return Mult
toOperator "-"  = return Minus
toOperator "/"  = return Div
toOperator "^"  = return Pow
toOperator "==" = return Equal
toOperator "/=" = return Nequal
toOperator ">"  = return Gt
toOperator ">=" = return Ge
toOperator "<=" = return Le
toOperator "<"  = return Lt
toOperator "&&" = return And
toOperator "||" = return Or
toOperator _    = fail' "Failed toOperator"

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
