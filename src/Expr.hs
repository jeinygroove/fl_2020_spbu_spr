module Expr where

import           AST         (AST (..), Operator (..))
import           Data.Char   (digitToInt, isLetter, isDigit)
import           Combinators (Parser (..), Result (..), elem', fail',
                              fmap', satisfy, some', symbol, sepBy1)
import           Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr [] p _ = p
uberExpr ((opParser, as):ps) p f = 
  let p' = uberExpr ps p f
  in case as of
    LeftAssoc -> do 
      (ini, xs) <- (,) <$> p' <*> (many ((,) <$> opParser <*> p'))
      return $ foldl (\rest (op, ast) -> f op rest ast) ini xs
    RightAssoc -> do
      (xs, ini) <- (,) <$> (many $ (,) <$> p' <*> opParser) <*> p'
      return $ foldr (\(ast, op) rest -> f op ast rest) ini xs
    NoAssoc -> (do 
      ast <- p'
      op <- opParser
      ast' <- p'
      return (f op ast ast')) <|> p'

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [(or', RightAssoc), (and', RightAssoc),
                      (eq' <|> neq' <|> ge' <|> le' <|> gt' <|> lt', NoAssoc),
                      (plus' <|> minus', LeftAssoc), (mult' <|> div', LeftAssoc), (pow', RightAssoc)]
                     (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
                     BinOp

symbols :: String -> Parser String String String
symbols [] = return []
symbols (x:xs) = (:) <$> satisfy (== x) <*> symbols xs
 
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
parseNum = Parser $ \input -> case input of 
                                ('-':xs) -> case runParser parseNum xs of
                                              Success i res -> Success i ((-1) * res)
                                              err -> err
                                otherwise -> runParser parser input
           where parser = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLetter <|> symbol '_')) <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = plus' <|> minus' <|> mult' <|> pow' <|> eq' <|> neq' <|> ge' <|> le' <|> and' <|> or' <|> gt' <|> lt' <|> div'

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
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
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)            = x
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Pow x y)    = compute x ^ compute y
compute (BinOp Equal x y)  = boolToInt $ compute x == compute y
compute (BinOp Nequal x y) = boolToInt $ compute x /= compute y
compute (BinOp Ge x y)     = boolToInt $ compute x >= compute y
compute (BinOp Le x y)     = boolToInt $ compute x <= compute y
compute (BinOp Gt x y)     = boolToInt $ compute x < compute y
compute (BinOp Lt x y)     = boolToInt $ compute x > compute y
compute (BinOp And x y)    = case compute x of
                                0 -> 0
                                _ -> compute y
compute (BinOp Or x y)     = case compute x of
                                0 -> compute y
                                x -> x

boolToInt :: Bool -> Int
boolToInt b = case b of
                True -> 1
                _ -> 0
