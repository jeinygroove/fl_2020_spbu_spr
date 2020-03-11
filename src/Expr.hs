module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              fmap', satisfy, some', symbol, sepBy1)
import           Data.Char   (digitToInt, isDigit)
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
      rightParser = ((\x g y -> f g x y) <$> p' <*> opParser <*> rightParser) <|> p'
      noAssocParser = ((\x g y -> f g x y) <$> p' <*> opParser <*> p') <|> p'
      leftParser = p' >>= parsing
      parsing left = (do
        op <- opParser
        right <- p'
        parsing $ f op left right) <|> return left
  in case as of
    LeftAssoc -> leftParser
    RightAssoc -> rightParser
    NoAssoc -> noAssocParser

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = parseSum

parseMult :: Parser String String AST
parseMult = let
    mult = symbol '*' >>= toOperator
    div = symbol '/' >>= toOperator
    parser = (Num <$> parseNum <|> symbol '(' *> parseSum <* symbol ')')
  in uberExpr [(mult, LeftAssoc), (div, LeftAssoc)] parser BinOp

 
parseSum :: Parser String String AST
parseSum = let
    plus = symbol '+' >>= toOperator
    minus = symbol '-' >>= toOperator
  in uberExpr [(plus, LeftAssoc), (minus, LeftAssoc)] parseMult BinOp

-- Парсер для натуральных чисел с 0
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> some (satisfy isDigit)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y

