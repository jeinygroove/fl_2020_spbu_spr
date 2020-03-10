module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f p = Parser $ \input -> case runParser p input of
                                         Failure e       -> Failure e
                                         Success inp res -> Success inp (f res)    

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x
  (Parser p1) <*> (Parser p2) = Parser $ helper1 . p1 where
                                helper1 (Failure e) = Failure e
                                helper1 (Success i f) = case p2 i of 
                                                        Failure e -> Failure e
                                                        Success input result -> Success input (f result)
instance Monad (Parser error input) where
  return = pure
  (Parser p) >>= k = Parser $ (\i -> helper (p i)) where
                     helper (Failure e) = Failure e
                     helper (Success i r) = runParser (k r) i 

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \i -> Failure mempty
  (Parser p1) <|> (Parser p2) = Parser $ \i -> case p1 i of
                                Failure e -> p2 i
                                s -> s 

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (helper sep elem) where
  helper sep elem = (:) <$> (sep *> elem) <*> (helper sep elem) <|> pure []


-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    _       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure

-- Проверяет, что первый элемент входной последовательности -- данный символ
fmap' :: (a -> b) -> Parser e i a -> Parser e i b
fmap' f p = Parser $ \input ->
  case runParser p input of
    Success i a -> Success i (f a)
    Failure e   -> Failure e

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: Monoid e => Parser e i a -> Parser e i [a]
some' p = do
  a <- p
  as <- many' p
  return (a : as)

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: Monoid e => Parser e i a -> Parser e i [a]
many' p = some' p <|> return []
