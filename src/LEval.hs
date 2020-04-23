module LEval where

import LLang (Program (..), Configuration (..), eval, Function (..), parseProg)
import Combinators (InputStream (..), Result (..), runParser)
import qualified Data.Map    as Map

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program functions main) input = eval main (Conf Map.empty input [] (Map.fromList (fmap (\f@(Function n _ _ _) -> (n, f)) functions)))

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg str input = case runParser parseProg str of
                                  Success (InputStream s p) prog -> evalProg prog input
