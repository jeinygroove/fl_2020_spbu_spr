module LLang where

import AST (Subst (..), AST (..), Operator (..))
import Combinators (Parser (..), Result (..),
                             satisfy, success, sepBy1)
import Expr (parseExpr, evalExpr, toOperator, parseIdent)
import Control.Applicative
import Control.Monad (guard)
import Data.Maybe 
import qualified Data.Map as Map

type Expr = AST

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

type Var = String
data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read ("X")
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign ("X")
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

subset :: [Var] -> [Var] -> Bool
subset xs ys = all (\x -> x `elem` ys) xs

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

getVarsE :: Expr -> [Var]
getVarsE (BinOp _ a b) = getVarsE a ++ getVarsE b
getVarsE (UnaryOp _ e) = getVarsE e
getVarsE (Num _) = []
getVarsE (Ident x) = [x]

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
    parseString "Assign("
    var <- parseVar
    parseString ")("
    expr <- parseExpr
    parseString ")"
    return (Assign var expr)

parseRead :: Parser String String LAst
parseRead = do
    parseString "Read("
    var <- parseVar
    parseString ")"
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

checkCorrectness :: LAst -> Bool
checkCorrectness ast = isJust $ check ast [] 
  where check :: LAst -> [Var] -> Maybe [Var]
        check (If cond e1 e2) vars = if (subset (getVarsE cond) vars && isJust (check e1 vars) && isJust (check e2 vars)) then (Just vars) else Nothing
        check (While cond body) vars = if (subset (getVarsE cond) vars && isJust (check body vars)) then (Just vars) else Nothing
        check (Write expr) vars = if (subset (getVarsE expr) vars) then (Just vars) else Nothing
        check (Seq stmts) vars = foldl f (Just vars) stmts
               where f Nothing st = Nothing
                     f (Just vars) st = check st vars
        check (Read var) vars = Just $ if (var `elem` vars) then vars else (var:vars)
        check (Assign var expr) vars = if (subset (getVarsE expr) vars) then (Just (if (elem var vars) then vars else (var:vars))) else Nothing

parseL :: Parser String String LAst
parseL = Parser $ \input -> check $ runParser parseSeq (removeSpaces input)
            where check s@(Success _ ast) | True <- checkCorrectness ast = s
                                          | otherwise = Failure "Incorrect programm"
                  check f = f

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval (If cond thn els) c@(Conf subst inp outp) = do r <- evalExpr subst cond
                                                    if (r == 0) then eval els c else eval thn c
eval w@(While cond body) c@(Conf subst inp outp) = do r <- evalExpr subst cond
                                                      if (r == 0) then return c
                                                                  else do i <- eval body c
                                                                          eval w i
eval (Assign var expr) (Conf subst inp outp) = do r <- evalExpr subst expr
                                                  return $ Conf (Map.insert var r subst) inp outp 
eval (Read var) (Conf subst [] outp) = Nothing 
eval (Read var) (Conf subst (token:inp) outp) = return $ Conf (Map.insert var token subst) inp outp 
eval (Write expr) (Conf subst inp outp) = do r <- evalExpr subst expr
                                             return $ Conf subst inp (r:outp)
eval (Seq []) c = Just c  
eval (Seq (x:xs)) c = do r <- eval x c
                         eval (Seq xs) r  
