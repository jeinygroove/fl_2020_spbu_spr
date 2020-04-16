module LLang where

import AST (Subst (..), AST (..), Operator (..))
import Combinators (Position, runParser, Parser (..), Result (..), InputStream (..),
                    curPos, ErrorMsg (..), satisfy, success, sepBy1, makeError)
import Expr (parseExpr, evalExpr, toOperator, parseIdent)
import Control.Applicative
import Control.Monad (guard)
import           Text.Printf (printf)
import           Data.List   (intercalate)
import Data.Maybe 
import qualified Data.Map as Map

type Expr = AST

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
  deriving (Eq)

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

keywords = ["If", "While", "Read", "Assign", "Write", "Seq", "Def", "Return"]

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
getVarsE (FunctionCall _ xs) = concat $ fmap (\e -> getVarsE e) xs

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

parseReturn :: Parser String String LAst
parseReturn = do
    parseString "Return("
    expr <- parseExpr
    parseString ")"
    return (Return expr)

parseFuncIf :: Parser String String LAst
parseFuncIf = do
     parseString "If("
     cond <- parseExpr
     parseString ")("
     b1 <- parseFuncSeq
     parseString ")("
     b2 <- parseFuncSeq
     parseString ")"
     return (If cond b1 b2)

parseFuncWhile :: Parser String String LAst
parseFuncWhile = do
     parseString "While("
     cond <- parseExpr
     parseString ")("
     body <- parseFuncSeq
     parseString ")"
     return (While cond body)

parseFuncSeq :: Parser String String LAst
parseFuncSeq = do
     parseString "Seq{"
     statement <- many (parseFuncStat <* parseString ";")
     parseString "}"
     return (Seq statement)
     
parseFuncStat :: Parser String String LAst
parseFuncStat = do parseIf <|> parseWhile <|> parseRead <|> parseWrite <|> parseAssign <|> parseSeq <|> parseReturn

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
        check (Return expr) vars = if (subset (getVarsE expr) vars) then (Just vars) else Nothing

parseL :: Parser String String LAst
parseL = Parser $ \(InputStream stream c) -> let check s@(Success _ ast) | True <- checkCorrectness ast = s
                                                                         | otherwise = Failure [makeError "Incorrect programm" c] 
                                                 check f = f in check $ runParser parseSeq (removeSpaces stream)

defaultReturn :: LAst -> LAst
defaultReturn (Seq stats) = Seq (stats ++ [Return (Num 0)])

parseArgs :: Parser String String [Var]
parseArgs = do
            x <- parseIdent
            xs <- many (symbol ',' *> parseIdent)
            return (x:xs)

parseDef :: Parser String String Function
parseDef = do
    parseString "Def("
    name <- parseIdent
    parseString ")("
    args <- parseArgs <|> return []
    parseString ")("
    body <- parseFuncSeq
    parseString ")"
    return (Function name args (defaultReturn body))

parseProg :: Parser String String Program
parseProg = do
    functions <- many parseDef
    main <- parseL
    return (Program functions main)

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

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

instance Eq Function where
  (==) a b = (show a) == (show b)

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Eq Program where
  (==) a b = (show a) == (show b)
  
instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
