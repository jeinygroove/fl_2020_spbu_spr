module LLang where

import AST (Subst (..), AST (..), Operator (..))
import Combinators (Position, runParser, Parser (..), Result (..), InputStream (..),
                    word, curPos, ErrorMsg (..), satisfy, success, sepBy1, makeError)
import Expr (parseSep, parseExpr, parseWithSep, parseIdent)
import Control.Applicative
import Control.Monad (guard)
import           Text.Printf (printf)
import           Data.List   (intercalate)
import Data.Maybe 
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs } deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
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

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (\x -> x `elem` ys) xs

symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

parseString :: String -> Parser String String String
parseString = word

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
    parseSep
    parseString "If" <* parseSep
    parseString "("
    cond <- parseExpr
    parseString ")" <* parseSep
    parseString "("
    b1 <- parseSeq
    parseString ")" <* parseSep
    parseString "("
    b2 <- parseSeq
    parseString ")" <* parseSep
    return (If cond b1 b2)

parseWhile :: Parser String String LAst
parseWhile = do
    parseSep
    parseString "While" <* parseSep
    parseString "("
    cond <- parseExpr
    parseString ")" <* parseSep 
    parseString "("
    body <- parseSeq
    parseString ")" <* parseSep
    return (While cond body)

parseAssign :: Parser String String LAst
parseAssign = do
    parseSep
    parseString "Assign" <* parseSep
    parseString "("
    var <- parseVar
    parseString ")" <* parseSep
    parseString "("
    expr <- parseExpr
    parseString ")" <* parseSep
    return (Assign var expr)

parseRead :: Parser String String LAst
parseRead = do
    parseSep
    parseString "Read" <* parseSep
    parseString "("
    var <- parseVar
    parseString ")" <* parseSep
    return (Read var)

parseWrite :: Parser String String LAst
parseWrite = do
    parseSep
    parseString "Write" <* parseSep
    parseString "("
    expr <- parseExpr
    parseString ")" <* parseSep
    return (Write expr)

parseSeq :: Parser String String LAst
parseSeq = do
    parseSep
    parseString "Seq" <* parseSep
    parseString "{" <* parseSep
    statement <- many $ parseStat <* (parseSep *> parseString ";" <* parseSep)
    parseString "}" <* parseSep
    return (Seq statement)

parseReturn :: Parser String String Expr
parseReturn = do
    parseSep
    parseString "Return" <* parseSep
    parseString "("
    expr <- parseExpr
    parseString ")" <* parseSep
    return expr

parseStat :: Parser String String LAst
parseStat = parseIf <|> parseWhile <|> parseRead <|> parseWrite <|> parseAssign <|> parseSeq

checkCorrectnessV :: [Var] -> LAst -> Expr -> Bool
checkCorrectnessV vars ast ret =  case check ast vars of 
                                       Just bodyVars -> subset (getVarsE ret) (vars ++ bodyVars)
                                       Nothing -> False 
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
parseL = Parser $ \(InputStream stream c) -> let check s@(Success _ ast) | True <- checkCorrectnessV [] ast (Num 0) = s
                                                                         | otherwise = Failure [makeError "Incorrect programm" c] 
                                                 check f = f in check $ runParser' parseSeq (InputStream stream c)

parseDef' :: Parser String String Function
parseDef' = do
    parseSep
    parseString "Def" <* parseSep
    parseString "("
    name <- parseIdent
    parseString ")" <* parseSep
    parseString "("
    args <- (parseWithSep (parseSep *> parseString "," <* parseSep) parseIdent) <* parseSep
    parseString ")" <* parseSep
    parseString "("
    body <- parseSeq
    parseString ")"
    ret  <- parseReturn
    return (Function name args body ret)

parseDef :: Parser String String Function
parseDef = Parser $ \(InputStream stream c) -> let check s@(Success _ (Function _ vars body ret)) | True <- checkCorrectnessV vars body ret  = s
                                                                                             | otherwise = Failure [makeError "Incorrect function definition" c]
                                                   check f = f in check $ runParser' parseDef' (InputStream stream c)


parseProg' :: Parser String String Program
parseProg' = do
    functions <- many parseDef
    main <- parseL
    return (Program functions main)

getCallsE :: Expr -> [(String, Int)]
getCallsE (FunctionCall n args) = (n, length args):(concat $ fmap (\e -> getCallsE e) args)
getCallsE (BinOp _ e1 e2) = getCallsE e1 ++ getCallsE e2
getCallsE (UnaryOp _ e) = getCallsE e
getCallsE _ = [] 

getCallsL :: LAst -> [(String, Int)]
getCallsL (If cond e1 e2) = getCallsE cond ++ getCallsL e1 ++ getCallsL e2
getCallsL (While cond body) = getCallsE cond ++ getCallsL body
getCallsL (Write e) = getCallsE e
getCallsL (Assign _ e) = getCallsE e
getCallsL (Seq stmts) = concat $ fmap (\last -> getCallsL last) stmts
getCallsL _ = []

getCalls :: Program -> [(String, Int)]
getCalls (Program functions main) = getCallsL main ++ foldr (\(Function _ _ body ret) calls -> getCallsL body ++ getCallsE ret ++ calls) [] functions 

getDefs :: Program -> [(String, Int)]
getDefs (Program functions _) = foldr (\(Function n args _ _) defs -> (n, length args):defs) [] functions

parseProg :: Parser String String Program
parseProg = Parser $ \(InputStream stream c) -> let check s@(Success _ program) | True <- getCalls program `subset` getDefs program  = s
                                                                                | otherwise = Failure [makeError "Incorrect function definition, you call function that doesn't exist" c]
                                                    check f = f in check $ runParser' parseProg' (InputStream stream c)

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input [] Map.empty

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) c@(Conf subst _ _ defs) = do (Conf _ i o _, x) <- evalExpr c cond
                                                    let c' = Conf subst i o defs
                                                    if (x == 0) then eval els c' else eval thn c'
                                                    
eval w@(While cond body) c@(Conf subst _ _ defs) = do (Conf _ i o _, x) <- evalExpr c cond
                                                      if (x == 0) then return (Conf subst i o defs)
                                                                  else do i <- eval body (Conf subst i o defs)
                                                                          eval w i
eval (Assign var expr) c@(Conf subst inp outp defs) = do (Conf _ i o _, x) <- evalExpr c expr
                                                         return $ Conf (Map.insert var x subst) i o defs 
eval (Read var) (Conf subst [] outp defs) = Nothing 
eval (Read var) (Conf subst (token:inp) outp defs) = return $ Conf (Map.insert var token subst) inp outp defs 
eval (Write expr) c@(Conf subst inp outp defs) = do (Conf _ i o _, x) <- evalExpr c expr
                                                    return $ Conf subst i (x:o) defs
eval (Seq stmts) c = foldl (\conf e -> conf >>= (eval e)) (Just c) stmts

evalFunc :: Function -> Configuration -> [Int] -> Maybe (Configuration, Int)
evalFunc (Function name args (Seq stmts) ret) (Conf subst inp outp defs) argVals = do (Conf _ i (x:o) _) <- eval (Seq $ stmts ++ [Write ret]) (Conf (Map.fromList (zip args argVals)) inp outp defs)
                                                                                      return (Conf subst i o defs, x)
evalFunc (Function name args instr ret) c argVals = evalFunc (Function name args (Seq [instr]) ret) c argVals

evalExpr :: Configuration -> Expr -> Maybe (Configuration, Int)
evalExpr c (Num x)                      = Just (c, x)
evalExpr c@(Conf subst _ _ _) (Ident x) = (,) c <$> Map.lookup x subst
evalExpr c (UnaryOp Minus x)            = (fmap (*(-1))) <$> evalExpr c x
evalExpr c (UnaryOp Not x)              = (fmap (fromEnum . (==0))) <$> evalExpr c x
evalExpr c (BinOp Plus x y)             = do (new_c, r) <- evalExpr c x
                                             (fmap $ (+) r) <$> evalExpr new_c y
evalExpr c (BinOp Mult x y)             = do (new_c, r) <- evalExpr c x
                                             (fmap $ (*) r) <$> evalExpr new_c y
evalExpr c (BinOp Minus x y)            = do (new_c, r) <- evalExpr c x
                                             (fmap $ (-) r) <$> evalExpr new_c y
evalExpr c (BinOp Div x y)              = do (new_c, r) <- evalExpr c x
                                             (fmap $ div r) <$> evalExpr new_c y
evalExpr c (BinOp Pow x y)              = do (new_c, r) <- evalExpr c x 
                                             (fmap $ (^) r) <$> evalExpr new_c y
evalExpr c (BinOp Equal x y)            = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum (r == r'))) <$> evalExpr new_c y
evalExpr c (BinOp Nequal x y)           = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum (r /= r'))) <$> evalExpr new_c y
evalExpr c (BinOp Ge x y)               = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum (r >= r'))) <$> evalExpr new_c y
evalExpr c (BinOp Gt x y)               = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum (r > r'))) <$> evalExpr new_c y
evalExpr c (BinOp Le x y)               = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum (r <= r'))) <$> evalExpr new_c y
evalExpr c (BinOp Lt x y)               = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum (r < r'))) <$> evalExpr new_c y
evalExpr c (BinOp Or x y)               = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum ((r/=0) || (r'/=0)))) <$> evalExpr new_c y
evalExpr c (BinOp And x y)              = do (new_c, r) <- evalExpr c x
                                             (fmap (\r' -> fromEnum ((r/=0) && (r'/=0)))) <$> evalExpr new_c y
evalExpr c@(Conf _ _ _ defs) (FunctionCall f args) = do (conf@(Conf subst _ _ defs'), x) <- foldl (\p e -> do {(c', d) <- p; (new_c, r) <- evalExpr c' e; return (new_c, d ++ [r])}) (Just (c, [])) args
                                                        f <- Map.lookup f defs
                                                        ((Conf _ i o _), new_x) <- evalFunc f conf x
                                                        return (Conf subst i o defs', new_x)
                                                                                                  
instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

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
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
