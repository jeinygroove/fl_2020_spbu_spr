module Test.LLang where

import           AST
import           Combinators      (ErrorMsg (..), Position (..), InputStream (..), Parser (..), Result (..), runParser, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

assertP :: (Eq a, Show a) => Parser String String a -> String -> a -> Assertion
assertP p str a = case runParser p str of
                       Success (InputStream "" _) b | a == b -> True @?= True
                       Success (InputStream "" _) b | a /= b -> b @?= a
                       _ -> str @?= "Fail"

isFailure (Failure _) = True
isFailure  _          = False

unit_parseIf :: Assertion
unit_parseIf = do 
    let p = parseIf
    assertP p "If (x==0)(Seq { Write(0); }) (Seq {Write (42); })" (If {
       cond = BinOp Equal (Ident "x") (Num 0),
       thn  = Seq [Write (Num 0)],
       els  = Seq [Write (Num 42)]})
    assertP p "If (2 > 3) (Seq {Write (2 - 3);}) (Seq {Write (3 - 2);})" (If {
       cond = BinOp Gt (Num 2) (Num 3),
       thn  = Seq [Write (BinOp Minus (Num 2) (Num 3))],
       els  = Seq [Write (BinOp Minus (Num 3) (Num 2))]})
    assertBool "" $ isFailure $ runParser p "If () (Seq { }) (Seq { })"
    assertBool "" $ isFailure $ runParser p "If 0 (Seq { }) (Seq { })"
    assertBool "" $ isFailure $ runParser p "If (0) (Seq { })"
    assertBool "" $ isFailure $ runParser p "If (x)"

unit_parseWhile :: Assertion
unit_parseWhile = do
    let p = parseWhile
    assertP p "While (x > 0) (Seq {Write (x); Assign (x) (x - 1);})" (While {
       cond = BinOp Gt (Ident "x") (Num 0),
       body = Seq [Write (Ident "x"), Assign "x" (BinOp Minus (Ident "x") (Num 1))]})
    assertP p "While (x) (Seq {})" (While {
       cond = Ident "x",
       body = Seq []})
    assertBool "" $ isFailure $ runParser p "While () (Seq { })"
    assertBool "" $ isFailure $ runParser p "While 0 (Seq { })"
    assertBool "" $ isFailure $ runParser p "While (x)"

unit_parseAssign :: Assertion
unit_parseAssign = do
     let p = parseAssign
     assertP p "Assign (x) (y)" (Assign "x" (Ident "y"))
     assertP p "Assign (_123) (42/6) " (Assign "_123" (BinOp Div (Num 42) (Num 6)))
     assertBool "" $ isFailure $ runParser p "Assign () (Seq { })"
     assertBool "" $ isFailure $ runParser p "Assign _@34 (Seq { })"
     assertBool "" $ isFailure $ runParser p "Assign x"
     assertBool "" $ isFailure $ runParser p "Assign Assign"
     assertBool "" $ isFailure $ runParser p "Assign Seq"
     assertBool "" $ isFailure $ runParser p "Assignx"

unit_parseRead :: Assertion
unit_parseRead = do
     let p = parseRead
     assertP p "Read (x)" (Read "x")
     assertP p "Read (_123)" (Read "_123")
     assertBool "" $ isFailure $ runParser p "Read ("
     assertBool "" $ isFailure $ runParser p "Read 1write"
     assertBool "" $ isFailure $ runParser p "Read While"
     assertBool "" $ isFailure $ runParser p "Readn"

unit_parseWrite :: Assertion
unit_parseWrite = do
     let p = parseWrite
     assertP p "Write (x)" (Write (Ident "x"))
     assertP p "Write (2||3)" (Write (BinOp Or (Num 2) (Num 3)))
     assertP p "Write (x + y)" (Write (BinOp Plus (Ident "x") (Ident "y")))
     assertBool "" $ isFailure $ runParser p "Write 3"
     assertBool "" $ isFailure $ runParser p "Write ("

unit_parseSeq :: Assertion
unit_parseSeq = do
     let p = parseSeq
     assertP p "Seq {Write (x);}" (Seq [Write (Ident "x")])
     assertP p "Seq {}" (Seq [])
     assertP p "Seq {Write (x); Read (x);}" (Seq [Write (Ident "x"), Read "x"])
     assertBool "" $ isFailure $ runParser p "Seq {Write (x) }"
     assertBool "" $ isFailure $ runParser p "Seq {Seq }}"

unit_parseL :: Assertion
unit_parseL = do
     let p = parseL
     assertP p "Seq {}" (Seq [])
     assertP p "Seq {Read (n); Assign (i) (1); While (n > 0) (Seq {Assign (i) (n*i); Assign (n) (n-1);}); Write (i);}" (Seq [
        Read "n",
        Assign "i" (Num 1),
        While (BinOp Gt (Ident "n") (Num 0)) (Seq [Assign "i" (BinOp Mult (Ident "n") (Ident "i")), Assign "n" (BinOp Minus (Ident "n") (Num 1))]),
        Write (Ident "i")])
     assertP p "Seq {Read (n); Assign (i) (1); Assign (i) (n*i); Assign (n) (n - 1);}" (Seq [
        Read "n", 
        Assign "i" (Num 1), 
        Assign "i" (BinOp Mult (Ident "n") (Ident "i")),
        Assign "n" (BinOp Minus (Ident "n") (Num 1))])
     assertP p "Seq {Read (n); Assign (i) (1); While (n > 0) (Seq {Write (n);}); Write (i);}" (Seq [
        Read "n", Assign "i" (Num 1), While (BinOp Gt (Ident "n") (Num 0)) (Seq [Write (Ident "n")]), Write (Ident "i")])
     assertBool "" $ isFailure $ runParser p "{}"
     assertBool "" $ isFailure $ runParser p "Read 4"
     assertBool "" $ isFailure $ runParser p "Seq {Write (3); If (x > 0);}"
     assertBool "" $ isFailure $ runParser p "Seq {Assign Seq; Write Seq; }"
     assertBool "" $ isFailure $ runParser p "Seq {Write (x);}"
     assertBool "" $ isFailure $ runParser p "Seq {Assign (x) (x + 1);}"
     assertBool "" $ isFailure $ runParser p "Seq {Assign (i) (1); Assign (n) (n*i);}"

unit_parseDef :: Assertion
unit_parseDef = do
    let p = parseDef'
    assertP p "Def(_)()(Seq{ })Return(0)" (Function "_" [] (Seq []) (Num 0))
    assertP p "Def (func)(x)(Seq {}) Return (0)" (Function "func" ["x"] (Seq []) (Num 0))
    assertP p "Def (kek)   (a, b, c, d) (Seq {}) Return ( a * d  )"
      (Function "kek" ["a", "b", "c", "d"] (Seq []) (BinOp Mult (Ident "a") (Ident "d")))
    assertBool "" $ isFailure $ runParser parseDef "Def (Read) (2)"
    assertBool "" $ isFailure $ runParser parseDef "Def ()()(Seq{})"
    assertBool "" $ isFailure $ runParser parseDef "Def ()()"
    assertBool "" $ isFailure $ runParser parseDef "Def (name)"
    assertBool "" $ isFailure $ runParser parseDef "Def (name)(Seq {})"
    assertBool "" $ isFailure $ runParser parseDef "Def (name)(x)(Seq {Return (x + y);})"

unit_parseProg :: Assertion
unit_parseProg = do
    let p = parseProg
    assertP p "Def(_)()(Seq{}) Return (0) \n \t Seq{}" (Program [(Function "_" [] (Seq []) (Num 0))] (Seq []))
    assertP p "Def (foo)(x) \n (Seq {}) Return (x) \n\n  Def (bar)(x) (Seq { }) Return (foo(x)) \n \t Seq { Read (x); Write (foo(x) - bar(x)); }" (Program 
        [(Function "foo" ["x"] (Seq []) (Ident "x")),
         (Function "bar" ["x"] (Seq []) (FunctionCall "foo" [Ident "x"]))] 
        (Seq [Read "x", Write (BinOp Minus (FunctionCall "foo" [Ident "x"]) (FunctionCall "bar" [Ident "x"]))])
     )
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq { })"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Return (0) Def (g)(x, y) (Seq{}) Return (x + y) Seq{Write(func(x));}"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Return (0) Def (g)(x, y) (Seq{}) Return (x) Seq{Write(g(x));}"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Return (0) Def (g)(x, y) (Seq{ Return (f(x));}) Return (2) Seq{}"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Return (0) Def (g)(x, y) (Seq{}) Return (5 * 7) Seq{Write(h(x));}"

assertPos p prog l = case runParser parseProg prog of
                                Success (InputStream l' p') _ -> (l', p') @?= (l, p)
                                otherwise                           -> "" @?= "Fail" 

unit_progPosition :: Assertion
unit_progPosition = do
    assertPos (Position 4 0) "Seq {\n\tRead (n);\n\tWrite(n);\n}\n" ""
    assertPos (Position 3 0) "Seq {}\n\n\n" ""
    assertPos (Position 2 8) "Seq {}\n\n\t \t" ""


unit_errors :: Assertion
unit_errors = do
    case runParser parseStat "" of
      Success _ _  -> True @?= False
      Failure m    -> m @?= [ErrorMsg [
         "Expected \"If\"",
         "Expected \"While\"",
         "Expected \"Read\"",
         "Expected \"Write\"",
         "Expected \"Assign\"",
         "Expected \"Seq\""
        ] (Position 0 0)]
    case runParser parseStat " Seq {\n\tLol\n}\n" of
      Success _ _  -> True @?= False
      Failure m    -> m @?= [ErrorMsg [
         "Expected \"If\"",
         "Expected \"While\"",
         "Expected \"Read\"",
         "Expected \"Write\"",
         "Expected \"Assign\""
        ] (Position 0 1), ErrorMsg [
         "Expected \"}\""
        ] (Position 1 4)]
    case runParser parseStat "\n \n  \tRead (" of
      Success _ _  -> True @?= False
      Failure m    -> m @?= [ErrorMsg [
         "Expected \"If\"",
         "Expected \"While\"",
         "Expected \"Write\"",
         "Expected \"Assign\"",
         "Expected \"Seq\""
        ] (Position 2 4), ErrorMsg [
         "Expected ident",
         "Predicate failed",
         "Expected symbol: \'_\'"
        ] (Position 2 10)]
    case runParser parseStat "\tAssign (@) (2);" of
     Success _ _  -> True @?= False
     Failure m    -> m @?= [ErrorMsg [
         "Expected \"If\"",
         "Expected \"While\"",
         "Expected \"Read\"",
         "Expected \"Write\"",
         "Expected \"Seq\""
        ] (Position 0 4), ErrorMsg [
         "Expected ident",
         "Predicate failed",
         "Expected symbol: \'_\'"
        ] (Position 0 12)]
