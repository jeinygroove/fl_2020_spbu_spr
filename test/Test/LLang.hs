module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser)
import           LLang
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_parseIf :: Assertion
unit_parseIf = do 
    let p = Parser $ (runParser parseIf) . removeSpaces
    runParser p "If (x==0)(Seq { Write(0); }) (Seq {Write (42); })" @?= Success "" (If {
       cond = BinOp Equal (Ident "x") (Num 0),
       thn  = Seq [Write (Num 0)],
       els  = Seq [Write (Num 42)]})
    runParser p "If (2 > 3) (Seq {Write (2 - 3);}) (Seq {Write (3 - 2);})" @?= Success "" (If {
       cond = BinOp Gt (Num 2) (Num 3),
       thn  = Seq [Write (BinOp Minus (Num 2) (Num 3))],
       els  = Seq [Write (BinOp Minus (Num 3) (Num 2))]})
    assertBool "" $ isFailure $ runParser p "If () (Seq { }) (Seq { })"
    assertBool "" $ isFailure $ runParser p "If 0 (Seq { }) (Seq { })"
    assertBool "" $ isFailure $ runParser p "If (0) (Seq { })"
    assertBool "" $ isFailure $ runParser p "If (x)"

unit_parseWhile :: Assertion
unit_parseWhile = do
    let p = Parser $ (runParser parseWhile) . removeSpaces
    runParser p "While (x > 0) (Seq {Write (x); Assign x (x - 1);})" @?= Success "" (While {
       cond = BinOp Gt (Ident "x") (Num 0),
       body = Seq [Write (Ident "x"), Assign "x" (BinOp Minus (Ident "x") (Num 1))]})
    runParser p "While (x) (Seq {})" @?= Success "" (While {
       cond = Ident "x",
       body = Seq []})
    assertBool "" $ isFailure $ runParser p "While () (Seq { })"
    assertBool "" $ isFailure $ runParser p "While 0 (Seq { })"
    assertBool "" $ isFailure $ runParser p "While (x)"

unit_parseAssign :: Assertion
unit_parseAssign = do
     let p = Parser $ (runParser parseAssign) . removeSpaces
     runParser p "Assign x (y)" @?= Success "" (Assign "x" (Ident "y"))
     runParser p "Assign _123 (42/6) " @?= Success "" (Assign "_123" (BinOp Div (Num 42) (Num 6)))
     assertBool "" $ isFailure $ runParser p "Assign () (Seq { })"
     assertBool "" $ isFailure $ runParser p "Assign _@34 (Seq { })"
     assertBool "" $ isFailure $ runParser p "Assign x"

unit_parseRead :: Assertion
unit_parseRead = do
     let p = Parser $ (runParser parseRead) . removeSpaces
     runParser p "Read x" @?= Success "" (Read "x")
     runParser p "Read _123" @?= Success "" (Read "_123")
     assertBool "" $ isFailure $ runParser p "Read ("
     assertBool "" $ isFailure $ runParser p "Read 1write"

unit_parseWrite :: Assertion
unit_parseWrite = do
     let p = Parser $ (runParser parseWrite) . removeSpaces
     runParser p "Write (x)" @?= Success "" (Write (Ident "x"))
     runParser p "Write (2||3)" @?= Success "" (Write (BinOp Or (Num 2) (Num 3)))
     runParser p "Write (x + y)" @?= Success "" (Write (BinOp Plus (Ident "x") (Ident "y")))
     assertBool "" $ isFailure $ runParser p "Write 3"
     assertBool "" $ isFailure $ runParser p "Write ("

unit_parseSeq :: Assertion
unit_parseSeq = do
     let p = Parser $ (runParser parseSeq) . removeSpaces
     runParser p "Seq {Write (x);}" @?= Success "" (Seq [Write (Ident "x")])
     runParser p "Seq {}" @?= Success "" (Seq [])
     runParser p "Seq {Write (x); Read x;}" @?= Success "" (Seq [Write (Ident "x"), Read "x"])
     assertBool "" $ isFailure $ runParser p "Seq {Write (x) }"
     assertBool "" $ isFailure $ runParser p "Seq {Seq }}"

unit_parseProg :: Assertion
unit_parseProg = do
     let p = Parser $ (runParser parseProg) . removeSpaces
     runParser p "Seq {}" @?= Success "" (Seq [])
     runParser p "Seq {Read n; Assign i (1); While (n > 0) (Seq {Assign i (n*i); Assign n (n-1);}); Write (i);}" @?= Success "" (Seq [
        Read "n",
        Assign "i" (Num 1),
        While (BinOp Gt (Ident "n") (Num 0)) (Seq [Assign "i" (BinOp Mult (Ident "n") (Ident "i")), Assign "n" (BinOp Minus (Ident "n") (Num 1))]),
        Write (Ident "i")])
     assertBool "" $ isFailure $ runParser p "{}"
     assertBool "" $ isFailure $ runParser p "Read 4"
     assertBool "" $ isFailure $ runParser p "Seq {Write (3); If (x > 0);}"

