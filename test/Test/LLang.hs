module Test.LLang where

import           AST              (AST (..), Operator (..))
import           Combinators      (Parser (..), Result (..), runParser)
import qualified Data.Map         as Map
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           LLang

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing

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
    runParser p "While (x > 0) (Seq {Write (x); Assign (x) (x - 1);})" @?= Success "" (While {
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
     runParser p "Assign (x) (y)" @?= Success "" (Assign "x" (Ident "y"))
     runParser p "Assign (_123) (42/6) " @?= Success "" (Assign "_123" (BinOp Div (Num 42) (Num 6)))
     assertBool "" $ isFailure $ runParser p "Assign () (Seq { })"
     assertBool "" $ isFailure $ runParser p "Assign _@34 (Seq { })"
     assertBool "" $ isFailure $ runParser p "Assign x"
     assertBool "" $ isFailure $ runParser p "Assign Assign"
     assertBool "" $ isFailure $ runParser p "Assign Seq"
     assertBool "" $ isFailure $ runParser p "Assignx"

unit_parseRead :: Assertion
unit_parseRead = do
     let p = Parser $ (runParser parseRead) . removeSpaces
     runParser p "Read (x)" @?= Success "" (Read "x")
     runParser p "Read (_123)" @?= Success "" (Read "_123")
     assertBool "" $ isFailure $ runParser p "Read ("
     assertBool "" $ isFailure $ runParser p "Read 1write"
     assertBool "" $ isFailure $ runParser p "Read While"
     assertBool "" $ isFailure $ runParser p "Readn"

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
     runParser p "Seq {Write (x); Read (x);}" @?= Success "" (Seq [Write (Ident "x"), Read "x"])
     assertBool "" $ isFailure $ runParser p "Seq {Write (x) }"
     assertBool "" $ isFailure $ runParser p "Seq {Seq }}"

unit_parseL :: Assertion
unit_parseL = do
     let p = Parser $ (runParser parseL) . removeSpaces
     runParser p "Seq {}" @?= Success "" (Seq [])
     runParser p "Seq {Read (n); Assign (i) (1); While (n > 0) (Seq {Assign (i) (n*i); Assign (n) (n-1);}); Write (i);}" @?= Success "" (Seq [
        Read "n",
        Assign "i" (Num 1),
        While (BinOp Gt (Ident "n") (Num 0)) (Seq [Assign "i" (BinOp Mult (Ident "n") (Ident "i")), Assign "n" (BinOp Minus (Ident "n") (Num 1))]),
        Write (Ident "i")])
     runParser p "Seq {Read (n); Assign (i) (1); Assign (i) (n*i); Assign (n) (n - 1);}" @?= Success "" (Seq [
        Read "n", 
        Assign "i" (Num 1), 
        Assign "i" (BinOp Mult (Ident "n") (Ident "i")),
        Assign "n" (BinOp Minus (Ident "n") (Num 1))])
     runParser p "Seq {Read (n); Assign (i) (1); While (n > 0) (Seq {Write (n);}); Write (i);}" @?= Success "" (Seq [
        Read "n", Assign "i" (Num 1), While (BinOp Gt (Ident "n") (Num 0)) (Seq [Write (Ident "n")]), Write (Ident "i")])
     assertBool "" $ isFailure $ runParser p "{}"
     assertBool "" $ isFailure $ runParser p "Read 4"
     assertBool "" $ isFailure $ runParser p "Seq {Write (3); If (x > 0);}"
     assertBool "" $ isFailure $ runParser p "Seq {Assign Seq; Write Seq; }"
     assertBool "" $ isFailure $ runParser p "Seq {Write (x);}"
     assertBool "" $ isFailure $ runParser p "Seq {Assign (x) (x + 1);}"
     assertBool "" $ isFailure $ runParser p "Seq {Assign (i) (1); Assign (n) (n*i);}"
