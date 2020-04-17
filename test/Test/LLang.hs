module Test.LLang where

import           AST
import           Combinators      (InputStream (..), Parser (..), Result (..), runParser, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

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
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
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
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
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
         (Seq [(Write (Num 1))])
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

assertP :: (Eq a, Show a) => Parser String String a -> String -> a -> Assertion
assertP p str a = do runParser p str @?= Success (toStream "" (length $ removeSpaces str)) a 

isFailure (Failure _) = True
isFailure  _          = False

unit_parseIf :: Assertion
unit_parseIf = do 
    let p = Parser $ \(InputStream str p) -> runParser parseIf (removeSpaces str)
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
    let p = Parser $ \(InputStream str p) -> runParser parseWhile (removeSpaces str)
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
     let p = Parser $ \(InputStream str p) -> runParser parseAssign (removeSpaces str)
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
     let p = Parser $ \(InputStream str p) -> runParser parseRead (removeSpaces str)
     assertP p "Read (x)" (Read "x")
     assertP p "Read (_123)" (Read "_123")
     assertBool "" $ isFailure $ runParser p "Read ("
     assertBool "" $ isFailure $ runParser p "Read 1write"
     assertBool "" $ isFailure $ runParser p "Read While"
     assertBool "" $ isFailure $ runParser p "Readn"

unit_parseWrite :: Assertion
unit_parseWrite = do
     let p = Parser $ \(InputStream str p) -> runParser parseWrite (removeSpaces str)
     assertP p "Write (x)" (Write (Ident "x"))
     assertP p "Write (2||3)" (Write (BinOp Or (Num 2) (Num 3)))
     assertP p "Write (x + y)" (Write (BinOp Plus (Ident "x") (Ident "y")))
     assertBool "" $ isFailure $ runParser p "Write 3"
     assertBool "" $ isFailure $ runParser p "Write ("

unit_parseSeq :: Assertion
unit_parseSeq = do
     let p = Parser $ \(InputStream str p) -> runParser parseSeq (removeSpaces str)
     assertP p "Seq {Write (x);}" (Seq [Write (Ident "x")])
     assertP p "Seq {}" (Seq [])
     assertP p "Seq {Write (x); Read (x);}" (Seq [Write (Ident "x"), Read "x"])
     assertBool "" $ isFailure $ runParser p "Seq {Write (x) }"
     assertBool "" $ isFailure $ runParser p "Seq {Seq }}"

unit_parseL :: Assertion
unit_parseL = do
     let p = Parser $ \(InputStream str p) -> runParser parseL (removeSpaces str)
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
    let p = Parser $ \(InputStream str p) -> runParser parseDef (removeSpaces str)
    assertP p "Def ( _ )()(Seq { })" (Function "_" [] (Seq [Return (Num 0)]))
    assertP p "Def (func)(x)(Seq {})" (Function "func" ["x"] (Seq [Return (Num 0)]))
    assertP p "Def (kek)   (a, b, c, d) (Seq { Return ( a * d  ); })"
      (Function "kek" ["a", "b", "c", "d"] (Seq [Return (BinOp Mult (Ident "a") (Ident "d")), Return (Num 0)]))
    assertBool "" $ isFailure $ runParser parseDef "Def (Read) (2)"
    assertBool "" $ isFailure $ runParser parseDef "Def ()()(Seq{})"
    assertBool "" $ isFailure $ runParser parseDef "Def ()()"
    assertBool "" $ isFailure $ runParser parseDef "Def (name)"
    assertBool "" $ isFailure $ runParser parseDef "Def (name)(Seq {})"
    assertBool "" $ isFailure $ runParser parseDef "Def (name)(x)(Seq {Return (x + y);})"

unit_parseProg :: Assertion
unit_parseProg = do
    let p = Parser $ \(InputStream str p) -> runParser parseProg (removeSpaces str)
    assertP p "Def(_)()(Seq{})Seq{}" (Program [(Function "_" [] (Seq [Return (Num 0)]))] (Seq []))
    assertP p "Def (foo)(x) (Seq { Return (x); }) Def (bar)(x) (Seq { Return (foo(x)); }) Seq { Read (x); Write (foo(x) - bar(x)); }" (Program 
        [(Function "foo" ["x"] (Seq [Return (Ident "x"), Return (Num 0)])),
         (Function "bar" ["x"] (Seq [Return (FunctionCall "foo" [Ident "x"]), Return (Num 0)]))] 
        (Seq [Read "x", Write (BinOp Minus (FunctionCall "foo" [Ident "x"]) (FunctionCall "bar" [Ident "x"]))])
     )
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq { })"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Def (g)(x, y) (Seq{}) Seq{Write(func(x));}"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Def (g)(x, y) (Seq{}) Seq{Write(g(x));}"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Def (g)(x, y) (Seq{ Return (f(x));}) Seq{}"
    assertBool "" $ isFailure $ runParser parseProg "Def (func)() (Seq {}) Def (g)(x, y) (Seq{}) Seq{Write(h(x));}"
