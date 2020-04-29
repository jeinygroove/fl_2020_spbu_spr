# Фидбек для Данила Аргунова

## Программа 1: Вычисление НОД
`func GCD(a, b) {
   if (a<b) {
     assign c (a);
     assign a (b);
     assign b (c);}
   else {};
   while (b) {
     assign c (b);
     assign b (a-b*(a/b));
     assign a (c);};
   } retrun(a);
  {read(a);
   read(b); 
   write(GCD(a, b));
  }`

`Program [ Function "GCD" ["a", "b"] (Seq [ If (BinOp Lt (Ident "a") (Ident "b")) 
                                            (Seq [Assign "c" (Ident "a"),
                                                  Assign "a" (Ident "b"),
                                                  Assign "b" (Ident "c")])
                                            (Seq []),
                                            While (Ident "b") (Seq [Assign "c" (Ident "b"),
                                                                    Assign "b" (BinOp Minus (Ident "a") (BinOp Mult (Ident "b") (BinOp Div (Ident "a") (Ident "b")))),
                                                                    Assign "a" (Ident "c")])])
           ((Ident "a"))]
           (Seq [Read "a",
                 Read "b",
                 Write (FunctionCall "GCD" [(Ident "a"), (Ident "b")])])`

## Программа 2: Бинарное возведение в степень
`func Pow(a, p){
    assign res (1);
    while (p) {
      if (p / 2 == 1) {
        assign res (res * a);
      } else {}; 
      assign a (a * a);
      assign p (p / 2);
    };
  } retrun(res);
  {
   read(a);
   read(p);
   write (Pow(a, p));
  }`


`Program [ Function "Pow" ["a", "p"]  (Seq [ Assign "res" (Num 1),
                                              While (Ident "p") (Seq [If (BinOp Equal (BinOp Div (Ident "p") (Num 2)) (Num 1))  
                                                                      (Seq [Assign "res" (BinOp Mult (Ident "res") (Ident "a"))])
                                                                      (Seq []),
                                              Assign "a" (BinOp Mult (Ident "a") (Ident "a")),
                                              Assign "p" (BinOp Div (Ident "p") (Num 2))])]) (Ident "res")]
            (Seq [Read "a",
                  Read "p",
                  Write (FunctionCall "Pow" [(Ident "a"), (Ident "p")])])`

## Программа 3: Проверка числа на то, что оно делится на каждую из своих цифр
`func func(n) { 
   assign res (1);
    while (n >= 10) {
      assign res (check(n, (n-(n/10)*10)));
      assign n (n/10);
    }; 
    assign res (check(n, n))
 ;} retrun(res);
 func check(n, i) {
   if (n==((n/i)*i)) {
     assign res (1);}
   else {
     assign res (0);};
 } retrun(res); 
  {
    read (n);
    write(func(n));
  }`

`Program [Function "func" ["n"] (Seq [Assign "res" (Num 1),
                                      While (BinOp Ge (Ident "n") (Num 10)) 
                                            (Seq [Assign "res" (FunctionCall "check" [ Ident "n", BinOp Minus (Ident "n") (BinOp Mult (BinOp Div (Ident "n") (Num 10)) (Num 10))]),
                                                  Assign "n" (BinOp Div (Ident "n") (Num 10))]),
                                      Assign "res" (FunctionCall "check" [Ident "n", Ident "n"])])
          (Ident "res"),
          Function "check" ["n", "i"] (If (BinOp Equal (Ident "n") (BinOp Mult (BinOp Div (Ident "n") (Ident "i")) (Ident "i")))
                                          (Seq [Assign "res" (Num 1)])
                                          (Seq [Assign "res" (Num 0)])) (Ident "res")]
          (Seq [Read "n",
                Write (FunctionCall "Pal" [(Ident "n")])])`

В язык добавились символы перевода строки, пробелы(почти: https://github.com/HandlessMidas/fl_2020_spbu_spr/issues/1). Знаков табуляции я не увидела, а жаль(((( Также автор языка утверждает, что
нельзя использовать ключевые слова в качестве названия переменных/функций, но я назвала в 3ей программе функцию `func` и это, вроде как, ключевое слово. В коде я вроде тоже не нашла, где проверяется
что `Ident` не ключевое слово. Eval вроде как считается, проблем не заметила. В целом всё хорошо, нужно только разобраться с пробелами в выражениях, потому что пользовательям языка очень некомфортно
писать выражения, если в каких-то местах пробелы можно ставить, а в каких-то нельзя и неочевидно почему. Также непонятно, можно ли использовать ключевые слова в качестве имен или всё же нет?

P.S. Также я заметила, что в parseExpr не было парсинга FunctionCall, но это быстро исправили, за что Данилу спасибо! :)
