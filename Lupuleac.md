# Фидбек для Василия Лупуляка

## Программа 1: Вычисление НОД
```
 fun GCD(a, b) {
   if (a<b) {
     c = a;
     a = b;
     b = c;};
   while (b) {
     c = b;
     b = (a-b*(a/b));
     a = c;};
   retrun(a);}
  { read(a);
    read(b); 
    print(GCD(a, b));
  }
  ```

  ```
  Program [ Function "GCD" ["a", "b"] (Seq [ If (BinOp Lt (Ident "a") (Ident "b")) 
                                               (Seq [Assign "c" (Ident "a"),
                                                  Assign "a" (Ident "b"),
                                                  Assign "b" (Ident "c")])
                                            (Seq []),
                                            While (Ident "b") (Seq [Assign "c" (Ident "b"),
                                                                    Assign "b" (BinOp Minus (Ident "a") (BinOp Mult (Ident "b") (BinOp Div (Ident "a") (Ident "b")))),
                                                                    Assign "a" (Ident "c")])], Return (Ident "res"))]
           (Seq [Read "a",
                 Read "b",
                 Write (FunctionCall "GCD" [(Ident "a"), (Ident "b")])])
```

## Программа 2: Бинарное возведение в степень
```
  fun Pow(a, p){
    res = 1;
    while (p) {
      if (p / 2 == 1) {
        res = (res * a);
      }; 
      a = a * a;
      p = p / 2;
    };
    return res;
  }
  {
   read(a);
   read(p);
   print (Pow(a, p));
  }
```


  ```
  Program [ Function "Pow" ["a", "p"]  (Seq [ Assign "res" (Num 1),
                                              While (Ident "p") (Seq [If (BinOp Equal (BinOp Div (Ident "p") (Num 2)) (Num 1))  
                                                                      (Seq [Assign "res" (BinOp Mult (Ident "res") (Ident "a"))])
                                                                      (Seq []),
                                              Assign "a" (BinOp Mult (Ident "a") (Ident "a")),
                                              Assign "p" (BinOp Div (Ident "p") (Num 2))])], Return (Ident "res"))]
            (Seq [Read "a",
                  Read "p",
                  Write (FunctionCall "Pow" [(Ident "a"), (Ident "p")])])
```

## Программа 3: Проверка числа на то, что оно делится на каждую из своих цифр
```
  fun func(n) { 
   res = 1;
    while (n >= 10) {
      res = check(n, (n-(n/10)*10)));
      n = (n/10);
    }; 
    res = check(n, n);
    return res;}
    
 func check(n, i) {
   if (n==((n/i)*i)) {
     res = 1;}
   else {
     res = 0;};
   return res;} 
  {
    read (n);
    print(func(n));
  }
```

 ``` 
 Program [Function "func" ["n"] (Seq [Assign "res" (Num 1),
                                      While (BinOp Ge (Ident "n") (Num 10)) 
                                            (Seq [Assign "res" (FunctionCall "check" [ Ident "n", BinOp Minus (Ident "n") (BinOp Mult (BinOp Div (Ident "n") (Num 10)) (Num 10))]),
                                                  Assign "n" (BinOp Div (Ident "n") (Num 10))]),
                                      Assign "res" (FunctionCall "check" [Ident "n", Ident "n"])], Return (Ident "res")),
          Function "check" ["n", "i"] (If (BinOp Equal (Ident "n") (BinOp Mult (BinOp Div (Ident "n") (Ident "i")) (Ident "i")))
                                          (Seq [Assign "res" (Num 1)])
                                          (Seq [Assign "res" (Num 0)]), Return (Ident "res"))]
          (Seq [Read "n",
                Write (FunctionCall "func" [(Ident "n")])])
```

Этот фидбек сделан для 8ой дз, поскольку 9ой еще нет. В язык добавилась возможность объявлять функции. В целом мой отзыв с последнего
времени остался тем же, поскольку кроме функций ничего нового не появилось и не ухудшилось. Всё также здорово, что парсятся пробелы 
и переводы строк. Багов в парсинге не обнаружено. 
