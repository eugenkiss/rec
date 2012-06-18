% TODO: Test pretty printer

> module RecTests (tests) where
>
> import Test.Framework.Providers.HUnit
> import Test.HUnit hiding (Test)
>
> import Rec
>
> tests = [ testCase "rec/parsing1" parsing1
>         , testCase "rec/parsing2" parsing2
>         , testCase "rec/parsing3" parsing3
>         , testCase "rec/basic1" basic1
>         , testCase "rec/basic2" basic2
>         , testCase "rec/basic3" basic3
>         , testCase "rec/basic4" basic4
>         , testCase "rec/basic5" basic5
>         , testCase "rec/basic6" basic6
>         , testCase "rec/basic7" basic7
>         , testCase "rec/basic8" basic8
>         , testCase "rec/if1" if1
>         , testCase "rec/if2" if2
>         , testCase "rec/if3" if3
>         , testCase "rec/if4" if4
>         , testCase "rec/if5" if5
>         , testCase "rec/if6" if6
>         , testCase "rec/if7" if7
>         , testCase "rec/if8" if8
>         , testCase "rec/if9" if9
>         , testCase "rec/if10" if10
>         , testCase "rec/arith1" arith1
>         , testCase "rec/arith2" arith2
>         , testCase "rec/arith3" arith3
>         , testCase "rec/arith4" arith4
>         , testCase "rec/arith5" arith5
>         , testCase "rec/lambda1" lambda1
>         , testCase "rec/lambda2" lambda2
>         , testCase "rec/lambda3" lambda3
>         , testCase "rec/lambda4" lambda4
>         ]
>
> parsing1 = parse' p @?= e
>   where
>   p = "main() := double(21); double(x) := x + x"
>   e =
>     [ ("main",   [],    Ap "double" [Num 21]          )
>     , ("double", ["x"], Ap "+"      [Var "x", Var "x"])
>     ]
>
> parsing2 = parse' p @?= e
>   where
>   p = "main() := 1 + add(2, add(2, 10)); add(x, y) := x + y"
>   e =
>     [ ("main", [],         Ap "+" [Num 1, Ap "add" [Num 2, Ap "add" [Num 2, Num 10]]])
>     , ("add",  ["x", "y"], Ap "+" [Var "x", Var "y"])
>     ]
>
> parsing3 = parse' p @?= e
>   where
>   p = "main() := \\x. x + x"
>   e =
>     [ ("main",   [],    Lam "x" (Ap "+" [Var "x", Var "x"]))
>     ]
>
>
> -- Evaluation
> -------------
> basic1 = run' p [] @?= 18
>   where
>   p = "main() := 18"
>
> basic2 = run' p [18] @?= 18
>   where
>   p = "main(a) := a"
>
> basic3 = run' p [21] @?= 42
>   where
>   p = "main(a) := double(a); double(x) := x + x"
>
> -- regression
> basic4 = run' p [] @?= 2
>   where
>   p = "main() := f(1); f(a) := a * 2"
>
> -- regression
> basic5 = run' p [] @?= 2
>   where
>   p = "main() := f(1); f(a) := a + a"
>
> -- regression
> basic6 = run' p [7] @?= 8
>   where
>   p = "main(a) := succ(a); succ(a) := a + 1"
>
> basic7 = run' p [2, 2] @?= 1
>   where
>   p = "main(a, b) := ((a^2 + b^2 - 5) * 30) / 10 % 8"
>
> basic8 = run' p [] @?= 4
>   where
>   p = "main() := f(((2^2 + 2^2 - 5) * 30) / 10 % 8); f(a) := a + 3"
>
> if1 = run' p [] @?= 2
>   where
>   p = "main() := if 0 then 1 else 2"
>
> if2 = run' p [] @?= 1
>   where
>   p = "main() := if 1 then 1 else 2"
>
> if3 = run' p [50] @?= 1
>   where
>   p = "main(a) := if a then 1 else 2"
>
> if4 = run' p [] @?= 1
>   where
>   p = "main() := if 5 = 5 then 1 else 2"
>
> if5 = run' p [] @?= 2
>   where
>   p = "main() := if 5 > 6 then 1 else 2"
>
> if6 = run' p [] @?= 1
>   where
>   p = "main() := if 5 > 6 || 100 > 0  then 1 else 2"
>
> if7 = run' p [6] @?= 1
>   where
>   p = "main(a) := if f(a) then 1 else 2; f(a) := a > 5"
>
> if8 = run' p [6] @?= 1
>   where
>   p =  "main(a, b) := if f(a) || f(b) then if 2 != 3 then 1 else 3 else 2;"
>     ++ "f(a) := a > 5 && a < g(10, 20);"
>     ++ "g(a, b) := a + 1"
>
> if9 = run' p [6] @?= 1
>   where
>   p = "main(a) := if if if f(a) && 1 then 1 else 0 then 1 else 0 then 1 else 0; f(a) := a > 5"
>
> if10 = run' p [4] @?= 4
>   where
>   p = "main(a, b) := if b = 0 then a else 9"
>
>
> arith1 = run' p [5] @?= 120
>   where
>   p =  "main(n) := fac(n);"
>     ++ "fac(n) := if n = 0 then 1 else n * fac(n-1)"
>
> arith2 = run' p [3] @?= 6
>   where
>   p =  "main(n) := if n = 0 then 1 else n * main(n-1)"
>
> arith3 = run' p [6, 10] @?= 2
>   where
>   p =  "main(a, b) := gcd(a, b);"
>     ++ "gcd(a, b) := if a = b then a else if a < b then gcd(b, a) else gcd(b, a-b)"
>
> arith4 = run' p [5] @?= 8
>   where
>   p =  "main(a) := nfib(a);"
>     ++ "nfib(n) := if n <= 1 then 1 else (nfib(n-1) + nfib(n-2))"
>
> arith5 = run' p [4] @?= 3
>   where
>   p =  "main(a) := fib(a);"
>     ++ "fib(n) := if n = 0 then 0 else (if n = 1 then 1 else (fib(n-1) + fib(n-2)))"
>
> lambda1 = run' p [] @?= 3
>   where
>   p =  "main(a) := twice(succ, 1);"
>     ++ "twice(f, x) := f(f(x));"
>     ++ "succ(x) := x + 1"
>
> lambda2 = run' p [] @?= 3
>   where
>   p =  "main(a) := twice(succ)(1);"
>     ++ "twice(f) := \\x. f(f(x));"
>     ++ "succ(x) := x + 1"
>
> lambda3 = run' p [] @?= 9
>   where
>   p =  "main(a) := mkadder(2)(1) + mkadder(3)(3);"
>     ++ "mkadder(x) := \\y. x + y"
>
> lambda4 = run' p [] @?= 3
>   where
>   p =  "main(a) := I(succ)(2)"
>     ++ "I(x) := x;"
>     ++ "succ(x) := x + 1"
>
> -- Überlege ob das erlaubt sein soll (anonymous lambdas)
> -- lambda5 = run' p [1] @?= 2
> --  where
> --   p =  "main(a) := (\\x. x + x)(1)"
>
> -- TODO: Noch mehr lambda tests
