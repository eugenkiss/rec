module Programs where

-- return 3
basic1
  = "main = I 3"

-- return 3
basic2
  = "main = S K K 3"

-- return 3
basic3
  =  "id = S K K;\n"
  ++ "main = id 3"

-- return 3
basic4
  =  "id = S K K;\n"
  ++ "main = twice twice twice id 3"

-- return 3
updating1
  = "main = twice (I I I) 3"

-- return 4
updating2
  =  "cons a b cc cn = cc a b;\n"
  ++ "nil      cc cn = cn;\n"
  ++ "hd list = list K abort;\n"
  ++ "tl list = list K1 abort;\n"
  ++ "abort = abort;\n"
  ++ "\n"
  ++ "infinite x = cons x (infinite x);\n"
  ++ "\n"
  ++ "main = hd (tl (infinite 4))"

-- return 20 (since - is defined as max)
arithmetic1
  = "main = 4 * 5 + (2 - 5)"

-- return 20
arithmetic2
  =  "inc x = x + 1;\n"
  ++ "main = twice twice inc 4"

-- return 3
arithmetic3
  =  "length xs = xs length1 0;\n"
  ++ "length1 x xs = 1 + (length xs);\n"
  ++ "main = length (cons 3 (cons 3 (cons 3 nil)))"

-- return 120
arithmetic4
  =  "fac n = if (n == 0) 1 (n * fac (n - 1));\n"
  ++ "main = fac 5"

-- return 2
arithmetic5
  =  "gcd a b = if (a == b) a if (a < b) (gcd b a) (gcd b (a - b));\n"
  ++ "main = gcd 6 10"

-- return 3
arithmetic6
  =  "nfib n = if (n == 0) 1 (1 + nfib (n - 1) + nfib (n - 2));\n"
  ++ "main = nfib 4"
