\begin{code}
module GotoTests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Util
import Goto
import qualified Rec
import GotoToRec

-- Helper

-- TODO: remove
runProgram :: [Integer] -> String -> Either String Integer
runProgram = flip $ mkStdRunner parse eval

runProgram' :: [Integer] -> String -> Integer
runProgram' = flip $ mkStdRunner' parse eval

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- Tests

tests :: [Test]
tests =
  [
  -- Parsing/Lexing
    testCase "goto/comments1" testComments1
  , testCase "goto/comments2" testComments2
  , testCase "goto/comments3" testComments3
  , testCase "goto/parsing1" testParsing1
  , testCase "goto/parsing2" testParsing2
  , testCase "goto/parsing3" testParsing3
  , testCase "goto/parsing4" testParsing4
  , testCase "goto/parsing5" testParsing5
  , testCase "goto/parsing6" testParsing6
  , testCase "goto/parsing7" testParsing7
  -- Strict transform tests
  , testCase "goto/transform/strict/renaming1" testStrictRenaming1
  , testCase "goto/transform/strict/renaming2" testStrictRenaming2
  , testCase "goto/transform/strict/renaming3" testStrictRenaming3
  , testCase "goto/transform/strict/renaming4" testStrictRenaming4
  , testCase "goto/transform/strict/renaming5" testStrictRenaming5
  , testCase "goto/transform/strict/assignment1" testStrictAssignment1
  , testCase "goto/transform/strict/assignment2" testStrictAssignment2
  , testCase "goto/transform/strict/arithmetic1" testStrictArithmetic1
  {-, testCase "goto/transform/strict/arithmetic2" testStrictArithmetic2-}
  {-, testCase "goto/transform/strict/arithmetic3" testStrictArithmetic3-}
  {-, testCase "goto/transform/strict/arithmetic4" testStrictArithmetic4-}
  {-, testCase "goto/transform/strict/arithmetic5" testStrictArithmetic5-}
  {-, testCase "goto/transform/strict/arithmetic6" testStrictArithmetic6-}
  , testCase "goto/transform/strict/arithmetic7" testStrictArithmetic7
  , testCase "goto/transform/strict/arithmetic8" testStrictArithmetic8
  {-, testCase "goto/transform/strict/control1" testStrictControl1-}
  {-, testCase "goto/transform/strict/control2" testStrictControl2-}
  {-, testCase "goto/transform/strict/control3" testStrictControl3-}
  {-, testCase "goto/transform/strict/control4" testStrictControl4-}
  {-, testCase "goto/transform/strict/control5" testStrictControl5-}
  {-, testCase "goto/transform/strict/control6" testStrictControl6-}
  {-, testCase "goto/transform/strict/control7" testStrictControl7-}
  {-, testCase "goto/transform/strict/control8" testStrictControl8-}
  {- , testCase "goto/transform/strict/control9" testStrictControl9-}
  , testCase "goto/transform/strict/control10" testStrictControl10
  {-, testCase "goto/transform/strict/control11" testStrictControl11-}
  {-, testCase "goto/transform/strict/control12" testStrictControl12-}
  {-, testCase "goto/transform/strict/control13" testStrictControl13-}
  {- , testCase "goto/transform/strict/control14" testStrictControl14-}
  , testCase "goto/transform/strict/control15" testStrictControl15
  , testCase "goto/transform/strict/control16" testStrictControl16
  , testCase "goto/transform/strict/control17" testStrictControl17
  -- Evaluation
  , testCase "goto/assignment1" testAssignment1
  , testCase "goto/assignment2" testAssignment2
  , testCase "goto/assignment3" testAssignment3
  , testCase "goto/assignment4" testAssignment4
  , testCase "goto/assignment5" testAssignment5
  , testCase "goto/assignment6" testAssignment6
  , testCase "goto/assignment7" testAssignment7
  , testCase "goto/arithmetic1" testArithmetic1
  , testCase "goto/arithmetic2" testArithmetic2
  , testCase "goto/arithmetic3" testArithmetic3
  , testCase "goto/arithmetic4" testArithmetic4
  , testCase "goto/arithmetic5" testArithmetic5
  , testCase "goto/arithmetic6" testArithmetic6
  , testCase "goto/arithmetic7" testArithmetic7
  , testCase "goto/arithmetic8" testArithmetic8
  , testCase "goto/arithmetic9" testArithmetic9
  , testCase "goto/arithmetic10" testArithmetic10
  , testCase "goto/arithmetic11" testArithmetic11
  , testCase "goto/arithmetic12" testArithmetic12
  , testCase "goto/arithmetic13" testArithmetic13
  , testCase "goto/arithmetic14" testArithmetic14
  , testCase "goto/arithmetic15" testArithmetic15
  , testCase "goto/looping1" testLooping1
  , testCase "goto/looping2" testLooping2
  , testCase "goto/control1" testControl1
  , testCase "goto/control2" testControl2
  , testCase "goto/control3" testControl3
  , testCase "goto/control4" testControl4
  , testCase "goto/control5" testControl5
  -- Stack
  , testCase "goto/stack1" testStack1
  , testCase "goto/stack2" testStack2
  -- Rec
  , testCase "goto/rec1" testGenRec1
  , testCase "goto/rec2" testGenRec2
  , testCase "goto/rec3" testGenRec3
  , testCase "goto/rec4" testGenRec4
  ]

-- Parsing/Lexing
-----------------

testComments1 = assertBool "" $ isRight $ runProgram [] $
    "// This is a comment\n" ++
    "x0 := x1 + 2"

testComments2 = assertBool "" $ isRight $ runProgram [] $
    "x0 := x1 + 2; // This is a comment\n" ++
    "x0 := x1 + 2"

testComments3 = assertBool "" $ isRight $ runProgram [] $
    "x0 := x1 + 2 /*" ++
    "This is a longer\n comment" ++
    "*/; x0 := x1 + 2"

-- Duplicate labels
testParsing1 = assertBool "" $ isLeft $ runProgram [] $
    "M1: x0 := 0; M1: x0 := 1; HALT"

-- Illegal ;
testParsing2 = assertBool "" $ isLeft $ runProgram [] $
    "x0 := x1 + 2;"

-- No : in :=
testParsing3 = assertBool "" $ isLeft $ runProgram [] $
    "x0 = x1 + 2"

-- No ; to separate statements
testParsing4 = assertBool "" $ isLeft $ runProgram [] $
    "x0 := x1 + 1\n" ++
    "x3 := x1 + 1"

-- Everything ok
testParsing5 = assertBool "" $ isRight $ runProgram [] "M1: HALT"

-- Everything ok
testParsing6 = assertBool "" $ isRight $ runProgram [] "M1: GOTO M1"

-- A valid strict program should be a valid extended Program.
-- The special case: IF .. THEN GOTO Mi;
-- I.e. the If does not end with an END!
testParsing7 = assertBool "" $ isRight $ runProgram [] $
    "M1: x0 := x1 + 3;"           ++
    "M2: IF x0 = 5 THEN GOTO M5;" ++
    "M3: x0 := x2 + 3;"           ++
    "M4: GOTO M6;"                ++
    "M5: x0 := x3 + 3;"           ++
    "M6: x0 := x4 + 3;"           ++
    "M7: HALT"


-- Strict transform tests
-------------------------
-- TODO: improve
testStrictRenaming1 = strictify (parse' e) @?= parse' s
  where
  e = "M1: x0 := x1 + 1"
  s = "M1: x0 := x1 + 1; M2: HALT"

testStrictRenaming2 = strictify (parse' e) @?= parse' s
  where
  e = "M1: x0 := v + 1"
  s = "M1: x0 := x1 + 1; M2: HALT"

-- GOTOs with an undefined label are simply transformed to M1.
testStrictRenaming3 = strictify (parse' e) @?= parse' s
  where
  e = "M1: GOTO timbuktu"
  s = "M1: GOTO M1"

testStrictRenaming4 = strictify (parse' e) @?= parse' s
  where
  e =  "quux := foo  + 1;"
    ++ "M3:   bar  := foo  + 2;"
    ++ "      x3   := quux + 0;"
    ++ "ij8h: quux := quux + 1;"
    ++ "      x6   := bar  + 0;"
    ++ "GOTO ij8h;"
    ++ "GOTO M9"
  s =  "M1:   x1   := x2   + 1;"
    ++ "M2:   x4   := x2   + 2;"
    ++ "M3:   x3   := x1   + 0;"
    ++ "M4:   x1   := x1   + 1;"
    ++ "M5:   x6   := x4   + 0;"
    ++ "M6: GOTO M4;"
    ++ "M7: GOTO M1"

-- This is a correct transformation of "x0 := x1 * x2". I encountered a
-- logical error of mine for the label renaming and fixed the mistake. This is
-- therefore a regression test.
-- The problem arises from the "clever" use of labels. The Label "M4" is
-- actually on 5th position, which should lead to changing all "M4"s in the
-- code to "M5". Then, on 6th position, there is the label "M5". Therefore, all
-- "M5"s in the code should be changed to "M6"s. But wait! That would change
-- the semantics of the program! The GOTO instruction on line 2 would not
-- anymore "go to" line 5 but instead to line 6! Hence, already changed lines
-- must be marked in order to prevent this problem from happening.
testStrictRenaming5 = strictify (parse' e) @?= parse' s
  where
  e =  "x0 := x1 + 3;\n"
    ++ "Nt: IF x0 = 5 THEN GOTO M4 END;\n"
    ++ "M3: x0 := x2 + 3;\n"
    ++ "Mx: GOTO M5;\n"
    ++ "M4: x0 := x3 + 3;\n"
    ++ "M5: x0 := x4 + 3"
  s =  "M1: x0 := x1 + 3;\n"
    ++ "M2: IF x0 = 5 THEN GOTO M5 END;\n"
    ++ "M3: x0 := x2 + 3;\n"
    ++ "M4: GOTO M6;\n"
    ++ "M5: x0 := x3 + 3;\n"
    ++ "M6: x0 := x4 + 3;\n"
    ++ "M7: HALT"

testStrictAssignment1 = strictify (parse' e) @?= parse' s
  where e = "x0 := x1"
        s = "M1: x0 := x1 + 0; M2: HALT"

testStrictAssignment2 = strictify (parse' e) @?= parse' s
  where e = "x0 := 42"
        s = "M1: x0 := x1 + 42; M2: HALT" -- x1 is unused
-- Using the label "M1" for e is important because I found a bug in my code
-- this way. Therefore, this is a regression test.

testStrictArithmetic1 = strictify (parse' e) @?= parse' s
  where e = "M1: x0 := x1 + x2"
        s = "M1: x0 := x1 + 0;"           ++
            "M2: x3 := x2 + 0;"           ++ -- x3 is counter
            "M3: IF x3 = 0 THEN GOTO M7;" ++
            "M4: x3 := x3 - 1;"           ++
            "M5: x0 := x0 + 1;"           ++
            "M6: GOTO M3;"                ++
            "M7: HALT"

testStrictArithmetic7 = strictify (parse' e1) @?= strictify (parse' e2)
  where e1 = "x0 := 7 + 42"
        e2 = "x1 := 7;"      ++ -- x1 is unused
             "x0 := x1 + 42"

testStrictArithmetic8 = strictify (parse' e1) @?= strictify (parse' e2)
  where e1 = "x0 := (x1 + 3) - x2"
        e2 = "x3 := x1 + 3;" ++ -- x3 is unused
             "x0 := x3 - x2"

testStrictControl10 = strictify (parse' e1) @?= strictify (parse' e2)
  where e1 = "IF x0 > 5 && 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "END"
        e2 = "IF x0 > 5 THEN"   ++
             "  IF 5 < 6 THEN"  ++
             "    x0 := x0 + 0" ++
             "  END "           ++ -- the space at the end is important!
             "END"

testStrictControl15 = strictify (parse' e1) @?= strictify (parse' e2)
  where e1 = "IF !(x0 = 5) THEN" ++
             "  x0 := x0 + 0"    ++
             "END"
        e2 = "IF x0 != 5 THEN" ++
             "  x0 := x0 + 0"  ++
             "END"

testStrictControl16 = strictify (parse' e1) @?= strictify (parse' e2)
  where e1 = "IF !(!(!(!(x0 = 5)))) THEN" ++
             "  x0 := x0 + 0"             ++
             "END"
        e2 = "IF x0 = 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl17 = strictify (parse' e1) @?= strictify (parse' e2)
  where e1 = "IF !(x0 = 5 && !(!(x1 < 6 || x0 > 6))) THEN" ++
             "  x0 := x0 + 0"                              ++
             "ELSE"                                        ++
             "  x1 := x1 + 0"                              ++
             "END"
        e2 = "IF x0 != 5 || (x1 >= 6 && x0 <= 6) THEN" ++
             "  x0 := x0 + 0"                          ++
             "ELSE"                                    ++
             "  x1 := x1 + 0"                          ++
             "END"


-- Evaluation
-------------

testAssignment1
    = runProgram' [] "M1: x0 := x1 + 1; M2: HALT"
  @?= 1
testAssignment2
    = runProgram' [] "M1: x1 := x1 + 1; M2: HALT"
  @?= 0
testAssignment3
    = runProgram' [] "M1: x1 := x1 + 1; M2: x0 := x1 + 0; M3: HALT"
  @?= 1
testAssignment4
    = runProgram' [] "M1: x2 := x9 + 2; M2: x1 := x2 + 2; M3: x0 := x1 + 2; M4: HALT"
  @?= 6
testAssignment5
    = runProgram' [] "x0 := x1 + 2"
  @?= 2
testAssignment6
    = runProgram' [] "x0 := 2"
  @?= 2
testAssignment7
    = runProgram' [] "x1 := 2; x0 := x1"
  @?= 2

testArithmetic1
    = runProgram' [] "M1: x0 := x1 + 10; M2: HALT"
  @?= 10
testArithmetic2
    = runProgram' [] "M1: x0 := x1 + 10; M2: x0 := x1 - 100; M3: HALT"
  @?= 0
testArithmetic3
    = runProgram' [] "M1:x0:=x121+827;M2:x1:=x2+0;M3:HALT"
  @?= 827
testArithmetic4
    = runProgram' [5, 6] "x0 := x1 + x2"
  @?= 11
testArithmetic5
    = runProgram' [6, 5] "x0 := x1 - x2"
  @?= 1
testArithmetic6
    = runProgram' [6, 9] "x0 := x1 - x2"
  @?= 0
testArithmetic7
    = runProgram' [6, 5] "x0 := x1 * x2"
  @?= 30
testArithmetic8
    = runProgram' [6, 0] "x0 := x1 * x2"
  @?= 0
testArithmetic9
    = runProgram' [4, 2] "x0 := x1^x2"
  @?= 16
testArithmetic10
    = runProgram' [4, 0] "x0 := x1^x2"
  @?= 1
testArithmetic11
    = runProgram' [8, 2] "x0 := x1 / x2"
  @?= 4
testArithmetic12
    = runProgram' [9, 2] "x0 := x1 / x2"
  @?= 4
testArithmetic13
    = runProgram' [0, 2] "x0 := x1 / x2"
  @?= 0
testArithmetic14
    = runProgram' [7, 3] "x0 := x1 % x2"
  @?= 1
testArithmetic15
    = runProgram' [10, 1] "x0 := 16 / 2^2 + (x1 * (x2 % 2)) - 1"
  @?= 13

testLooping1 = runProgram' [10] p @?= 10
  where
  p =  "M1: x3 := x1 + 0;"
    ++ "M2: IF x3 = 0 THEN GOTO M7;"
    ++ "M3: x3 := x3 - 1; "
    ++ "M4: x0 := x0 + 1; "
    ++ "M5: x1 := x2 + 1;"
    ++ "M6: GOTO M2;"
    ++ "M7: HALT"

testLooping2 = runProgram' [8,7] p @?= 8 * 7
  where
  p =  "M1: IF x1 = 0 THEN GOTO M9;"
    ++ "M2: x1 := x1 - 1; "
    ++ "M3: x3 := x2 + 0; "
    ++ "M4: IF x3 = 0 THEN GOTO M8;"
    ++ "M5: x3 := x3 - 1; "
    ++ "M6: x0 := x0 + 1; "
    ++ "M7: GOTO M4; "
    ++ "M8: GOTO M1; "
    ++ "M9: HALT"

testControl1 = runProgram' [10, 1] p @?= 13
  where
  p =  "c := 16 / 2^2 + (x1 * (x2 % 2)) - 1;"
    ++ "M1: c := c - 1;"
    ++ "x0 := x0 + 1;"
    ++ "x0 := x0 - 1;"
    ++ "x0 := x0 + 1;"
    ++ "IF c = 0 THEN HALT END;"
    ++ "GOTO M1"

testControl2 = runProgram' [10, 1] p @?= 26
  where
  p =  "c0 := 16 / 2^2 + (x1 * (x2 % 2)) - 1;"
    ++ "M1: c0 := c0 - 1;"
    ++ "    c1 := 2;"
    ++ "M2:   c1 := c1 - 1;"
    ++ "      x0 := x0 + 1;"
    ++ "      x0 := x0 - 1;"
    ++ "      x0 := x0 + 1;"
    ++ "      IF c1 != 0 THEN GOTO M2 END;"
    ++ "    IF c0 != 0 THEN GOTO M1 END"

testControl3 = runProgram' [] p @?= 1
  where
  p = "IF 2 = 2 THEN x0 := 1 END"

testControl4 = runProgram' [] p @?= 1
  where
  p = "IF 2 >= 2 THEN x0 := 1 ELSE x0 := 2 END"

testControl5 = runProgram' [10, 1] p @?= 42
  where
  p =  "IF !(16 / 2^2 + (x1 * (x2 % 2)) - 1 < 8 && x1 >= x2 || 2 = 2) THEN"
    ++ "      c := 2;"
    ++ "  M1: c  := c  - 1;"
    ++ "      x0 := x0 + 1;"
    ++ "      x0 := x0 - 1;"
    ++ "      x0 := x0 + 1;"
    ++ "      IF c != 0 THEN GOTO M1 END "
    ++ "ELSE "
    ++ "  IF !(!(3 >= 3)) THEN"
    ++ "    x0 := 42 "
    ++ "  END "
    ++ "END"


-- Stack tests
-------------------------

testStack1 = runProgram' [] p @?= 10
  where
  p =  "PUSH 1;"
    ++ "PUSH 10;"
    ++ "PUSH 24;"
    ++ "PUSH 101;"
    ++ "x := POP;"
    ++ "PUSH 202;"
    ++ "PUSH 202;"
    ++ "PUSH 202;"
    ++ "x0 := PEEK 2"

testStack2 = eval (strictify $ parse' p) [] @?= 3
  where
  p =  "PUSH 1;"
    ++ "PUSH 2;"
    ++ "x0 := POP;"
    ++ "PUSH 3;"
    ++ "x0 := PEEK 3"


-- Rec translation tests
------------------------

testGenRec1 = (genRec $ parse' p) @?= Rec.parse' e
  where
  p =  ""
    ++ "M1: x0 := x2 + 1;"
    ++ "M2: IF x1 = 0 THEN GOTO M6 END;"
    ++ "M3: x0 := x0 * x1;"
    ++ "M4: x1 := x1 - 1;"
    ++ "M5: GOTO M2;"
    ++ "M6: HALT"
  e =  ""
    ++ "main(x1, x2, x0) := M1(x1, x2, x0);"
    ++ "M1(x1, x2, x0) := M2(x1, x2, x2 + 1);"
    ++ "M2(x1, x2, x0) := if x1 = 0 then M6(x1, x2, x0) else M3(x1, x2, x0);"
    ++ "M3(x1, x2, x0) := M4(x1, x2, x0 * x1);"
    ++ "M4(x1, x2, x0) := M5(x1 - 1, x2, x0);"
    ++ "M5(x1, x2, x0) := M2(x1, x2, x0);"
    ++ "M6(x1, x2, x0) := x0"

testGenRec2 = Rec.eval (genRec $ parse' p) [4] @?= 24
  where
  p =  ""
    ++ "    x0 := 1;"
    ++ "M1: IF x1 = 0 THEN GOTO M2 END;"
    ++ "    x0 := x0 * x1;"
    ++ "    x1 := x1 - 1;"
    ++ "    GOTO M1;"
    ++ "M2: HALT"

testGenRec3 = eval (Rec.genGoto . genRec . parse' $ p) [4] @?= 24
  where
  p =  ""
    ++ "    x0 := 1;"
    ++ "M1: IF x1 = 0 THEN GOTO M2 END;"
    ++ "    x0 := x0 * x1;"
    ++ "    x1 := x1 - 1;"
    ++ "    GOTO M1;"
    ++ "M2: HALT"

-- regression
testGenRec4 = eval (Rec.genGoto . genRec . parse' $ p) [4] @?= 3
  where
  p =  ""
    ++ "    a := 0;"
    ++ "    b := 1;"
    ++ "M1: IF x1 = 0 THEN GOTO M2 END;"
    ++ "    t := a + b;"
    ++ "    a := b;"
    ++ "    b := t;"
    ++ "    x1 := x1 - 1;"
    ++ "    GOTO M1;"
    ++ "M2: x0 := a;"
    ++ "    HALT"
\end{code}