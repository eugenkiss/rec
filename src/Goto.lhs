\chapter{Goto}

\begin{code}
{-# LANGUAGE RankNTypes, PatternGuards #-}
\end{code}

\begin{code}
module Goto
  ( Program (..)
  , AExp (..)
  , BExp (..)
  , VId
  , LId
  , eval
  , run
  , run'
  , parse
  , parse'
  , pprint
  , desugar
  , desugarStack
  , desugarHeap
  , simplify
  , strictify
  , getVIds -- because cyclic dependancies are disallowed
  ) where

import Control.Applicative hiding ((<|>))
import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Data.Monoid
import Data.STRef
import Data.List ((\\), partition, nub, genericLength)
import Data.Char (isDigit)
import Data.Vector ((!))
import qualified Data.HashMap as M
import qualified Data.Vector as V

import Text.PrettyPrint.HughesPJ hiding ((<>), parens, integer)
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec hiding (Parser, State, labels, parse)
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Expr

import Util
\end{code}


\section{Abstrakte Syntax}

\begin{code}
data Program
  = Halt
  | Goto   LId
  | Assign VId AExp
  | If     BExp Program
  | IfElse BExp Program Program
  | Label  LId Program
  | Seq    [Program]
  | Loop   AExp Program
  | While  BExp Program
  -- augmented constructors
  | Push AExp -- push the result of the arithmetic expression into the stack
  | Pop  VId  -- pop the topmost stack entry into a variable
  | Peek VId AExp -- save the ith (i is determined from arith exp) item in the stack into a variable
  | Call LId Int
  | Return
  -- closures
  | PushHeap AExp
  | PeekHeap VId AExp
  | MakeClosure Int [AExp]
  | CallClosure AExp
  deriving Eq

data AExp
  = Var VId
  | Num Integer
  | AOp Op AExp AExp
  deriving Eq

data BExp
  = ROp Op AExp AExp
  | BOp Op BExp BExp
  | BNegOp BExp
  deriving Eq
\end{code}

\begin{code}
type LId = String
type VId = String
type Op = String
\end{code}

\begin{code}
instance Monoid Program where
    mempty = Seq []
    (Seq ps1) `mappend` (Seq ps2) = Seq $ ps1 ++ ps2
    (Seq ps)  `mappend` p         = Seq $ ps ++ [p]
    p         `mappend` (Seq ps)  = Seq $ p : ps
    p1        `mappend` p2        = Seq $ [p1, p2]
\end{code}


\section{Pretty Printing}


\begin{code}
pprint :: Program -> String
pprint = show . pprint'

pprint' :: Program -> Doc
pprint' (Assign v a) = text v <+> text ":=" <+> text (show a)
pprint' (Loop a p) =
    text "WHILE" <+> text (show a) <+> text "DO" $+$
      nest 2 (pprint' p)                         $+$
    nest 0 (text "END")
pprint' (While b p) =
    text "WHILE" <+> text (show b) <+> text "DO" $+$
      nest 2 (pprint' p)                         $+$
    nest 0 (text "END")
pprint' (If b p) =
    text "IF" <+> text (show b) <+> text "THEN" $+$
      nest 2 (pprint' p)                        $+$
    nest 0 (text "END")
pprint' (IfElse b p1 p2) =
    text "IF" <+> text (show b) <+> text "THEN" $+$
      nest 2 (pprint' p1)                       $+$
    nest 0 (text "ELSE")                        $+$
      nest 2 (pprint' p2)                       $+$
    nest 0 (text "END")
pprint' (Goto l)    = text "GOTO" <+> text l
pprint' (Halt)      = text "HALT"
pprint' (Label l p) = (text l PP.<> text ":") $$ nest (length l + 2) (pprint' p)
pprint' (Seq ps)    = vcat $ punctuate semi $ map pprint' ps

pprint' (Push a)   = text "PUSH" <+> text (show a)
pprint' (Pop v)    = text v <+> text ":= POP"
pprint' (Peek v a) = text v <+> text ":= PEEK" <+> text (show a)
pprint' (Call l n) = text "CALL" <+> text l PP.<> text "," <+> text (show n)
pprint' (Return)   = text "RETURN"

pprint' (PushHeap a)   = text "PUSH_HEAP" <+> text (show a)
pprint' (PeekHeap v a) = text v <+> text ":= PEEK_HEAP" <+> text (show a)
pprint' (MakeClosure p [])
  = text "MAKE_CLOSURE" <+> text (show p)
pprint' (MakeClosure p args)
  = text "MAKE_CLOSURE" <+> hcat (punctuate comma $ (text . show) p : map (text . show) args)
pprint' (CallClosure a)
  = text "CALL_CLOSURE" <+> text (show a)
\end{code}

%TODO: print without redundant parantheses

\begin{code}
instance Show AExp where
    show (Var v)       = v
    show (Num n)       = show n
    show (AOp s a1 a2) = "(" ++ show a1 ++ " " ++ s ++ " " ++ show a2 ++ ")"

instance Show BExp where
    show (BNegOp b)      = "!(" ++ show b ++ ")"
    show (BOp s b1 b2)   = "(" ++ show b1 ++ " " ++ s ++ " " ++ show b2 ++ ")"
    show (ROp s a1 a2) = "(" ++ show a1 ++ " " ++ s ++ " " ++ show a2 ++ ")"
\end{code}

\begin{code}
instance Show Program where
  show = pprint
\end{code}


\section{Parser}

\begin{code}
gotoDef
  = javaStyle
  { Token.reservedNames =
      [ "GOTO", "HALT", "END", "IF", "THEN", "ELSE", "DO"
      , "PUSH", "POP", "PEEK", "PUSH_HEAP", "PEEK_HEAP"
      , "CALL", "RETURN", "CALL_CLOSURE", "MAKE_CLOSURE"
      , "LOOP", "WHILE"
      ]
  , Token.reservedOpNames =
      [ ":="
      , "+", "-", "*", "/", "^", "%"
      , "=", "!=", "<", "<=", ">", ">="
      , "!", "&&", "||"
      ]
  , Token.opLetter = oneOf (concat (Token.reservedOpNames gotoDef))
  , Token.caseSensitive = True
  }

lexer = Token.makeTokenParser gotoDef

semiSep1   = Token.semiSep1   lexer
commaSep1  = Token.commaSep1  lexer
parens     = Token.parens     lexer
whiteSpace = Token.whiteSpace lexer
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
symbol     = Token.symbol     lexer
integer    = Token.integer    lexer
\end{code}

Given a string representation of an augmented Goto program parse it and
return either an error string or the AST.

\begin{code}
parse :: String -> Either String Program
parse = mkStdParser pProgram [] whiteSpace

parse' :: String -> Program
parse' = mkStdParser' pProgram [] whiteSpace
\end{code}

Carry along a list of labels to check if there aren't any duplicates.

\begin{code}
type Parser a = GenParser Char [LId] a

pProgram :: Parser Program
pProgram = do
  stmnts <- semiSep1 pStmnt
  return $ if length stmnts < 2 then head stmnts else Seq stmnts
\end{code}

\begin{code}
pStmnt :: Parser Program
pStmnt
  = choice
  [ pLabeledOrNot pIfElse
  , pLabeledOrNot pIf
  , pLabeledOrNot pIfStrict
  , pLabeledOrNot pLoop
  , pLabeledOrNot pWhile
  , pLabeledOrNot pGoto
  , pLabeledOrNot pHalt
  , pLabeledOrNot pPush
  , pLabeledOrNot pPop
  , pLabeledOrNot pPeek
  , pLabeledOrNot pCall
  , pLabeledOrNot pReturn
  , pLabeledOrNot pPushHeap
  , pLabeledOrNot pPeekHeap
  , pLabeledOrNot pCallClosure
  , pLabeledOrNot pMakeClosure
  , pLabeledOrNot pAssign
  ]
\end{code}

Parse a statement that may or may not be preceded by a label.

\begin{code}
pLabeledOrNot :: Parser Program -> Parser Program
pLabeledOrNot p = try (pLabeled p) <|> try p
\end{code}

Parse a statement that is preceded by a label.

\begin{code}
pLabeled :: Parser Program -> Parser Program
pLabeled p = do
  l <- pLabel
  labels <- getState
  when (l `elem` labels) $
     fail $ "Use of duplicate labels: " ++ l ++ " is already used!"
  updateState ((:) l)
  s <- p
  return $ Label l s

pLabel :: Parser LId
pLabel = do
  l <- identifier
  _ <- symbol ":"
  return l
\end{code}

TODO

\begin{code}
pCallClosure :: Parser Program
pCallClosure
  = do reserved "CALL_CLOSURE"
       aexp <- pAExp
       return $ CallClosure aexp

pMakeClosure :: Parser Program
pMakeClosure
  = do reserved "MAKE_CLOSURE"
       p <- integer
       args <- option [] (do { _ <- symbol ","
                             ; commaSep1 pAExp
                             })
       return $ MakeClosure (fromInteger p) args

pPushHeap :: Parser Program
pPushHeap = do
  reserved "PUSH_HEAP"
  aexp <- pAExp
  return $ PushHeap aexp

pPeekHeap :: Parser Program
pPeekHeap = do
  v <- identifier
  reservedOp ":="
  reserved "PEEK_HEAP"
  aexp <- pAExp
  return $ PeekHeap v aexp

pReturn :: Parser Program
pReturn = do
  reserved "RETURN"
  return $ Return

pCall :: Parser Program
pCall = do
  reserved "CALL"
  l <- identifier
  _ <- symbol ","
  n <- integer
  return $ Call l (fromInteger n)

pPush :: Parser Program
pPush = do
  reserved "PUSH"
  aexp <- pAExp
  return $ Push aexp

pPop :: Parser Program
pPop = do
  v <- identifier
  reservedOp ":="
  reserved "POP"
  return $ Pop v

pPeek :: Parser Program
pPeek = do
  v <- identifier
  reservedOp ":="
  reserved "PEEK"
  aexp <- pAExp
  return $ Peek v aexp

pHalt :: Parser Program
pHalt = do
  reserved "HALT"
  return Halt

pGoto :: Parser Program
pGoto = do
  reserved "GOTO"
  l <- identifier
  return $ Goto l

pAssign :: Parser Program
pAssign = do
  ident <- identifier
  reservedOp ":="
  s <- pAExp
  return $ Assign ident s

pLoop :: Parser Program
pLoop = do
  reserved "LOOP"
  a <- pAExp
  reserved "DO"
  body <- pProgram
  reserved "END"
  return $ Loop a body

pWhile :: Parser Program
pWhile = do
  reserved "WHILE"
  cond <- pBExp
  reserved "DO"
  body <- pProgram
  reserved "END"
  return $ While cond body

pIfStrict :: Parser Program
pIfStrict = do
  reserved "IF"
  cond <- pBExp
  reserved "THEN"
  thenpart <- pGoto
  return $ If cond thenpart

pIf :: Parser Program
pIf = do
  reserved "IF"
  cond <- pBExp
  reserved "THEN"
  thenpart <- pProgram
  reserved "END"
  return $ If cond thenpart

pIfElse :: Parser Program
pIfElse = do
  reserved "IF"
  cond <- pBExp
  reserved "THEN"
  thenpart <- pProgram
  reserved "ELSE"
  elsepart <- pProgram
  reserved "END"
  return $ IfElse cond thenpart elsepart

\end{code}

Boolean $\&$ Relational Expressions

\begin{code}
pBExp :: Parser BExp
pBExp = buildExpressionParser opTable pSimpleBExp
  where
  opTable =
    [ [ op "&&" AssocRight ]
    , [ op "||" AssocRight ]
    ]
  op name = Infix (reservedOp name >> return (\x y -> BOp name x y))

pSimpleBExp :: Parser BExp
pSimpleBExp
  = choice
  [ try pRExp
  , parens pBExp
  , pNegBOp
  ]

pNegBOp :: Parser BExp
pNegBOp = do
  reservedOp "!"
  whiteSpace
  _ <- string "("
  whiteSpace
  bexp <- pBExp
  whiteSpace
  _ <- string ")"
  whiteSpace
  return $ BNegOp bexp

pRExp :: Parser BExp
pRExp = do
    arg1 <- pAExp
    op <- choice [ symbol "="
                 , try (symbol "!=")
                 , try (symbol "<=")
                 , symbol "<"
                 , try (symbol ">=")
                 , symbol ">"
                 ]
    arg2 <- pAExp
    return $ ROp op arg1 arg2
\end{code}

Arithmetic Expressions.

\begin{code}
pAExp :: Parser AExp
pAExp = buildExpressionParser opTable pSimpleAExp
  where
  opTable =
    [ [ op "^" AssocRight ]
    , [ op "*" AssocLeft, op "/" AssocLeft ]
    , [ op "+" AssocLeft, op "-" AssocLeft ]
    , [ op "%" AssocRight ]
    ]
  op name = Infix (reservedOp name >> return (\x y -> AOp name x y))

pSimpleAExp :: Parser AExp
pSimpleAExp
  = choice
  [ pNum
  , pVar
  , parens pAExp
  ]
\end{code}

Das sollte wohl klar sein.

\begin{code}
pNum :: Parser AExp
pNum = liftM Num (integer <?> "constant")

pVar :: Parser AExp
pVar = liftM Var (identifier <?> "identifier")
\end{code}


\section{Simplifizierung}

Transform a Goto program to a simplified subset so that the evaluator can be
more concise.

It is important that addHalt comes before removeRedundancy due to
several special cases, e.g. a program which ends with a redundant
statement like "x0 := x0 + 0", which would be removed and GOTOs
pointing to that label would point into nothing (undefined). With
a HALT statement this case cannot happen.

TODO: deredundize should come immediately after desugar, but currently it
assumes too much. Rewrite deredundize without the superfluous assumptions!

\begin{code}
simplify :: Program -> Program
simplify
  =   desugar
  >>> addHaltOrNot
  >>> normalizeUndefLIds
  >>> normalizeLIds
  >>> normalizeVIds
  >>> deredundize
  >>> normalizeLIds
  >>> flatten
\end{code}


\subsection{Desugaring statements}

% TODO: make desugarCall desugarPush desugarArith...

First of all some helper functions.

Given an inital string s `mkIdStream` creates a list `s,s1,s2,...` whereas
`mkIdStream'` creates a list `s1,s2,s3,...`. This utility functions are used in
several places among others things to allow unique identifers.

\begin{code}
mkIdStream  id = id : map (id ++) (map show [1..])
mkIdStream' id = map (id ++) (map show [1..])
\end{code}

Create an infinite list of consecutive, strict variables beginning with 'x1'.

\begin{code}
vIdStream :: [VId]
vIdStream = mkIdStream' "x"
\end{code}

Create an infinite list of consecutive, strict labels beginning with 'M1'.

\begin{code}
lIdStream :: [LId]
lIdStream = mkIdStream' "M"
\end{code}

Often, one has to jump after a certain statement so one has to create a new
statment with a new label x in order to be able to reference that statement
with the label x with a GOTO. But the sole purpose of this label is to be
jumped at so it should do nothing. So here it is:

TODO: Name backpatching as an alternative.

\begin{code}
nop = Assign "x0" $ AOp "+" (Var "x0") (Num 0)
\end{code}

Let's now think what state we need when transforming the code.
Since sometimes an unused variable/label is needed in order to correctly
transform a statement two infinite lists of unused variable names and unused
label names are carried along as state that gets updated whenever an unused
variable resp. label name is requested.

\begin{code}
data TransformRecord
  = TransformRecord
  { _vIdStream        :: [VId]
  , _lIdStream        :: [LId]
  , _returnSectionId  :: LId
  , _rIdStream        :: [LId]
  , _rIdsUsed         :: [LId]
  }

type TransformState =  State TransformRecord
\end{code}

Return an unused variable name and remove it from the list of unused
variable names (the state).

\begin{code}
newVId :: TransformState VId
newVId = do
  tr <- get
  put tr { _vIdStream = drop 1 (_vIdStream tr) }
  return $ head $ _vIdStream tr
\end{code}

Return an unused label name and remove it from the list of unused label
names (the state).

\begin{code}
newLId :: TransformState LId
newLId = do
  tr <- get
  put tr { _lIdStream = drop 1 (_lIdStream tr) }
  return $ head $ _lIdStream tr
\end{code}

TODO

\begin{code}
newRId :: TransformState LId
newRId = do
  tr <- get
  let rId = head $ _rIdStream tr
  put tr { _rIdStream = drop 1 (_rIdStream tr)
         , _rIdsUsed   = (_rIdsUsed tr) ++ [rId]
         }
  return rId
\end{code}

A helper function to encode a loop with a predetermined number of iterations in
Goto.

\begin{verbatim}
    LOOP aexp DO P END
    ~>
    v := aexp;
    My: IF v = 0 THEN GOTO Mx END;
    v := v - 1;
    GOTO My;
    Mx: x0 := x0 + 0
\end{verbatim}

\begin{code}
mkLoop aexp p = do
  vId <- newVId
  let v = Var vId
  x   <- newLId
  y   <- newLId
  return
    $  Assign vId aexp
    <> Label  y (If (ROp "=" v (Num 0)) (Goto x))
    <> Assign vId (AOp "-" v (Num 1))
    <> p
    <> Goto   y
    <> Label  x nop
\end{code}

Helper function for the boolean negation operator.

\begin{code}
invert (ROp "="  a b) = ROp "!=" a b
invert (ROp "!=" a b) = ROp "="  a b
invert (ROp "<"  a b) = ROp ">=" a b
invert (ROp ">"  a b) = ROp "<=" a b
invert (ROp "<=" a b) = ROp ">"  a b
invert (ROp ">=" a b) = ROp "<"  a b
invert (BOp "&&" a b) = BOp "||" (BNegOp a) (BNegOp b)
invert (BOp "||" a b) = BOp "&&" (BNegOp a) (BNegOp b)
invert (BNegOp a)     = a
invert _              = error "Impossible! Wrong operator!"
\end{code}

Some helper functions for the augmented constructors.

Now we come to the meat.
Transform an extended Goto AST into a strict Goto AST.
@s@ is stack, @sp@ stack pointer, @fp@ is frame pointer (maybe remove one of
them), @pc@ is program counter.

\begin{code}
desugar :: Program -> Program
desugar p
  = evalState (do p <- go p
                  retsec  <- genReturnSection
                  return $ p <> retsec)
      TransformRecord
      { _vIdStream        = vIdStream \\ vIdsUsed
      , _lIdStream        = lIdStream \\ lIdsUsed

      , _returnSectionId  = head $ mkIdStream "ret" \\ lIdsUsed
      , _rIdStream        = mkIdStream' "r" \\ vIdsUsed
      , _rIdsUsed         = []
      }
  where
  vIdsUsed = getVIds p
  lIdsUsed = getLIds p
\end{code}

Return section should only be created if needed. If there were no `rIds`
created then there haven't been any `CALL` instructions so in that case don't
create a return section.

\begin{code}
  genReturnSection = do
    rids <- _rIdsUsed         <$> get
    rsid <- _returnSectionId  <$> get
    if null rids
       then return mempty
       else go $ (Label rsid
                  $ Seq $ map (genCondJump "pc") $ zip [1..] rids)
               <> Assign "x0" (Var "pc") <> Halt -- Only for testing TODO: remove
    where
    genCondJump pc (i, l) = If (ROp "=" (Var pc) (Num i)) $ Goto l
\end{code}

Keep `HALT` \& `GOTO Mx` unchanged as these instructions are already desugared!

\begin{code}
  go Halt       = return Halt
  go p@(Goto _) = return p
\end{code}

@v0 := v1 ~> v0 := v1 + 0@

\begin{code}
  go (Assign v0 (Var v1)) = return $ Assign v0 (AOp "+" (Var v1) (Num 0))
\end{code}

@v0 := n ~> v0 := v_new + n@

\begin{code}
  go (Assign v0 (Num n)) = do
    v <- Var <$> newVId
    return $ Assign v0 (AOp "+" v (Num n))
\end{code}

Keep `v0 := v1 o c` \& `v0 := v1 o v2` unchanged as further desugaring to `+`
and loops would increase the runtime too much when evaluating the program.

\begin{code}
  go p@(Assign _ (AOp op (Var _) (Num _)))
    | op `elem` ["+","-","*","^","/","%"] = return p
  go p@(Assign _ (AOp op (Var _) (Var _)))
    | op `elem` ["+","-","*","^","/","%"] = return p
\end{code}

Desugar `v0 := a o b` to `va := a; vb := b; v0 := va o vb` where `a` \& `b`
itself are both arithmetic expressions so that each right hand side of an
assignment has at most one operator. If either `a` is a sole variable or `b` is
a sole variable or a number do not introduce a superfluous assignment.

\begin{code}
  go (Assign v0 (AOp op (Var v) b)) = do       -- v0 := v o b =>
    vb <- newVId
    go $  Assign vb b                          -- vb := b
       <> Assign v0 (AOp op (Var v) (Var vb))  -- v0 := v o vb
  go (Assign v0 (AOp op a (Var v))) = do       -- v0 := a o v =>
    va <- newVId
    go $  Assign va a                          -- va := a
       <> Assign v0 (AOp op (Var va) (Var v))  -- v0 := va o v
  go (Assign v0 (AOp op a (Num n))) = do       -- v0 := a o n =>
    va <- newVId
    go $  Assign va a                          -- va := a
       <> Assign v0 (AOp op (Var va) (Num n))  -- v0 := va o n
  go (Assign v0 (AOp op a b)) = do             -- v0 := a o b =>
    va <- newVId
    vb <- newVId
    go $  Assign va a                          -- va := a
       <> Assign vb b                          -- vb := b
       <> Assign v0 (AOp op (Var va) (Var vb)) -- v0 := va o vb
\end{code}

Now, here come the IF instructions.

`IF xi o c/xj THEN GOTO Mx END` -- Keep that unchanged!

\begin{code}
  go p@(If (ROp _ (Var _) x) (Goto _))
    | Num _ <- x = return p
    | Var _ <- x = return p
\end{code}

`LOOP a DO P END` becomes

\begin{verbatim}
v := a;
Mx: IF v = 0 THEN GOTO My END;
v := v - 1;
GOTO My;
My: x0 := x0 + 0
\end{verbatim}

\begin{code}
  go (Loop a p) = do
    t <- mkLoop a p
    go t
\end{code}

`WHILE b DO P END` becomes

\begin{verbatim}
Mx: IF !b THEN GOTO My;
P;
GOTO Mx;
My: x0 := x0 + 0
\end{verbatim}

\begin{code}
  go (While cond p) = do
    mx <- newLId
    my <- newLId
    go $  Label mx (If (BNegOp cond) (Goto my))
       <> p
       <> Goto mx
       <> Label my nop
\end{code}

`IF a o b THEN P END` becomes

\begin{verbatim}
    va := a;
    vb := b;
    IF va o vb THEN GOTO Mthen END;
    GOTO Mskip;
    Mthen: P;
    Mskip: nop
\end{verbatim}

\begin{code}
  go (If (ROp op a b) p) = do
    va <- newVId
    vb <- newVId
    lthen <- newLId
    lskip <- newLId
    go $  Assign va a
       <> Assign vb b
       <> If (ROp op (Var va) (Var vb)) (Goto lthen)
       <> Goto lskip
       <> Label lthen p
       <> Label lskip nop
\end{code}

`IF a o b THEN P1 ELSE P2 END` becomes

    va := a;
    vb := b;
    IF va o vb THEN GOTO Mthen END;
    P2;
    GOTO Mskip;
    Mthen: P;
    Mskip: nop

\begin{code}
  go (IfElse (ROp op a b) p1 p2) = do
    va <- newVId
    vb <- newVId
    lthen <- newLId
    lskip <- newLId
    go $  Assign va a
       <> Assign vb b
       <> If (ROp op (Var va) (Var vb)) (Goto lthen)
       <> p2
       <> Goto lskip
       <> Label lthen p1
       <> Label lskip nop
\end{code}

`IF b THEN P {ELSE P2} END` where b is a boolean expression.

\begin{code}
  go (If (BOp "&&" a b) p) = go $ If a (If b p)
  go (IfElse (BOp "&&" a b) p1 p2) = do
    v    <- newVId
    loop <- mkLoop (Var v) p2
    go $  Assign v (Num 1)
       <> If a (If b (Assign v (Num 0) <> p1))
       <> loop
  go (If (BOp "||" a b) p) = do
    v    <- newVId
    loop <- mkLoop (Var v) p
    go $  Assign v (Num 0)
       <> If a (Assign v (Num 1))
       <> If b (Assign v (Num 1))
       <> loop
  go (IfElse (BOp "||" a b) p1 p2) = do
    v1 <- newVId
    v2 <- newVId
    loop1 <- mkLoop (Var v1) p1
    loop2 <- mkLoop (Var v2) p2
    go $  Assign v1 (Num 0)
       <> Assign v2 (Num 1)
       <> If a (Assign v1 (Num 1) <> Assign v2 (Num 0))
       <> If b (Assign v1 (Num 1) <> Assign v2 (Num 0))
       <> loop1
       <> loop2
  go (If (BNegOp bexp) p) = go $ If (invert bexp) p
  go (IfElse (BNegOp bexp) p1 p2) = go $ IfElse (invert bexp) p1 p2
\end{code}

Here come the augmented constructors.

\begin{code}
  go p@(Push (Var _)) = return (p <> Assign "sp" (AOp "+" (Var "sp") (Num 1)))
  go (Push aexp) = do
    t <- newVId
    go $  mempty
       <> Assign t aexp
       <> Push (Var t)
\end{code}

\begin{code}
  go p@(Pop _) = return (p <> Assign "sp" (AOp "-" (Var "sp") (Num 1)))
\end{code}

\begin{code}
  go p@(Peek _ (Var _)) = return p
  go (Peek x0 aexp) = do
    t <- newVId
    go $  mempty
       <> Assign t aexp
       <> Peek x0 (Var t)
\end{code}

\begin{code}
  go p@(PushHeap (Var _)) = return (p <> Assign "hp" (AOp "+" (Var "hp") (Num 1)))
  go (PushHeap aexp) = do
    t <- newVId
    go $  mempty
       <> Assign t aexp
       <> PushHeap (Var t)
\end{code}

\begin{code}
  go p@(PeekHeap _ (Var _)) = return p
  go (PeekHeap x0 aexp) = do
    t <- newVId
    go $  mempty
       <> Assign t aexp
       <> PeekHeap x0 (Var t)
\end{code}

\begin{code}
  go (MakeClosure p args) = do
    go $  mempty
       <> PushHeap (Num $ toInteger $ p)
       -- <> Push (Var "hp")
       <> (Seq $ map PushHeap args)
\end{code}

\begin{code}
  go (CallClosure aexp) = do
    r <- newRId
    returnVal <- newVId
    temp <- newVId
    pcVal <- (length . _rIdsUsed) <$> get
    go $  mempty
       <> Assign temp (Num 0) -- side effects TODO: I can't really explain, but without this some tests break...
       <> Push (Var "fp")
       <> Assign "fp" (Var "sp")
       <> Push (Num $ toInteger pcVal)
       <> Push aexp -- push heap adress on stack (for environment/free vars)
       -- <> Assign "cp" aexp -- get pointer to closure code
       <> PeekHeap "cp" aexp -- get pointer to closure code
       <> Goto "lamret"
       <> Label r
          (  Pop returnVal
          <> Pop temp -- old heap adress
          <> Pop temp -- old return adress
          <> Pop "fp" -- reassign old fp
          <> Pop temp -- pop old args + ret addr
         -- <> Assign "fp" (Var "sp") -- reset fp!
          <> Push (Var returnVal)
          )
\end{code}

\begin{code}
  go (Call fn n) = do
    r <- newRId
    returnVal <- newVId
    temp <- newVId
    pcVal <- (length . _rIdsUsed) <$> get
    go $  mempty
       <> Assign temp (Num 0) -- side effects TODO: I can't really explain, but without this some tests break...
       <> Push (Var "fp")
       <> Assign "fp" (Var "sp")
       <> Push (Num $ toInteger pcVal)
      -- <> Push (Num 0) -- stub heap adress
       <> Goto fn
       <> Label r
          (  Pop returnVal
       --   <> Pop temp -- old heap adress
          <> Pop temp -- old return adress
          <> Pop "fp" -- reassign old fp
          <> mkPopSequence n temp -- pop old args + ret addr
         -- <> Assign "fp" (Var "sp") -- reset fp!
          <> Push (Var returnVal)
          )
    where
    mkPopSequence n t = Seq $ replicate n (Pop t)
\end{code}

\begin{code}
  go Return = do
    ret <- _returnSectionId <$> get
    go $  Peek "pc" (AOp "+" (Var "fp") (Num 1))
       <> Goto ret
\end{code}

Here come the containers, if you like.

\begin{code}
  -- Mx: x0 := x0 + 0 -- Do nothing!
  go p@(Label _ (Assign "x0" (AOp "+" (Var "x0") (Num 0)))) = return p
  -- Mx: P
  go (Label l p) = go $ Label l nop <> p
  -- P1; P2;...
  go (Seq ps) = liftM (Seq . flatten') $ mapM go ps
    where
    flatten' = foldr f []
    f (Seq ps) acc = ps ++ acc
    f x        acc = x : acc
\end{code}

All cases must have been checked by now. If not there is an error in the
transformation.

\begin{code}
  go _ = error $ "Not all cases were considered while desugaring Goto!"
\end{code}


\subsection{Normalizing variable identifiers}

Return a list without duplicates of all variable identifiers in the
arithemtic expression.

\begin{code}
getVIdsInAExp :: AExp -> [VId]
getVIdsInAExp = nub . f
  where f (Num _)     = []
        f (Var var)     = [var]
        f (AOp _ e1 e2) = f e1 ++ f e2
\end{code}

Return a list without duplicates of all variable identifiers in the
boolean expression.

\begin{code}
getVIdsInBExp :: BExp -> [VId]
getVIdsInBExp = nub . f
  where f (ROp _ aexp1 aexp2) = getVIdsInAExp aexp1 ++
                                  getVIdsInAExp aexp2
        f (BOp _ bexp1 bexp2)   = f bexp1 ++ f bexp2
        f (BNegOp bexp)         = f bexp
\end{code}

Analyze the AST and return a list without duplicates of all used variable
names.

\begin{code}
getVIds :: Program -> [VId]
getVIds = nub . go
  where
  go (Halt)               = []
  go (Goto _)             = []
  go (Assign v aexp)      = v : getVIdsInAExp aexp
  go (Loop aexp p)        = getVIdsInAExp aexp ++ go p
  go (While bexp p)       = getVIdsInBExp bexp ++ go p
  go (If bexp p)          = getVIdsInBExp bexp ++ go p
  go (IfElse bexp p1 p2)  = getVIdsInBExp bexp ++ go p1 ++ go p2
  go (Push aexp)          = getVIdsInAExp aexp
  go (Pop v)              = [v]
  go (Peek v aexp)        = v : getVIdsInAExp aexp
  go (Call _ _)           = []
  go (Return)             = []
  go (PushHeap aexp)      = getVIdsInAExp aexp
  go (PeekHeap v aexp)    = v : getVIdsInAExp aexp
  go (MakeClosure _ args) = concatMap getVIdsInAExp args
  go (CallClosure aexp)   = getVIdsInAExp aexp
  go (Label _ p)          = go p
  go (Seq ps)             = concatMap go ps
\end{code}

Rename all occurences of 'from' as a variable identifier to 'to' in the given
arithmetic expression.

\begin{code}
renameVIdInAExp :: VId -> VId -> AExp -> AExp
renameVIdInAExp from to aexp = case aexp of
    Num n        -> Num n
    Var var      -> Var (rename var)
    AOp op e1 e2 -> AOp op (renameVIdInAExp from to e1)
                           (renameVIdInAExp from to e2)
  where rename var
            | var == from = to
            | otherwise   = var
\end{code}

Rename all occurences of 'from' as a variable identifier to 'to' in the given
boolean expression.

\begin{code}
renameVIdInBExp :: VId -> VId -> BExp -> BExp
renameVIdInBExp from to bexp = case bexp of
    ROp op aexp1 aexp2 -> ROp op (renameVIdInAExp from to aexp1)
                                     (renameVIdInAExp from to aexp2)
    BOp op bexp1 bexp2   -> BOp   op (renameVIdInBExp from to bexp1)
                                     (renameVIdInBExp from to bexp2)
    BNegOp bexp1         -> BNegOp   (renameVIdInBExp from to bexp1)
\end{code}

Rename all occurences of 'from' as a variable identifier to 'to' in the given
AST.

\begin{code}
renameVId :: VId -> VId -> Program -> Program
renameVId from to ast = case ast of
  Halt               -> Halt
  Goto l             -> Goto l
  Assign v aexp      -> Assign (r v) (rAExp aexp)
  Loop aexp p        -> Loop (rAExp aexp) (rVId p)
  While bexp p       -> While (rBExp bexp) (rVId p)
  If bexp p          -> If (rBExp bexp) (rVId p)
  IfElse bexp p1 p2  -> IfElse (rBExp bexp) (rVId p1) (rVId p2)
  Push aexp          -> Push (rAExp aexp)
  Pop v              -> Pop (r v)
  Peek v aexp        -> Peek (r v) (rAExp aexp)
  Call f n           -> Call f n
  Return             -> Return
  PushHeap aexp      -> PushHeap (rAExp aexp)
  PeekHeap v aexp    -> PeekHeap (r v) (rAExp aexp)
  MakeClosure p args -> MakeClosure p (map rAExp args)
  CallClosure aexp   -> CallClosure (rAExp aexp)
  Label l p          -> Label l (rVId p)
  Seq ps             -> Seq (map rVId ps)
  where
  rVId  = renameVId from to
  rAExp = renameVIdInAExp from to
  rBExp = renameVIdInBExp from to
  r v | v == from = to | otherwise = v
\end{code}

Return true if the given identifier is strict.

\begin{code}
isStrictVar :: VId -> Bool
isStrictVar (x:xs) | not (null xs) = x == 'x' && all isDigit xs
isStrictVar _      = False
\end{code}

Change all occurences of unstrict variable names into strict variable names
without altering the semantics of the AST.

\begin{code}
normalizeVIds :: Program -> Program
normalizeVIds ast = foldr (uncurry renameVId) ast renameMappings
  where
  renameMappings     = zip unstrict unused
  unused             = vIdStream \\ strict
  (strict, unstrict) = partition isStrictVar $ getVIds ast
\end{code}


\subsection{Normalizing label identifiers}

% Given a list of disjunctive rename mappings in the form [...,(from_i,
% to_i),...] rename all occurences of 'from_i' as a label name to 'to_i' in the
% given AST. Do not rename the labels denoting the statement, however, only in a
% GOTO.

\begin{code}
renameGotoLabels :: [(LId, LId)] -> Program -> Program
renameGotoLabels = go
  where
  go [] p = p
  go mappings@((from,to):rest) p = case p of
      Goto l | l == from -> Goto to
             | otherwise -> go rest $ Goto l
      If bexp p          -> If bexp (go mappings p)
      IfElse bexp s1 s2  -> IfElse bexp (go mappings s1) (go mappings s2)
      Label l p          -> Label l (go mappings p)
      Seq ps             -> Seq (map (go mappings) ps)
      x                  -> x
\end{code}

Rename and reorder the labels in such a way that they are successive and the
first label is "M1". Assume that the AST is already flat/desugared and that
there are *no* undefined labels.

\begin{code}
normalizeLIds :: Program -> Program
normalizeLIds (Seq ps) =
  let (ps', mappings) = foldr go ([], []) $ zip ps lIdStream
  in  renameGotoLabels mappings (Seq ps')
  where
  go (Label l s, l') (ps, mappings)
    | l == l'   = ((Label l  s):ps,         mappings)
    | otherwise = ((Label l' s):ps, (l, l'):mappings)
  go (s, l') (ps, mappings) = ((Label l' s):ps, mappings)
normalizeLIds p = normalizeLIds (Seq [p])
\end{code}


\subsection{Removing redundancy}

Analyze the AST and return a list without duplicates of all used label names.

\begin{code}
getLIds :: Program -> [LId]
getLIds = nub . go
  where
  go (Halt)            = []
  go (Goto l)          = [l]
  go (Assign _ _)      = []
  go (Loop _ p)        = go p
  go (While _ p)       = go p
  go (If _ p)          = go p
  go (IfElse _ p1 p2)  = go p1 ++ go p2
  go (Label l p)       = l : go p
  go (Seq ps)          = concatMap go ps
  go (Push _)          = []
  go (Pop _)           = []
  go (Peek _ _)        = []
  go (Call l _)        = [l]
  go (Return)          = []
  go (PushHeap _)      = []
  go (PeekHeap _ _)    = []
  go (MakeClosure _ _) = []
  go (CallClosure _)   = []
\end{code}


During transformation some "NOPs" (like "x0 := x0 + 0") are used here and
there to keep the transformation and thus the code simpler. However, as
these statements are redundant they should be removed so as to unclutter the
transformed code. Assume that the AST is in strict form and that it is
a sequence of statements if only the singleton sequence.

\begin{code}
deredundize :: Program -> Program
deredundize ast = removeRedundantStats . renameGotoLabels mappings $ ast
  where mappings = map (\(i,j) -> ('M':show i, 'M':show j))
                 . relabelMappings 0 1 . reverse . naiveRelabelMappings $ ast
\end{code}

relabelMappings is needed for the case of several successive "NOPs"
like "M3: x0 := x0 + 0; M4: x0 := x0 + 0; M5 := x0 := x0 + 0" (here
n would be 3 when the recursion reaches M3).

\begin{code}
        relabelMappings _    _ []              = []
        relabelMappings lastIndex n ((i,j):xs) =
            if lastIndex - j == 1
               then (i, j+n) : relabelMappings j (n+1) xs
               else (i, j)   : relabelMappings j 1     xs
        naiveRelabelMappings = map (\l -> (l, succ l)) . getRedundantLabelIndices
\end{code}

\begin{code}
removeRedundantStats :: Program -> Program
removeRedundantStats (Seq stats) = Seq $ filter (not . isRedundant) $ stats
removeRedundantStats _ = error $ "This should be impossible"
\end{code}

\begin{code}
getRedundantLabelIndices :: Program -> [Integer]
getRedundantLabelIndices (Seq stats) =
    map extractIndex . getLIds . Seq . filter isRedundant $ stats
  where extractIndex (_:i) = (read i :: Integer)
        extractIndex _     = error $ "This should be impossible"
getRedundantLabelIndices _ = error $ "This should be impossible"
\end{code}

\begin{code}
isRedundant :: Program -> Bool
isRedundant (Label _ (Assign v0 (AOp _ (Var v1) (Num 0))))
    | v0 == v1  = True
    | otherwise = False
isRedundant _ = False
\end{code}


\subsection{Sonstiges}

Analyze the AST and return a list without duplicates of all used label names
but do not consider label names that are only used in a goto statement.

\begin{code}
getLIdsWithoutGoto :: Program -> [LId]
getLIdsWithoutGoto = nub . go
  where
  go (Halt)           = []
  go (Goto _)         = []
  go (Assign _ _)     = []
  go (Push _)         = []
  go (Pop _)          = []
  go (Peek _ _)       = []
  go (PushHeap _)     = []
  go (PeekHeap _ _)   = []
  go (If _ p)         = go p
  go (IfElse _ p1 p2) = go p1 ++ go p2
  go (Label l p)      = l : go p
  go (Seq ps)         = concatMap go ps
  go _                = error "Impossible!"
\end{code}

Transform GOTOs with an undefined label to "M1".

\begin{code}
normalizeUndefLIds :: Program -> Program
normalizeUndefLIds p = go (getLIdsWithoutGoto p) p
  where
  go ls (Goto l)
    | not (l `elem` ls)     = Goto "M1"
  go ls (If bexp p)         = If bexp (go ls p)
  go ls (IfElse bexp p1 p2) = IfElse bexp (go ls p1) (go ls p2)
  go ls (Label l p)         = Label l (go ls p)
  go ls (Seq ps)            = Seq (map (go ls) ps)
  go _  x                   = x
\end{code}

Add a Halt statement to the end if needed.

\begin{code}
addHaltOrNot :: Program -> Program
addHaltOrNot (Seq ps)
  | null ps   = Seq [halt]
  | otherwise = case (last ps) of
      Halt             -> Seq ps
      Goto _           -> Seq ps
      Label _ Halt     -> Seq ps
      Label _ (Goto _) -> Seq ps
      _                -> Seq $ ps ++ [halt]
  where
  halt = Label (head $ lIdStream \\ (getLIds (Seq ps))) Halt
addHaltOrNot p = addHaltOrNot $ Seq [p]
\end{code}

Unwrap the singleton sequence.

\begin{code}
flatten :: Program -> Program
flatten (Seq [x]) = x
flatten x         = x
\end{code}


\section{Evaluation}

% TODO: comments, improve code

\begin{code}
type Index = Integer
\end{code}

\begin{code}
type VarEnv s = STRef s (M.Map Index (STRef s Integer))
type Env s = (VarEnv s, [Integer], [Integer])
\end{code}

\begin{code}
nullEnv :: ST s (VarEnv s)
nullEnv = newSTRef M.empty
\end{code}

\begin{code}
getVar :: VarEnv s -> Index -> ST s Integer
getVar envRef i = do
  env <- readSTRef envRef
  case M.lookup i env of
    Just varRef -> readSTRef varRef
    Nothing     -> do x <- newSTRef 0
                      writeSTRef envRef (M.insert i x env)
                      return $ fromInteger 0
\end{code}

\begin{code}
setVar :: VarEnv s -> Index -> Integer -> ST s ()
setVar envRef i v = do
  env <- readSTRef envRef
  case M.lookup i env of
    Just varRef -> writeSTRef varRef v
    Nothing     -> do x <- newSTRef v
                      writeSTRef envRef (M.insert i x env)
\end{code}

Given a strict Goto AST and a list of arguments evaluate the program
and return the value of 'x0'.

\begin{code}
eval :: Program -> [Integer] -> Integer
eval ast args = runST $ do
  envRef <- nullEnv
  let (Seq stats) = case simplify ast of
                      Seq ss -> Seq ss
                      other  -> Seq [other]
  let statsArr = V.fromList stats
  forM_ [1..length args] $ \i ->
      setVar envRef (toInteger i) (args !! (i-1))
  eval' (envRef, [], []) statsArr 1
  getVar envRef 0
\end{code}

\begin{code}
eval' :: Env s -> V.Vector Program -> Integer -> ST s ()
eval' (env, stack, heap) arr index = do
  let stmnt = arr ! ((fromInteger index) - 1)
  case stmnt of
    Label (_:l) (Push (Var (_:j))) -> do
      xj <- getVar env (read j)
      eval' (env, xj:stack, heap) arr $ succ (read l)
    Label (_:l) (Pop (_:i)) -> do
      let (top:rest) = stack
      setVar env (read i) $! top
      eval' (env, rest, heap) arr $ succ (read l)
    Label (_:l) (Peek (_:i) (Var (_:j))) -> do
      let sp = genericLength stack
      xj <- getVar env (read j)
      setVar env (read i) $! stack !! (fromIntegral (sp - xj))
      eval' (env, stack, heap) arr $ succ (read l)
    Label (_:l) (PushHeap (Var (_:j))) -> do
      xj <- getVar env (read j)
      eval' (env, stack, xj:heap) arr $ succ (read l)
    Label (_:l) (PeekHeap (_:i) (Var (_:j))) -> do
      let hp = genericLength heap
      xj <- getVar env (read j)
      setVar env (read i) $! heap !! (fromIntegral (hp - xj))
      eval' (env, stack, heap) arr $ succ (read l)
    Label (_:l) (Assign (_:i) (AOp op (Var (_:j)) (Num n))) -> do
      xj <- getVar env (read j)
      setVar env (read i) $! toNativeAOp op xj n
      eval' (env, stack, heap) arr $ succ (read l)
    Label (_:l) (Assign (_:i) (AOp op (Var (_:j)) (Var (_:k)))) -> do
      xj <- getVar env (read j)
      xk <- getVar env (read k)
      setVar env (read i) $! toNativeAOp op xj xk
      eval' (env, stack, heap) arr $ succ (read l)
    Label (_:l1) (If (ROp op (Var (_:i)) (Num n)) (Goto (_:l2))) -> do
      xi <- getVar env (read i)
      if toNativeROp op xi n
         then eval' (env, stack, heap) arr (read l2)
         else eval' (env, stack, heap) arr $ succ (read l1)
    Label (_:l1) (If (ROp op (Var (_:i)) (Var (_:j))) (Goto (_:l2))) -> do
      xi <- getVar env (read i)
      xj <- getVar env (read j)
      if toNativeROp op xi xj
         then eval' (env, stack, heap) arr (read l2)
         else eval' (env, stack, heap) arr $ succ (read l1)
    Label _ (Goto (_:l)) -> eval' (env, stack, heap) arr (read l)
    Label _ Halt -> return ()
    _ -> error "Impossible! Desugaring before evaluation not sufficient!"
\end{code}

\begin{code}
--toNativeAOp :: Num a => String -> (a -> a -> a)
toNativeAOp "+" = (+)
toNativeAOp "-" = \x y -> max 0 (x - y)
toNativeAOp "*" = (*)
toNativeAOp "^" = (^)
toNativeAOp "/" = quot
toNativeAOp "%" = rem
toNativeAOp _   = error "Impossible!"
\end{code}

\begin{code}
--toNativeROp :: Num a => String -> (a -> a -> Bool)
toNativeROp "="  = (==)
toNativeROp "!=" = (/=)
toNativeROp ">"  = (>)
toNativeROp ">=" = (>=)
toNativeROp "<"  = (<)
toNativeROp "<=" = (<=)
toNativeROp _    = error "Impossible!"
\end{code}

Given a string representation of a strict Goto program and a list of
arguments parse \& evaluate the program and return either an error string or
the value of 'x0'.

\begin{code}
run :: String -> [Integer] -> Either String Integer
run = mkStdRunner parse eval
\end{code}

\begin{code}
run' :: [Integer] -> String -> Integer
run' = flip $ mkStdRunner' parse eval
\end{code}


\section{Transformation to the strict subset}
% TODO: anpassen an desugar

Sinn von stricitfy ist es wirklich auf die strikte Version von GOTO
herunterzubrechen auch wenn das für die meisten Programme einen erheblichen
Geschwindigkeitsverlust bedeutet.

\begin{code}
strictify :: Program -> Program
strictify
  =   desugar
  >>> desugarMore
  >>> addHaltOrNot
  >>> normalizeUndefLIds
  >>> normalizeLIds
  >>> normalizeVIds
  >>> deredundize
  >>> normalizeLIds
  >>> flatten
\end{code}

TODO: This is only needed because of GotoToRec.lhs, because I don't want to
strictify everything, but only the stack operations: Put it somewhere more
appropriate.

\begin{code}
desugarStack :: Program -> Program
desugarStack p
  = evalState (go p)
      TransformRecord
      { _vIdStream = vIdStream \\ (getVIds p)
      , _lIdStream = lIdStream \\ (getLIds p)
      }
  where
  go (Pop x0) = do
    p1 <- genFstCode x0  (Var "s")
    p2 <- genSndCode "s" (Var "s")
    go $ p1 <> p2
  go (Push aexp) = do
    p <- genPairCode "s" aexp (Var "s")
    go p
  go (Peek x0 aexp) = do
    s0 <- newVId
    p1 <- genSndCode s0 (Var s0)
    p2 <- mkLoop (AOp "-" (Var "sp") aexp) p1
    p3 <- genFstCode x0 (Var s0)
    go $  Assign s0 (Var "s")
       <> p2
       <> p3
  -- Mx: x0 := x0 + 0 -- Do nothing!
  go p@(Label _ (Assign "x0" (AOp "+" (Var "x0") (Num 0)))) = return p
  -- Mx: P
  go (Label l p) = go $ Label l nop <> p
  -- P1; P2;...
  go (Seq ps) = liftM (Seq . flatten') $ mapM go ps
      where
      flatten' = foldr f []
      f (Seq ps) acc = ps ++ acc
      f x        acc = x : acc
  go p = return p
\end{code}

TODO
\begin{code}
desugarHeap :: Program -> Program
desugarHeap p
  = evalState (go p)
      TransformRecord
      { _vIdStream = vIdStream \\ (getVIds p)
      , _lIdStream = lIdStream \\ (getLIds p)
      }
  where
  go (PushHeap aexp) = do
    p <- genPairCode "h" aexp (Var "h")
    go p
  go (PeekHeap x0 aexp) = do
    s0 <- newVId
    p1 <- genSndCode s0 (Var s0)
    p2 <- mkLoop (AOp "-" (Var "hp") aexp) p1
    p3 <- genFstCode x0 (Var s0)
    go $  Assign s0 (Var "h")
       <> p2
       <> p3
  -- Mx: x0 := x0 + 0 -- Do nothing!
  go p@(Label _ (Assign "x0" (AOp "+" (Var "x0") (Num 0)))) = return p
  -- Mx: P
  go (Label l p) = go $ Label l nop <> p
  -- P1; P2;...
  go (Seq ps) = liftM (Seq . flatten') $ mapM go ps
      where
      flatten' = foldr f []
      f (Seq ps) acc = ps ++ acc
      f x        acc = x : acc
  go p = return p
\end{code}

Some helper functions for the cantor encoding. Wie im Skript

\url{http://de.wikipedia.org/wiki/Cantorsche_Paarungsfunktion}

\begin{verbatim}
c(x,y)= 1/2(x+y+1)(x+y)+x
\end{verbatim}

\begin{code}
genPairCode :: VId -> AExp -> AExp -> TransformState Program
genPairCode x0 x y =
  return
    $  mempty
    <> Assign x0
       (AOp "+"
         (AOp "/"
           (AOp "*"
             (AOp "+"
               x
               (AOp "+"
                 y
                 (Num 1)
               )
             )
             (AOp "+"
               x
               y
             )
           )
           (Num 2)
         )
         x
       )
\end{code}

Wie in Aufgabenblatt 12 GTI

\begin{verbatim}
c  := 0;
x0 := 0;
LOOP x1 DO
  IF x0 = c THEN
    c  := c + 1;
    x0 := 0
  ELSE
    x0 := x0 + 1
  END
END
\end{verbatim}

\begin{code}
genFstCode :: VId -> AExp -> TransformState Program
genFstCode x0 aexp = do
  t <- newVId
  c <- newVId
  p <- mkLoop (Var t)
        $  IfElse (ROp "=" (Var x0) (Var c))
             (  Assign c (AOp "+" (Var c) (Num 1))
             <> Assign x0 (Num 0)
             )
             (  Assign x0 (AOp "+" (Var x0) (Num 1))
             )
  return
    $  Assign t aexp
    <> Assign c (Num 0)
    <> Assign x0 (Num 0)
    <> p
\end{code}

\begin{verbatim}
c  := 0;
x0 := 0;
LOOP x1 DO
  IF x0 = 0 THEN
    c  := c + 1;
    x0 := c
  ELSE
    x0 := x0 - 1
  END
END
\end{verbatim}

\begin{code}
genSndCode :: VId -> AExp -> TransformState Program
genSndCode x0 aexp = do
  t <- newVId
  c <- newVId
  p <- mkLoop (Var t)
        $  IfElse (ROp "=" (Var x0) (Num 0))
             (  Assign c (AOp "+" (Var c) (Num 1))
             <> Assign x0 (Var c)
             )
             (  Assign x0 (AOp "-" (Var x0) (Num 1))
             )
  return
    $  Assign t aexp
    <> Assign c (Num 0)
    <> Assign x0 (Num 0)
    <> p
\end{code}


Assume that program is already desugared. Here's quite some code duplication
but I can't think of a way to reduce it.

\begin{code}
desugarMore :: Program -> Program
desugarMore p
  = evalState (go p)
      TransformRecord
      { _vIdStream = vIdStream \\ (getVIds p)
      , _lIdStream = lIdStream \\ (getLIds p)
      }
  where
  -- GOTO Mx. Keep unchanged!
  go p@(Goto _) = return p
  -- HALT. Keep unchanged!
  go Halt = return Halt
  -- v0 := v1 +- c. Keep unchanged!
  go p@(Assign _ (AOp op (Var _) (Num _))) | op `elem` ["+","-"] = return p
  -- v0 := v1
  go (Assign v0 (Var v1)) = return $ Assign v0 (AOp "+" (Var v1) (Num 0))
  -- v0 := c
  go (Assign v0 (Num n)) = do
    v <- newVId
    return $ Assign v0 (AOp "+" (Var v) (Num n))
  -- v0 := v1 o v2
  go (Assign v0 (AOp op (Var v1) (Var v2))) = case op of
    "+" -> do
      loop <- mkLoop (Var v2) (Assign v0 (AOp "+" (Var v0) (Num 1)))
      go $ Assign v0 (Var v1) <> loop
    "-" -> do
      loop <- mkLoop (Var v2) (Assign v0 (AOp "-" (Var v0) (Num 1)))
      go $ Assign v0 (Var v1) <> loop
    "*" -> do
      v <- newVId
      loop <- mkLoop (Var v2) (Assign v (AOp "+" (Var v) (Var v1)))
      go $ Assign v (Num 0) <> loop <> Assign v0 (Var v)
    "^" -> do
      v <- newVId
      loop <- mkLoop (Var v2) (Assign v (AOp "*" (Var v) (Var v1)))
      go $ Assign v (Num 1) <> loop <> Assign v0 (Var v)
    "/" -> do
      c <- newVId
      loop <- mkLoop (Var v0)
        $ If (ROp ">=" (Var v0) (Var v2))
          $  Assign c  (AOp "+" (Var c) (Num 1))
          <> Assign v0 (AOp "-" (Var v0) (Var v2))
      go $  Assign c (Num 0)
         <> Assign v0 (Var v1)
         <> loop
         <> Assign v0 (Var c)
    "%" -> do
      loop <- mkLoop (Var v0)
        $ If (ROp ">=" (Var v0) (Var v2))
          $ Assign v0 (AOp "-" (Var v0) (Var v2))
      go $ Assign v0 (Var v1) <> loop
    _ -> error "Impossible! Undefined operator!"
\end{code}

Desugar `v0 := a o b` to `va := a; vb := b; v0 := va o vb` where `a` \& `b`
itself are both arithmetic expressions so that each right hand side of an
assignment has at most one operator. If either `a` is a sole variable or `b` is
a sole variable or a number do not introduce a superfluous assignment.

\begin{code}
  go (Assign v0 (AOp op (Var v) b)) = do       -- v0 := v o b =>
    vb <- newVId
    go $  Assign vb b                          -- vb := b
       <> Assign v0 (AOp op (Var v) (Var vb))  -- v0 := v o vb
  go (Assign v0 (AOp op a (Var v))) = do       -- v0 := a o v =>
    va <- newVId
    go $  Assign va a                          -- va := a
       <> Assign v0 (AOp op (Var va) (Var v))  -- v0 := va o v
  go (Assign v0 (AOp op a (Num n))) = do       -- v0 := a o n =>
    va <- newVId
    go $  Assign va a                          -- va := a
       <> Assign v0 (AOp op (Var va) (Num n))  -- v0 := va o n
  go (Assign v0 (AOp op a b)) = do             -- v0 := a o b =>
    va <- newVId
    vb <- newVId
    go $  Assign va a                          -- va := a
       <> Assign vb b                          -- vb := b
       <> Assign v0 (AOp op (Var va) (Var vb)) -- v0 := va o vb
\end{code}

\begin{code}
  -- IF xi = c THEN GOTO Mx END -- Keep that unchanged!
  go p@(If (ROp "=" (Var _) (Num _)) (Goto _)) = return p
  -- IF a o b THEN P1 END
  go (If (ROp op a b) p) = case op of
    "="  -> f 0 1 $ AOp "+" (AOp "-" a b) (AOp "-" b a)
    "!=" -> f 1 0 $ AOp "+" (AOp "-" a b) (AOp "-" b a)
    "<"  -> f 1 0 $ AOp "-" b a
    ">"  -> go $ If (ROp "<" b a) p
    "<=" -> go $ If (BOp "||" (ROp "<" a b) (ROp "=" a b)) p
    ">=" -> go $ If (BOp "||" (ROp ">" a b) (ROp "=" a b)) p
    _    -> error "Impossible! Wrong operator!"
    where
    f i0 i1 aexp = do
      v1 <- newVId
      v2 <- newVId
      loop1 <- mkLoop (Var v1) $ Assign v2 (Num i0)
      loop2 <- mkLoop (Var v2) p
      go $  Assign v1 aexp
         <> Assign v2 (Num i1)
         <> loop1
         <> loop2
  -- IF a o b THEN P1 ELSE P2 END
  go (IfElse (ROp op a b) p1 p2) = case op of
    "="  -> f 0 1 $ AOp "+" (AOp "-" a b) (AOp "-" b a)
    "!=" -> f 1 0 $ AOp "+" (AOp "-" a b) (AOp "-" b a)
    "<"  -> f 1 0 $ AOp "-" b a
    ">"  -> go $ IfElse (ROp "<" b a) p1 p2
    "<=" -> go $ IfElse (BOp "||" (ROp "<" a b) (ROp "=" a b)) p1 p2
    ">=" -> go $ IfElse (BOp "||" (ROp ">" a b) (ROp "=" a b)) p1 p2
    _    -> error "Impossible! Wrong operator!"
    where
    f i0 i1 aexp = do
      v1 <- newVId
      v2 <- newVId
      v3 <- newVId
      loop1 <- mkLoop (Var v1)
        $ Assign v2 (Num i0) <> Assign v3 (Num i1)
      loop2 <- mkLoop (Var v2) p1
      loop3 <- mkLoop (Var v3) p2
      go $  Assign v1 aexp
         <> Assign v2 (Num i1)
         <> Assign v3 (Num i0)
         <> loop1
         <> loop2
         <> loop3
  -- IF b THEN P {ELSE P2} END
  go (If (BOp "&&" a b) p) = go $ If a (If b p)
  go (IfElse (BOp "&&" a b) p1 p2) = do
    v    <- newVId
    loop <- mkLoop (Var v) p2
    go $  Assign v (Num 1)
       <> If a (If b (Assign v (Num 0) <> p1))
       <> loop
  go (If (BOp "||" a b) p) = do
    v    <- newVId
    loop <- mkLoop (Var v) p
    go $  Assign v (Num 0)
       <> If a (Assign v (Num 1))
       <> If b (Assign v (Num 1))
       <> loop
  go (IfElse (BOp "||" a b) p1 p2) = do
    v1 <- newVId
    v2 <- newVId
    loop1 <- mkLoop (Var v1) p1
    loop2 <- mkLoop (Var v2) p2
    go $  Assign v1 (Num 0)
       <> Assign v2 (Num 1)
       <> If a (Assign v1 (Num 1) <> Assign v2 (Num 0))
       <> If b (Assign v1 (Num 1) <> Assign v2 (Num 0))
       <> loop1
       <> loop2
  go (If (BNegOp bexp) p) = go $ If (invert bexp) p
  go (IfElse (BNegOp bexp) p1 p2) = go $ IfElse (invert bexp) p1 p2
\end{code}

Desugar Stack Operations using Cantor encoding

\begin{verbatim}
PUSH(c):
- s  := pair(a, s)
(- sp := sp + 1) (already done in desugar)
\end{verbatim}

\begin{code}
  go (Push aexp) = do
    p <- genPairCode "s" aexp (Var "s")
    go p
\end{code}

\begin{verbatim}
POP:
- x0 := fst(s)
- s  := snd(s)
(- sp := sp - 1) (already done in desugar)
\end{verbatim}

\begin{code}
  go (Pop x0) = do
    p1 <- genFstCode x0  (Var "s")
    p2 <- genSndCode "s" (Var "s")
    go $ p1 <> p2
\end{code}

\begin{verbatim}
PEEK(n): (n is offset from bottom of stack)
- s0 := s
- do (sp - n) times: s0 := snd(s0)
- x0 := fst(s0)
\end{verbatim}

\begin{code}
  go (Peek x0 aexp) = do
    s0 <- newVId
    p1 <- genSndCode s0 (Var s0)
    p2 <- mkLoop (AOp "-" (Var "sp") aexp) p1
    p3 <- genFstCode x0 (Var s0)
    go $  Assign s0 (Var "s")
       <> p2
       <> p3
\end{code}

Desugar Heap Operations using Cantor encoding

\begin{verbatim}
PUSH_HEAP(a):
- h  := pair(a, s)
(- hp := hp + 1) (already done in desugar)
\end{verbatim}

\begin{code}
  go (PushHeap aexp) = do
    p <- genPairCode "h" aexp (Var "h")
    go p
\end{code}

\begin{verbatim}
PEEK_HEAP(n): (n is offset from bottom of heap)
- h0 := h
- do (hp - n) times: h0 := snd(h0)
- x0 := fst(h0)
\end{verbatim}

\begin{code}
  go (PeekHeap x0 aexp) = do
    h0 <- newVId
    p1 <- genSndCode h0 (Var h0)
    p2 <- mkLoop (AOp "-" (Var "hp") aexp) p1
    p3 <- genFstCode x0 (Var h0)
    go $  Assign h0 (Var "h")
       <> p2
       <> p3
\end{code}

\begin{code}
  -- Mx: x0 := x0 + 0 -- Do nothing!
  go p@(Label _ (Assign "x0" (AOp "+" (Var "x0") (Num 0)))) = return p
  -- Mx: P
  go (Label l p) = go $ Label l nop <> p
  -- P1; P2;...
  go (Seq ps) = liftM (Seq . flatten') $ mapM go ps
    where
    flatten' = foldr f []
    f (Seq ps) acc = ps ++ acc
    f x        acc = x : acc
  -- All cases must have been checked by now.
  go _ = error $ "Not all cases were considered while desugaring Goto more!"
\end{code}
