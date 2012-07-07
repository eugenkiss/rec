Only because ghc can't cycle!

\begin{code}
module GotoToRec (genRec) where
\end{code}

\begin{code}
import Goto
import qualified Rec as R
\end{code}

\section{Übersetzung nach Rec}

\begin{code}
genRec :: Program -> R.Program
genRec p = mainDef : map go ps'
  where
  Seq ps' = case (simplify . desugarHeap . desugarStack . simplify) p of
              Seq ps -> Seq ps
              x      -> Seq [x]
  l  = length $ getVIds $ Seq ps'
  params = map ("x"++) $ map show ([1..l-1] ++ [0])
  params' = map R.Var params
  succLId [] = error "Impossible"
  succLId (_:i) = "M" ++ show ((read i) + 1)
  mainDef = ("main", params, R.Ap "M1" params')
  go (Label l e) = (l, params, go' l e)
  go _ = error "Impossible, as strict GOTO program is assumend"
  -- HALT. Keep unchanged!
  go' _ Halt = R.Var "x0"
  -- GOTO Mx. Keep unchanged!
  go' _ (Goto l') = R.Ap l' params'
  -- v0 := v1 o c. Keep unchanged!
  go' l (Assign (_:i) (AOp op (Var y) (Num n)))
    = R.Ap (succLId l) $ replaceVar (read i) params' $ R.Ap op [R.Var y, R.Num n]
  go' l (Assign (_:i) (AOp op (Var y) (Var z)))
    = R.Ap (succLId l) $ replaceVar (read i) params' $ R.Ap op [R.Var y, R.Var z]
  go' l (If (ROp op (Var x) (Num n)) (Goto l'))
    = R.If (R.Ap op [R.Var x, R.Num n]) (R.Ap l' params') (R.Ap (succLId l) params')
  go' l (If (ROp op (Var x) (Var y)) (Goto l'))
    = R.If (R.Ap op [R.Var x, R.Var y]) (R.Ap l' params') (R.Ap (succLId l) params')
  go' _ _ = error "Impossible, as strict GOTO program is assumend"
\end{code}

\begin{code}
replaceVar 0 xs new = init xs ++ [new]
replaceVar i xs new = replaceNth (i-1) xs new

replaceNth _ [] _ = []
replaceNth n (x:xs) newVal
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) xs newVal
\end{code}
