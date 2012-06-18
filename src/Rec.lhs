\section{Rec}

\newcommand{\Rec}{\textsf{Rec} }

\Rec ist eine minimalistische funktionale Sprache deren einziger primitiver
Datentyp $\mathbb{N}$ ist. Ein \Rec Programm, das die Fakultätsfunktion
berechnet, sieht so aus:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
main(a) := fac(a);
fac(n) := if n <= 1 then 1 else n * fac(n-1)
\end{lstlisting}
\end{myindent}

Teile zu Beginn dieser Datei sind leicht an ``Implementing a functional
programming language: a tutorial'' angelehnt.

In dem folgenden Abschnitt wird das Modul, das durch diese Datei beschrieben
wird, definiert. Das Modul hat den Namen \emph{Rec} und exportiert eine Reihe
öffentlicher Funktionen, die im weitern Verlauf noch erläutert werden.

> module Rec
>   ( Program
>   , pprint
>   , parse
>   , parse'
>   , eval
>   , run
>   , run'
>   , genGoto
>   -- for testing
>   , Def
>   , Exp (..)
>   , Name
>   ) where

Da im Quellcode teilweise auf Bibliotheken und Hilfsfunktionen des Haskell
Ökosystems zugegriffen wird, müssen diese natürlich vorher importiert werden:

> import Control.Monad
> import Data.Monoid
> import Data.List ((\\), intersect, intercalate, nub, elemIndex)
> import Data.Maybe (fromJust)
> import qualified Data.Map as M
>
> import Text.Parsec ((<|>), (<?>), try, oneOf, letter, alphaNum)
> import Text.Parsec.Expr
> import Text.Parsec.String
> import qualified Text.Parsec.Token as Token
>
> import Util
> import qualified Goto as G
>
> lookup' k map = fromJust $ M.lookup k map


\subsection{Abstrakte Syntax}
\renewcommand{\l}{\langle}
\renewcommand{\r}{\rangle}

Die Syntax von \Rec ist folgendermaßen definiert:

\begin{myindent}{3mm}
\begin{tabular}{ l c l l }
$\l prog \r$  &$\to$&     $\l fn_1 \r$;$\ldots$;$\l fn_n \r$ & // $n \ge 1$\\
\\
$\l fn \r$    &$\to$&     $\l var \r$($\l var_1 \r$,$\ldots$,$\l var_n \r$)
                          := $\l exp \r$                                         & // $n \ge 0$\\
\\
$\l exp \r$   &$\to$&     $\l var \r$($\l exp_1 \r$,$\ldots$,$\l exp_n \r$)      & // Funktionsanwendung ($n \ge 0$)\\
              &$||$& $\l exp_1 \r$ $\l op \r$ $\l exp_2 \r$                 & // Infix-Funktionsanwendung\\
              &$||$& if $\l exp_1 \r$ then $\l exp_2 \r$ else $\l exp_2 \r$ & // If-Ausdruck\\
              &$||$& $\l var \r$                                            & // Variable\\
              &$||$& $\l num \r$                                            & // Zahl\\
              &$||$& ($\l exp \r$)                                          & // Geklammerter Ausdruck\\
\\
$\l op \r$    &$\to$&     $\l aop \r$ $||$ $\l rop \r$ $||$ $\l bop \r$     & // Binärer Infix-Operator\\
$\l aop \r$   &$\to$&     $+$ $||$ $-$ $||$ $\cdot$ $||$
                          $\div$ $||$ \textasciicircum $\,$$\!$ $||$ $\%$               & // Arithemtik\\
$\l rop \r$   &$\to$&     $<$ $||$ $\,$$\!$ $\le$  $||$ $\,$$\!$ $=$
                              $||$ $\,$$\!$ $\neq$ $||$ $\,$$\!$ $\ge$
                              $||$ $\, >$                                   & // Relationaler Vergleich\\
$\l bop \r$   &$\to$&     $\&\&$ $||$ $||||$                                & // Boolsche Verknüpfung\\
\\
$\l var \r$   &$\to$&     $\l apha \r$$\l varch_1 \r$$\ldots$$\l varch_n \r$     &// $n \ge 0$\\
$\l alpha \r$ &$\to$&     [A-Z] $||$ $\,$$\!$ [a-z]                         &\\
$\l varch \r$ &$\to$&     $\l apha \r$ $||$ $\,$$\!$ $\l dig \r$
                                       $||$ $\,$$\!$ \_                     &\\
\\
$\l num \r$   &$\to$&     $\l dig_1 \r$$\ldots$$\l dig_n \r$                     & // $n \ge 1$\\
$\l dig \r$   &$\to$&     [0-9]                                                  &
\end{tabular}
\end{myindent}

Des Weiteren beschreibt die folgende Tabelle die Assoziativität und Priorität
der binären Operatoren:

\begin{myindent}{3mm}
\begin{tabular}{ c || c || c }
Priorität & Assoziativität & Operator \\ \hline
    7     &     Rechts     &  \textasciicircum                \\
    6     &     Links      &  $\cdot$ $\div$                  \\
    5     &     Links      &  $+$ $-$                         \\
    4     &     Links      &  $\%$                            \\
    3     &     Keine      &  $=$ $\neq$ $>$ $\ge$ $<$ $\le$  \\
    2     &     Links      &  $\&\&$                          \\
    1     &     Links      &  $||||$                       %
\end{tabular}
\end{myindent}

Damit sich der Code, der die in der Tabelle angegebenen Eigenschaften der
Operatoren definiert, möglichst in der Nähe dieser Tabelle befindet, werden
hier schon einmal zwei Funktionen und eine Konstante
definiert\footnote{|AssocRight| etc. sind Konstruktoren, die aus Parsec
importiert sind. Üblicherweise würde man einen eigenen Datentyp |Assoc|
definieren.}:

> operators = ["+","-","*","/","^","%","=","!=","<","<=",">",">=","&&","||"]
>
> getOpPrecedence op
>   | op `elem` ["^"]                        = 7
>   | op `elem` ["*","/"]                    = 6
>   | op `elem` ["+","-"]                    = 5
>   | op `elem` ["%"]                        = 4
>   | op `elem` ["=","!=",">",">=","<","<="] = 3
>   | op `elem` ["&&"]                       = 2
>   | op `elem` ["||"]                       = 1
>   | otherwise                              = 8 -- useful for application
>
> getOpAssociativity op
>   | op `elem` ["^"]                           = AssocRight
>   | op `elem` ["=","!=",">",">=","<","<="]    = AssocNone
>   | op `elem` ["*","/","+","-","%","&&","||"] = AssocLeft
>   | otherwise                                 = error "Impossible!"

Kommen wir zurück zu \Rec Programmen. Ein \Rec Programm ($\l prog \r$) besteht
also aus einer oder mehreren Funktionsdefinitionen ($\l fn \r$), die durch ein
`\lstinline[language=Rec]$;$` getrennt sind. Es wird des Weiteren davon
ausgegangen, dass genau eine dieser Definitionen die Bezeichnung
`\lstinline[language=Rec]$main$` hat und dass es keine zwei Definitionen mit
derselben Bezeichnung gibt. Außerdem dürfen auf der rechten Seite einer
Definition nur solche Bezeichner vorkommen, die auch im Programm (ggf.
implizit) definiert sind. Hält sich ein Programm nicht an diese Bedingungen, so
ist die Auswertung des Programms undefiniert. Doch beginnen wir zunächst mit
der Definition eines \Rec Ausdrucks ($\l exp \r$), also jenes Ausdrucks, der
sich jeweils rechts vom `\lstinline[language=Rec]$:=$` einer Definition
befindet:

> data Exp
>   = Num Integer
>   | Var Name
>   | Ap Name [Exp]
>   | Lam Name Exp
>   | If Exp Exp Exp
>   deriving (Eq, Show)

Wie wird nun ein \Rec Ausdruck durch den algebraischen Datentyp |Exp|
dargestellt? Wir betrachten ein Beispiel: der \Rec Ausdruck $x + y$ wird durch
den Haskell Ausdruck |Ap "+" [Var "x", Var "y"]| dargestellt. Offensichtlich
werden Variablen durch einen |Var| Konstruktor dargestellt, der den Namen der
Variablen beinhaltet. Ein Variablenname ist nichts weiter als eine
Zeichenkette:

> type Name = String

Ein \Rec Programm ist lediglich eine Liste von Funktionsdefinitionen:

> type Program = [Def]

Eine Funktionsdefinition beinhaltet den Namen der Funktion, ihre (formalen)
Parameter\footnote{Die Parameterliste darf natürlich auch leer sein im Falle
einer Funktion ohne Parameter.} und den Funktionskörper:

> type Def = (Name, [Name], Exp)

Zum Abschluss betrachten wird noch ein kleines \Rec Programm:
%
\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
main(x) = double(21);
double(x) = x + x
\end{lstlisting}
\end{myindent}
%
Dieses Programm wird durch den folgenden Haskell Ausdruck vom Typ |Program|
repräsentiert:

\begin{spec}
  [ ("main"  , []   , (Ap "double" [Num 21]          ))
  , ("double", ["x"], (Ap "+"      [Var "x", Var "x"]))
  ]
\end{spec}


% A standard prelude
% ------------------
% 
% TODO: Better prelude with map, lists constructor etc.
% TODO: Also only include if they are used at all.
% 
% Almost every programming language provides some useful, predefined definitions
% by default. Rec is no different:
% 
%     I(x) = x;
%     K(x, y) = x;
%     K1(x, y) = y;
%     S(f, g, x) = f(x, g(x));
%     compose(f, g, x) = f(g(x));
%     twice(f) = compose(f, f)
% 
% The following definition for `preludeDefs` embodies these definitions:
% 
% > preludeDefs :: RecProgram
% > preludeDefs
% >     = [ ("I", ["x"], Var "x")
% >       , ("K", ["x","y"], Var "x")
% >       , ("K1",["x","y"], Var "y")
% >       , ("S", ["f","g","x"], Ap "f" [Var "x", Ap "g" [Var "x"]])
% >       , ("compose", ["f","g","x"], Ap "f" [Ap "g" [Var "x"]])
% >       , ("twice", ["f"], Ap "compose" [Var "f", Var "f"])
% >       ]


\subsection{Pretty Printer}

Sobald der Wert des Typs |Program| festgestellt worden ist, ist es oft
praktisch diesen lesbar zu präsentieren. Das heißt, eine \emph{Pretty Printing}
Funktion wird benötigt! Dabei sollten wir insbesondere darauf achten, dass
keine überflüssigen Klammern bei Infix-Operatoren ausgegeben werden, weshalb
wir folgende zwei Hilfsfunktionen definieren:

> parensOrNotL op l s
>   | getOpPrecedence op > getOpPrecedence l = "(" ++ s ++ ")"
>   | getOpPrecedence op < getOpPrecedence l = s
>   | otherwise = case getOpAssociativity op of
>                   AssocLeft  -> s
>                   _          -> "(" ++ s ++ ")"
>
> parensOrNotR op r s
>   | getOpPrecedence op > getOpPrecedence r = "(" ++ s ++ ")"
>   | getOpPrecedence op < getOpPrecedence r = s
>   | otherwise = case getOpAssociativity op of
>                   AssocRight  -> s
>                   _          -> "(" ++ s ++ ")"

Wir nutzen also die Informationen aus der Operatortabelle um zu entscheiden, ob
Klammern gesetzt werden müssen oder nicht. Mit diesen Funktionen können wir
schon einen Pretty Printer für \Rec Ausdrücke angeben:

> pprExp :: Exp -> String
> pprExp (Num n) = show n
> pprExp (Var v) = v
> pprExp (Ap op [a, b])
>   | op `elem` operators
>   = case [a, b] of
>       [Num  _, Num  _] -> pprExp a ++ space op ++ pprExp b
>       [Num  _, Var  _] -> pprExp a ++ space op ++ pprExp b
>       [Var  _, Num  _] -> pprExp a ++ space op ++ pprExp b
>       [Var  _, Var  _] -> pprExp a ++ space op ++ pprExp b
>       [Ap l _, Num  _] ->
>         parensOrNotL op l (pprExp a) ++ space op ++ pprExp b
>       [Ap l _, Var  _] ->
>         parensOrNotL op l (pprExp a) ++ space op ++ pprExp b
>       [Num  _, Ap r _] ->
>         pprExp a ++ space op ++ parensOrNotR op r (pprExp b)
>       [Var  _, Ap r _] ->
>         pprExp a ++ space op ++ parensOrNotR op r (pprExp b)
>       [Ap l _, Ap r _] ->
>         parensOrNotL op l (pprExp a) ++ space op ++ parensOrNotR op r (pprExp b)
>       _                ->
>         "(" ++ pprExp a ++ space op ++ pprExp b ++ ")"
>   | otherwise = op ++ "(" ++ pprExp a ++ ", " ++ pprExp b ++ ")"
>   where
>   space "^" = "^"
>   space op  = " " ++ op ++ " "
> pprExp (Ap f es)
>   = f ++ "(" ++ intercalate ", " (map pprExp es) ++ ")"
> pprExp (Lam x e)
>   = "\\" ++ x ++ ". " ++ pprExp e
> pprExp (If e1 e2 e3)
>   = "if " ++ pprExp e1
>           ++ " then " ++ pprExp e2
>           ++ " else " ++ pprExp e3

Jetzt ist es ein leichtes einen Pretty Printer für Funktionsdefinitionen und
für das gesamte Programm zu schreiben:

> pprDef :: Def -> String
> pprDef (name, args, rhs)
>   = name ++ "(" ++ intercalate ", " args ++ ")" ++ " := " ++ pprExp rhs
>
> pprint :: Program -> String
> pprint = intercalate ";\n" . map pprDef


\subsection{Parser}

% TODO: Parser should only allow programs without duplicate definitions and for a
% definition all argument names must be unique.

% TODO: Consider applicative Style where appropriate

Natürlich soll es möglich sein als Klartext vorliegende \Rec Programme als
solche zu interpretieren. In diesem Abschnitt wird deshalb ein \emph{Parser}
für \Rec Programme definiert.

Die ``Parser Combinator''-Bibliothek \emph{Parsec} wird hierfür verwendet.
Vorteile eine schon existierende Bibliothek zu verwenden sind zum Beispiel eine
integrierte Fehlerberichterstattung und eine Reihe schon vordefinierter,
nützlicher Hilfsfunktionen bzw. Kombinatoren. Außerdem können die Parser direkt
als Haskell Programme geschrieben werden - ein zusätzlicher Generierungsschritt
aus anderweitigen Beschreibungen entfällt.

Die generelle Vorgehensweise bei der Verwendung einer ``Parser
Combinator''-Bibliothek ist es einen großen Parsers aus mehreren kleinen
Parsern zusammenzufügen.

Anzumerken ist, dass kein echter, separater \emph{Lexing} Schritt gemacht wird.
Stattdessen greifen wir auf einige komfortable Hilfskonstrukte von Parsec
zurück. Im Folgenden wird ein Lexer-Stil und die verwendeten Tokens
beschrieben, sowie eine Reihe Hilfsparser definiert:

> lexDef :: Token.LanguageDef ()
> lexDef
>   = Token.LanguageDef
>   { Token.commentStart    = "/*"
>   , Token.commentEnd      = "*/"
>   , Token.commentLine     = "//"
>   , Token.nestedComments  = True
>   , Token.identStart      = letter
>   , Token.identLetter     = alphaNum <|> oneOf "_"
>   , Token.opStart         = Token.opLetter lexDef
>   , Token.opLetter        = oneOf (concat (Token.reservedOpNames lexDef))
>   , Token.reservedOpNames = ":=" : operators
>   , Token.reservedNames   = [ "if", "then", "else" ]
>   , Token.caseSensitive   = True
>   }
>
> lexer = Token.makeTokenParser lexDef
>
> parens     = Token.parens lexer
> semiSep1   = Token.semiSep1 lexer
> commaSep   = Token.commaSep lexer
> reserved   = Token.reserved lexer
> reservedOp = Token.reservedOp lexer
> identifier = Token.identifier lexer
> natural    = Token.natural lexer
> whiteSpace = Token.whiteSpace lexer
> symbol     = Token.symbol lexer

Kommen wir nun zum eigentlichen Parsen. Die Funktion |parse| erhält als Eingabe
den Programmtext und liefert entweder den entsprechenden Wert vom Typ |Program|
oder eine Fehlermeldung. Um die Stellen im Quellcode, an denen eine
Fehlerbehandlung notwendig ist um Laufzeitfehlern vorzubeugen, zur
Kompilierzeit feststellbar zu machen, wird der Rückgabewert im |Either|
Datentyp umschlossen. |mkStdParser| ist hierbei eine importierte Hilfsfunktion.
Im Gegensatz zu |parse| wirft |parse'| einen Laufzeitfehler bei invalider
Eingabe.

> parse :: String -> Either String Program
> parse = mkStdParser pProgram () whiteSpace
>
> parse' :: String -> Program
> parse' = mkStdParser' pProgram () whiteSpace

Rufen wir uns noch einmal die Syntaxdefinition von \Rec ins Gedächtnis. Diese
können ziemlich direkt nach Haskell übersetzt werden. Betrachten wir zum
Beispiel die Produktionen für @<prog>@ und @<fn>@:

\begin{myindent}{3mm}
\begin{tabular}{ l c l l }
$\l prog \r$  &$\to$&     $\l fn_1 \r$;$\ldots$;$\l fn_n \r$ & // $n \ge 1$\\
$\l fn \r$    &$\to$&     $\l var \r$($\l var_1 \r$,$\ldots$,$\l var_n \r$)
\end{tabular}
\end{myindent}

Diese werden folgendermaßen übersetzt:

> pProgram :: Parser Program
> pProgram = semiSep1 pFn
>
> pFn :: Parser Def
> pFn = do
>   name <- identifier
>   args <- parens $ commaSep identifier
>   reservedOp ":="
>   expr <- pExp
>   return $ (name, args, expr)

Leider kann nicht jede Produktion so direkt übersetzt werden - insbesondere im
Falle von Linksrekursion. Zur Veranschaulichung soll die Produktion für
Infix-Funktionsanwendungen herhalten:

\begin{myindent}{3mm}
\begin{tabular}{ l c l l }
$\l exp \r$ &$\to$&  $\l exp_1 \r$ $\l op \r$ $\l exp_2 \r$     &
\end{tabular}
\end{myindent}

Falls die Produktion einfach zu |pExp = liftM Ap $ pExp pOp pExp| übersetzt
werden würde, dann würde |pExp| leider nie terminieren, da es sich unaufhörlich
selbst aufrufen würde. Zum Glück ist es üblicherweise möglich eine Grammatik
für Programmiersprachen so umzuformen, dass sie nicht mehr linksrekursiv ist,
aber immernoch dieselbe Sprache erzeugt. Jedoch ist diese Umformung recht
mühsam. Parsec schafft aber auch hier Abhilfe und zwar mit dem Hilfskonstrukt
|buildExpressionParser|.

|buildExpressionParser| erhalt als Eingabe eine Tabelle von Operatorparsern (in
der Reihenfolge der Operatorpriorität) und erzeugt einen Parser für Ausdrücke,
der die angegebene Priorität und Assoziativität korrekt beachtet. Wir erinnern
uns zudem an die Operatortabelle von \Rec$\!\!$; diese kann ziemlich direkt
übersetzt werden:

> pExp :: Parser Exp
> pExp = buildExpressionParser opTable pTerm
>   where
>   opTable =
>     [ [ op "^"  AssocRight ]
>     , [ op "*"  AssocLeft, op "/"  AssocLeft ]
>     , [ op "+"  AssocLeft, op "-"  AssocLeft ]
>     , [ op "%"  AssocLeft ]
>     , [ op "="  AssocNone
>       , op "!=" AssocNone
>       , op "<"  AssocNone
>       , op "<=" AssocNone
>       , op ">"  AssocNone
>       , op ">=" AssocNone
>       ]
>     , [ op "&&" AssocRight ]
>     , [ op "||" AssocRight ]
>     ]
>   op name = Infix (reservedOp name >> return (\x y -> Ap name [x, y]))
>
> pTerm = pLam <|> try pAp <|> try pIf <|> pVar <|> pNum <|> parens pExp <?> "term"

Hier nun noch die restlichen Parser:

> pLam = do
>   _ <- symbol "\\"
>   x <- identifier
>   _ <- symbol "."
>   e <- pExp
>   return $ Lam x e
>
> pAp = do
>   fn <- identifier
>   args <- parens $ commaSep pExp
>   return $ Ap fn args
>
> pIf = do
>   reserved "if"
>   e1 <- pExp
>   reserved "then"
>   e2 <- pExp
>   reserved "else"
>   e3 <- pExp
>   return $ If e1 e2 e3
>
> pVar :: Parser Exp
> pVar = liftM Var (identifier <?> "variable")
>
> pNum :: Parser Exp
> pNum = liftM Num (natural <?> "number")


\subsection{Übersetzung nach Goto}

In diesem Teil wird die Generierung eines semantisch äquivalenten Goto
Programms aus einem gegebenen \Rec Programm beschrieben. Es wird versucht die
Überlegungen hinter dem Nutzen einer jeden definierten Funktion hinsichtlich
des Ziels der Übersetzung immerhin knapp darzustellen. Doch zuerst sollen
einige Hilfsfunktionen eingeführt werden.

|getDefNames| liefert bei Eingabe eines \Rec Programms einfach die Namensliste
der im Programm definierten Funktionen. Analog liefert |getDefRhss| eine Liste
aller Ausdrücke die sich in den Funktionskörpern befinden und zwar entspricht
ein Listeneintrag der rechten Seite einer Funktionsdefinition. `Rhss` steht
hierbei für `Right hand sides`.

> getDefNames :: Program -> [Name]
> getDefNames p = [ name | (name, _, _) <- p ]
>
> getDefRhss :: Program -> [Exp]
> getDefRhss p = [ exp | (_, _, exp) <- p ]

|getCalledFnNames| liefert bei Eingabe einer Liste von Ausdrücken eine Liste
von allen Bezeichnern, die innerhalb der Ausdrücke vorkommen. Diese Funktion
wird später unter anderem dafür nützlich sein, Spezialcode nativer Operationen
wie `$+$` nur dann in das generierte Goto Programm einzubinden, wenn sie auch
wirklich benutzt worden sind.

> getCalledFnNames :: [Exp] -> [Name]
> getCalledFnNames [] = []
> getCalledFnNames (Num _:rest) = getCalledFnNames rest
> getCalledFnNames (Var _:rest) = getCalledFnNames rest
> getCalledFnNames (Ap fn args:rest)
>   = fn : getCalledFnNames args ++ getCalledFnNames rest
> getCalledFnNames (Lam _ e:rest)
>   = getCalledFnNames [e] ++ getCalledFnNames rest
> getCalledFnNames (If e1 e2 e3:rest)
>   = getCalledFnNames [e1, e2, e3] ++ getCalledFnNames rest

|findDef| liefert bei Eingabe eines Funktionsnamens und eines \Rec Programms
die vollständige Definition der gesuchten Funktion. Es wird davon ausgegangen,
dass |findDef| im Quellcode nur so benutzt wird, dass ein Nichtfinden einer
Definition nicht möglich ist. Im Grunde wird |findDef| nur benötigt, um die
Definition der `\lstinline[language=Rec]$main$` Funktion zu finden, da ja nicht
vorgeschrieben ist, an welcher Stelle im Quellcode sich die Definition befinden
muss.

> findDef :: Name -> Program -> Def
> findDef _ [] = error "Impossible! Definition not found!"
> findDef n ((n', args, exp):ps)
>   | n == n'   = (n', args, exp)
>   | otherwise = findDef n ps

Um eine einzelne Funktionsdefinition zu übersetzen muss diese Funktion
``irgendwie'' auf ein Goto Programm abgebildet werden. Dazu erhält der
Abschnitt einer Funktionsdefinition auf jeden Fall ein Label. In diesem
Abschnitt müssen die Argumente bzw. Parameterwerte geholt werden, sowie der
Definitionsrumpf übersetzt werden. Schließlich muss an die Stelle
zurückgesprungen werden, an der die Funktion aufgerufen wurde. Hier ein
Beispiel:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
double(n) := n + n
\end{lstlisting}
\end{myindent}
wird übersetzt zu

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
double: a1 := PEEK fp - 1;
         PUSH a1;
         PUSH a1;
         CALL add, 2;
         a1 := PEEK fp - 1;
         RETURN
\end{lstlisting}
\end{myindent}

Zu Beginn der Abbildung muss also eine Sequenz erzeugt werden, die die
Funktionsargumente in den konkreten Variablen
`\lstinline[language=Goto]$a1,a2,...$` zwischenspeichert. |genArgSequence|
leistet dieses:

> genArgSequence :: Int -> G.Program
> genArgSequence 0 = mempty
> genArgSequence numberOfArgs
>   = G.Seq $ map f [1..numberOfArgs]
>   where
>   f i = G.Peek ('a':show i)
>         $ G.AOp "-" (G.Var "fp") (G.Num $ toInteger (numberOfArgs - i + 1))

Wenn wir uns dazu auf machen den Funktionsrumpf zu übersetzen, dann müssen
wir den formalen Parametern einer \Rec Definition positionell die
entsprechenden Goto Variablen `\lstinline[language=Goto]$a1,a2,...$` zuordnen.
|mkParamMap| erzeugt diese Zuordnung:

> type ParamMap = M.Map Name Name
>
> mkParamMap :: [Name] -> ParamMap
> mkParamMap args = M.fromList $ zip args $ map ("a"++) $ map show [1..]

Nun widmen wir uns dem Kern der Übersetzung; der Übersetzung eines \Rec
Ausdrucks. Die ersten beiden Parameter von |genCallSequence| sind eine Liste
der im Programm definierten Funktionsnamen und die Abbildung von Parametern der
zum Ausdruck gehörigen Funktionsdefinition (auf der linken Seite) auf die
entsprechenden Variablen `\lstinline[language=Goto]$a1,a2,...$`. Wie auch sonst
dient der erste Parameter dazu den Abschnitten für etwaige benutzte Operatoren
alphanumerische Namen geben zu können. Der dritte Parameter von
|genCallSequence| ist nicht einfach ein Ausdruck, sondern eine Liste von
Ausdrücken. Das hat einfach den Grund, dass |genCallSequence| auch für die
Argumentliste des |AP| Konstruktors verwendet wird und ohne die
Verlistifizierung würden zwei Funktionen benötigt werden, die ohnehin fast das
selbe leisten würden.

> genCallSequence :: [Name] -> ParamMap -> [Exp] -> G.Program
> genCallSequence _ _ []
>   = mempty
> genCallSequence fnNames paramMap (Num n : rest)
>   =  G.Push (G.Num n)
>   <> genCallSequence fnNames paramMap rest

Assoziiere Nummern zu Funktionsdefinitionen, d.h. Zeiger auf
Funktionsdefinitionen, sodass im GOTO-Programm anhand dieser Nummer auf eine
bestimmte Funktionsdefinition gesprungen werden kann.

> genCallSequence fnNames paramMap (Var a : rest)
>   =  case M.lookup a paramMap of
>        Just x  -> G.Push $ G.Var x
>        Nothing -> G.Push $ G.Num $ toInteger $ fromJust $ elemIndex a fnNames
>   <> genCallSequence fnNames paramMap rest

Die Übersetzung atomarer Ausdrücke gestaltet sich problemlos. Bei der
Übersetzung eines Funktionsaufrufs muss jedoch unbedingt darauf geachtet werden
nach der |CALL| Instruktion die Variablen `\lstinline[language=Goto]
$a1,a2,...$` zurückzusetzen, da ihre Werte beim Aufruf von anderen
Funktionen\footnotemark $ $ ersetzt werden können.

\footnotetext{
Man könnte sich vielleicht überlegen für jeden übersetzten Funktionsabschnitt
andere temporäre Variablen zu verwenden (also nicht immer
`\lstinline[language=Goto] $a1,a2,...$` sondern z.B. auch
`\lstinline[language=Goto] $b1,b2,...$`), was auf den ersten Blick ein
Zurücksetzen überflüssig machen würde. Spätestens jedoch bei rekursiven
Aufrufen sollte auffallen, dass das so nicht funktioniert.
}

> genCallSequence fnNames paramMap (Ap fn args : rest)
>   =  genCallSequence fnNames paramMap args
>   <> G.Call (labelizeIfOp fnNames fn) (length args)
>   <> genArgSequence (M.size paramMap) -- reset args
>   <> genCallSequence fnNames paramMap rest

Zur Erinnerung: \emph{false} wird in \Rec als $0$ kodiert und alle anderen
Werte sind \emph{true}. Deshalb wird der pseudo \Rec Ausdruck
`\lstinline[language=Rec]$if 0 then a else b$' in den pseudo Goto Ausdruck
`\lstinline[language=Rec]$IF 0 != 0 THEN a ELSE b END$' übersetzt. Analog für
einzelne Variablen:

> genCallSequence fnNames paramMap (If (Num n) e2 e3 : rest)
>   =  G.IfElse (G.ROp "!=" (G.Num n) (G.Num 0))
>               (genCallSequence fnNames paramMap [e2])
>               (genCallSequence fnNames paramMap [e3])
>   <> genCallSequence fnNames paramMap rest
> genCallSequence fnNames paramMap (If (Var a) e2 e3 : rest)
>   =  G.IfElse (G.ROp "!=" (G.Var $ lookup' a paramMap) (G.Num 0))
>               (genCallSequence fnNames paramMap [e2])
>               (genCallSequence fnNames paramMap [e3])
>   <> genCallSequence fnNames paramMap rest

Im Allgemeinen kann sich im If-Kopf ein beliebiger \Rec Ausdruck befinden,
weshalb sich in diesem Fall die Übersetzung etwas anders gestaltet. Und zwar
werden die relationalen bzw. boolschen Operatoren als Funktionen interpretiert,
die entweder eine $0$ (\emph{false}) oder eine beliebige andere Zahl wie zum
Beispiel $1$ (\emph{true}) liefern:

> genCallSequence fnNames paramMap (If e1 e2 e3 : rest)
>   =  genCallSequence fnNames paramMap [e1]
>   <> G.Pop "t" -- return value of fn
>   <> G.IfElse (G.ROp "!=" (G.Var "t") (G.Num 0))
>               (genCallSequence fnNames paramMap [e2])
>               (genCallSequence fnNames paramMap [e3])
>   <> genCallSequence fnNames paramMap rest

Nun können wir endlich die Funktion beschreiben, die eine einzelne \Rec
Definition übersetzt. Zu beachten ist, dass am Ende eines Abschnitts eine
`\lstinline[language=Goto]$RETURN$` Anweisung vorhanden sein muss um zum
\emph{Caller} zurückzuspringen:

> genDefSection :: [Name] -> Def -> G.Program
> genDefSection fns (name, args, exp)
>   = G.Label name
>     $  genArgSequence (length args)
>     <> genCallSequence fns (mkParamMap args) [exp]
>     <> G.Return

Wir haben bisher die Übersetzung nativer Operatoren etwas vernachlässigt.
Ein \Rec Ausdruck wie `\lstinline[language=Rec]$2 + 3$' könnte eins zu eins in
eine Goto Addition übersetzt werden. Leider gilt dies nicht für einen Ausdruck
wie `\lstinline[language=Rec]$f(2, 3) + a$', weil erst der Rückgabewert des
Funktionsaufrufes berechnet werden muss, was nicht direkt in Goto darstellbar
ist. Aus diesem Grund und aus Gründen der Einheitlichkeit werden native
Operatoren fast wie benutzerdefinierte Funktionen übersetzt. Und zwar erhält
jeder Operator seinen eigenen Abschnitt im generierten Goto Programm. Die
`\lstinline[language=Rec]$+$'-Funktion wird z.B. so übersetzt

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
plus: a1 := PEEK fp - 2;
       a2 := PEEK fp - 1;
       PUSH a1 + a2;
       RETURN
\end{lstlisting}
\end{myindent}

, die `\lstinline[language=Rec]$<$'-Funktion so

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
leq: a1 := PEEK fp - 2;
      a2 := PEEK fp - 1;
      IF a1 < a2 THEN
        PUSH 1
      ELSE
        PUSH 0
      END
      RETURN
\end{lstlisting}
\end{myindent}

und die `\lstinline[language=Rec]$&&$'-Funktion so

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
and: a1 := PEEK fp - 2;
      a2 := PEEK fp - 1;
      IF a1 != 0 && a2 != 0 THEN
        PUSH 1
      ELSE
        PUSH 0
      END
      RETURN
\end{lstlisting}
\end{myindent}

Zu beachten ist, dass nur diejenigen Abschnitte in das generierte Goto Programm
eingefügt werden, deren zugehöriger Operator auch wirklich im \Rec
Ausgangsprogramm benutzt worden ist. Der Sinn dabei ist es, das generierte
Programm nicht unnötig lang werden zu lassen.

> genOpSection :: [Name] -> [Exp] -> G.Program
> genOpSection fns exps
>   =  foldr (<>) mempty (map genAorRorBSection calledOps)
>   where
>   calledFns = getCalledFnNames exps
>   calledOps = nub $ calledFns `intersect` operators
>   genAorRorBSection op
>     | op `elem` ["+","-","*","/","^","%"]    = genArithSection op
>     | op `elem` ["=","!=","<","<=",">",">="] = genRelSection   op
>     | op `elem` ["&&","||"]                  = genBoolSection  op
>     | otherwise                              = error "Impossible!"
>   genArithSection op
>     = G.Label (labelizeIfOp fns op)
>       $  genArgSequence 2
>       <> G.Push (G.AOp op (G.Var "a1") (G.Var "a2"))
>       <> G.Return
>   genRelSection op
>     = G.Label (labelizeIfOp fns op)
>       $  genArgSequence 2
>       <> G.IfElse (G.ROp op (G.Var "a1") (G.Var "a2"))
>                   (G.Push (G.Num 1))
>                   (G.Push (G.Num 0))
>       <> G.Return
>   genBoolSection op
>     = G.Label (labelizeIfOp fns op)
>       $  genArgSequence 2
>       <> G.IfElse (G.BOp op
>                      (G.ROp "!=" (G.Var "a1") (G.Num 0))
>                      (G.ROp "!=" (G.Var "a2") (G.Num 0)))
>                   (G.Push (G.Num 1))
>                   (G.Push (G.Num 0))
>       <> G.Return

Da z.B. `$+$' nicht direkt als Labelbezeichner in Goto möglich ist, müssen
eindeutige, alphanumerische Namen für die Operatoren im Goto Programm erzeugt
werden. Dieses leistet |labelizeIfOp|\footnote{ Da die Funktion oft mit den
selben Argumente aufgerufen wird, würde sich im Allgemeinen \emph{Memoization}
lohnen, hier verzichten wir aber darauf, da die Eingabeprogrammtexte sehr klein
sind.}:

> labelizeIfOp :: [Name] -> Name -> Name
> labelizeIfOp defNames op
>   | op `elem` operators = head $ mkLIdStream (opToLabel op) \\ defNames
>   | otherwise           = op -- op isn't operator symbol
>   where
>   mkLIdStream l = l : map (l++) (map show [1..])
>   opToLabel op  = lookup' op $ M.fromList
>     [ ("+" , "add"), ("-" , "sub")
>     , ("*" , "mul"), ("/" , "div")
>     , ("^" , "exp"), ("%" , "mod")
>     , ("=" , "eq" ), ("!=", "neq")
>     , ("<" , "lt" ), ("<=", "leq")
>     , (">" , "gt" ), (">=", "geq")
>     , ("&&", "and"), ("||", "or" )
>     ]

Bisher haben wir uns ebenfalls noch nicht überlegt wie externe Parameter
behandelt werden sollen und überhaupt der Anfang des übersetzten Programs
aussehen soll. Das gestaltelt sich jedoch recht einfach. Die $n$ Parameter der
`\lstinline[language=Rec]$main$'-Funktion sollen bekanntlich mit den Werten der
Variablen `\lstinline[language=Rec]$x1,...,xn$' ersetzt werden. Dazu werden
diese Variablen am Anfang des Programms einfach in den Stack gepusht. Da
`\lstinline[language=Rec]$main$' der Eintrittspunkt der Auswertung ist, wird
als nächstes der `\lstinline[language=Rec]$main$'-Abschnitt ``gecallt''. Wir
wissen, dass sich nach der Auswertung das Ergebnis als oberstes Element auf dem
Stack befindet. Dieses wird einfach gepoppt und in
`\lstinline[language=Goto]$x0$' gespeichert. Schliesslich wird die Rechnung mit
dem `\lstinline[language=Goto]$HALT$'-Befehl beendet:

> genExtArgsSection :: Program -> G.Program
> genExtArgsSection defs
>   =  G.Seq (map (\i -> G.Push (G.Var $ 'x':show i)) [1..argsLen])
>   <> G.Assign "fp" (G.AOp "+" (G.Var "sp") (G.Num 1)) -- TODO: Explain why important
>   <> G.Call "main" (length mainArgs)
>   <> G.Pop "x0"
>   <> G.Halt
>   where
>   (_, mainArgs, _) = findDef "main" defs
>   argsLen = length mainArgs

Endlich nun können wir alle Teile zusammenfügen und die Funktion |genGoto|
definieren, die ein beliebiges \Rec Programm in ein semantisch äquivalentes
Goto Programm übersetzt:

> genGoto :: Program -> G.Program
> genGoto defs = mempty
>  <> genExtArgsSection defs
>  <> G.Seq (map (genDefSection defNames) defs)
>  <> genOpSection defNames defRhss
>  where
>  defNames = getDefNames defs
>  defRhss  = getDefRhss  defs

Zum Abschluss wollen wir uns die vollständige Übersetzung nach Goto des
folgenden \Rec Programms anschauen:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
main(a) := fac(a);
fac(n)  := if n <= 1 then 1 else n * fac(n-1)
\end{lstlisting}
\end{myindent}
wird übersetzt zu

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
PUSH x1;
fp := sp + 1;
CALL main, 1;
x0 := POP;
HALT;

main: a1 := PEEK fp - 1;
       PUSH a1;
       CALL fac, 1;
       a1 := PEEK fp - 1;
       RETURN;

fac: a1 := PEEK fp - 1;
      PUSH a1;
      PUSH 1;
      CALL leq, 2;
      a1 := PEEK fp - 1;
      t := POP;
      IF t != 0 THEN
        PUSH 1
      ELSE
        PUSH a1;
        PUSH a1;
        PUSH 1;
        CALL sub, 2;
        a1 := PEEK fp - 1;
        CALL fac, 1;
        a1 := PEEK fp - 1;
        CALL mul, 2;
        a1 := PEEK fp - 1
      END;
      RETURN;

leq: a1 := PEEK fp - 2;
      a2 := PEEK fp - 1;
      IF a1 <= a2 THEN
        PUSH 1
      ELSE
        PUSH 0
      END;
      RETURN;

mul: a1 := PEEK fp - 2;
      a2 := PEEK fp - 1;
      PUSH a1 * a2;
      RETURN;

sub: a1 := PEEK fp - 2;
      a2 := PEEK fp - 1;
      PUSH a1 - a2;
      RETURN
\end{lstlisting}
\end{myindent}


\subsection{Auswertung}

In diesem Abschnitt werden die Funktionen zur direkten Auswertung eines \Rec
Programms definiert.

|eval| erhält als Eingabe ein \Rec Program vom Typ |Program| und eine Liste von
Eingabeparametern. Nach erfolgreicher Auswertung wird, falls das Programm nicht
in eine Endlosschleife geraten ist, die Ergebniszahl zurückgegeben. Und zwar
geschieht die Auswertung so: Zuerst wird aus dem \Rec Programm das
entsprechende Goto Programm generiert und anschließend wird das Goto Programm
mit dem Goto Auswerter ausgewertet.

> eval :: Program -> [Integer] -> Integer
> eval p input = G.eval (genGoto p) input

|run| verknüpft die beiden Funktionen |parse| und |eval|. Im Gegensatz zu
|eval| erwartet |run| also ein \Rec Programm im Klartext und liefert entweder
die Ergebniszahl oder einen Fehlernachricht. |run'| dagegen liefert bei
jeglichem (Parse-)Fehler als Ergebnis die Zahl $-1$.

> run :: String -> [Integer] -> Either String Integer
> run = mkStdRunner parse eval
>
> run' :: String -> [Integer] -> Integer
> run' = mkStdRunner' parse eval
