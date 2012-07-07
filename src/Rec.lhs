\chapter{Rec}

REC ist eine minimalistische funktionale Sprache. REC's einziger
\emph{primitiver} Datentyp ist $\mathbb{N}$, jedoch können selbstverständlich
Funktionen wie Werte behandelt werden - \emph{higher-order functions} werden
also unterstützt.  Ein beispielhaftes REC Programm, das die Fakultätsfunktion
berechnet, sieht folgendermaßen aus:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
main(a) := fac(a);
fac(n) := if n <= 1 then 1 else n * fac(n-1)
\end{lstlisting}
\end{myindent}

In dieser Datei werden wir einen vollständigen Compiler für REC angeben, der
REC in die Sprache GOTO übersetzt. Dazu werden wir uns eine Repräsentation von
REC Programmen in Haskell mittels algebraischen Datentypen überlegen, sowie
einen Parser erstellen. Darüber hinaus werden wir einen Pretty Printer für REC
schreiben und einige zusätzliche Hilfsmethoden. Noch eine Anmerkung: Teile zu
Beginn dieser Datei sind leicht an ``Implementing a functional programming
language: a tutorial'' angelehnt.

In dem folgenden Abschnitt wird das Modul, das durch diese Datei beschrieben
wird, definiert. Das Modul hat den Namen \emph{Rec} und exportiert eine Reihe
öffentlicher Funktionen, die im weitern Verlauf noch erläutert werden.

\begin{code}
module Rec
  ( Program
  , pprint
  , parse
  , parse'
  , eval
  , run
  , run'
  , genGoto
  -- wegen Testfaellen
  , Def
  , Exp (..)
  , Name
  ) where
\end{code}

Da wir im Quellcode teilweise auf Bibliotheken und Hilfsfunktionen des Haskell
Ökosystems zugreifen, müssen wir diese natürlich vorher importieren:

\begin{code}
import Control.Monad
import Data.Monoid
import Data.Functor
import Data.List ((\\), intersect, intercalate, nub, genericLength, sort)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec hiding (Parser, State, labels, parse)
import Text.ParserCombinators.Parsec.Expr

import Util
import qualified Goto as G
\end{code}

Hier definieren wir uns kurz eine Hilfsfunktion |lookup'|, die im Gegensatz zu
|M.lookup| bei Nichtfinden eines Schlüssels das Program zum Abbrechen zwingt.
|lookup'| benötigt weniger Boilerplate bei der Verwendung und wird ohnehin nur
an Stellen im Programm benutzt, bei denen ein Nichtfinden unmöglich ist, oder
wo es keine sinnvolle Fehlerbehandlung gibt bzw. sie überflüssig ist:

\begin{code}
lookup' k map = fromJust $ M.lookup k map
\end{code}

\section{Abstrakte Syntax}
\renewcommand{\l}{\langle}
\renewcommand{\r}{\rangle}

Die Syntax von REC ist folgendermaßen definiert:

\begin{myindent}{3mm}
\begin{tabular}{ l c l l }
$\l prog \r$  &$\to$&     $\l fn_1 \r$;$\ldots$;$\l fn_n \r$ & // $n \ge 1$\\
\\
$\l fn \r$    &$\to$&     $\l var \r$($\l var_1 \r$,$\ldots$,$\l var_n \r$)
                          := $\l exp \r$                                         & // $n \ge 0$\\
\\
$\l exp \r$   &$\to$&     $\l var \r$($\l exp_1 \r$,$\ldots$,$\l exp_n \r$) & // Funktionsanwendung ($n \ge 0$)\\
              &$||$& ($\l exp \r$)($\l exp \r$)                              & // Fkt.Anw. höherer Ordnung\\
              &$||$& $\lambda$$\l var \r$$.$ $\l exp \r$                     & // Lambda-Ausdruck\\
              &$||$& $\l exp_1 \r$ $\l op \r$ $\l exp_2 \r$                  & // Infix-Funktionsanwendung\\
              &$||$& if $\l exp_1 \r$ then $\l exp_2 \r$ else $\l exp_3 \r$  & // If-Ausdruck\\
              &$||$& $\l var \r$                                             & // Variable\\
              &$||$& $\l num \r$                                             & // Zahl\\
              &$||$& ($\l exp \r$)                                           & // Geklammerter Ausdruck\\
\\
$\l op \r$    &$\to$&     $\l aop \r$ $||$ $\l rop \r$ $||$ $\l bop \r$       & // Binärer Infix-Operator\\
$\l aop \r$   &$\to$&     $+$ $||$ $-$ $||$ $\cdot$ $||$
                          $\div$ $||$ \textasciicircum $\,$$\!$ $||$ $\%$     & // Arithemtik\\
$\l rop \r$   &$\to$&     $<$ $||$ $\,$$\!$ $\le$  $||$ $\,$$\!$ $=$
                              $||$ $\,$$\!$ $\neq$ $||$ $\,$$\!$ $\ge$
                              $||$ $\, >$                                    & // Relationaler Vergleich\\
$\l bop \r$   &$\to$&     $\&\&$ $||$ $||||$                                   & // Boolsche Verknüpfung\\
\\
$\l var \r$   &$\to$&     $\l apha \r$$\l varch_1 \r$$\ldots$$\l varch_n \r$     &// $n \ge 0$\\
$\l alpha \r$ &$\to$&     [A-Z] $||$ $\,$$\!$ [a-z]                          &\\
$\l varch \r$ &$\to$&     $\l apha \r$ $||$ $\,$$\!$ $\l dig \r$
                                       $||$ $\,$$\!$ \_                      &\\
\\
$\l num \r$   &$\to$&     $\l dig_1 \r$$\ldots$$\l dig_n \r$                &// $n \ge 1$\\
$\l dig \r$   &$\to$&     [0-9]                                             &
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
    1     &     Links      &  $||$                       %
\end{tabular}
\end{myindent}

Damit sich der Code, der die in der Tabelle angegebenen Eigenschaften der
Operatoren definiert, möglichst in der Nähe dieser Tabelle befindet, werden
hier schon einmal zwei Funktionen und eine Konstante
definiert\footnote{|AssocRight| etc. sind Konstruktoren, die aus Parsec
importiert sind. Üblicherweise würde man einen eigenen Datentyp |Assoc|
definieren.}:

\begin{code}
operators = ["+","-","*","/","^","%","=","!=","<","<=",">",">=","&&","||"]
getOpPrecedence op
  | op `elem` ["^"]                        = 7
  | op `elem` ["*","/"]                    = 6
  | op `elem` ["+","-"]                    = 5
  | op `elem` ["%"]                        = 4
  | op `elem` ["=","!=",">",">=","<","<="] = 3
  | op `elem` ["&&"]                       = 2
  | op `elem` ["||"]                       = 1
  | otherwise                              = 8 -- useful for application
getOpAssociativity op
  | op `elem` ["^"]                           = AssocRight
  | op `elem` ["=","!=",">",">=","<","<="]    = AssocNone
  | op `elem` ["*","/","+","-","%","&&","||"] = AssocLeft
  | otherwise                                 = error "Impossible!"
\end{code}

Kommen wir zurück zu REC Programmen. Ein REC Programm ($\l prog \r$) besteht
also aus einer oder mehreren Funktionsdefinitionen ($\l fn \r$), die durch ein
`\lstinline[language=Rec]$;$' getrennt sind. Es wird des Weiteren davon
ausgegangen, dass genau eine dieser Definitionen die Bezeichnung
`\lstinline[language=Rec]$main$' hat und dass es keine zwei Definitionen mit
derselben Bezeichnung gibt. Außerdem dürfen auf der rechten Seite einer
Definition nur solche Bezeichner vorkommen, die auch im Programm (ggf.
implizit) definiert sind. Hält sich ein Programm nicht an diese Bedingungen, so
ist die Auswertung des Programms undefiniert. Doch beginnen wir zunächst mit
der Definition eines REC Ausdrucks ($\l exp \r$), also jenes Ausdrucks, der
sich jeweils rechts vom `\lstinline[language=Rec]$:=$' einer Definition
befindet:

\begin{code}
data Exp
  = Num Integer
  | Var Name
  | Ap Name [Exp]
  | HAp Exp Exp
  | Lam Int Name Exp
  | If Exp Exp Exp
  deriving (Eq, Show)
\end{code}

|Num| steht für eine Zahl, |Var| für eine Variable. |Ap| steht für die
Applikation einer top-level Funktion (wenn sie die korrekte Anzahl an Argumenten
übergeben bekommen hat). Es wird sich der Name (|Name|) der aufgerufenen
Funktion, sowie die Argumentenliste (|[Exp]|) gemerkt. |HAp|, dagegen, steht für
die Applikation einer Funktion höherer Ordnung. Nicht überraschend repräsentiert
der |If|-Konstruktor REC's `\lstinline[language=Rec]$if$'-Konstrukt.

Verwundern sollte uns der erste Parameter von |Lam|; und zwar |Int|
(|Name| steht offensichtlich für die gebundene Variable und |Exp| für den Körper
des Lambda-Ausdrucks). Wir wir noch sehen werden, erhält jeder Lambda-Ausdruck
einen gesonderten Abschnitt im generierten GOTO-Programm,  vorangestellt mit
einer eindeutigen Marke. Da Lambda-Ausdrücke aber anonym sind, müssen wir selber
beim Parsen dafür sorgen, ihnen eindeutige Bezeichnungen zu geben. Mit dem |Int|
Parameter zählen wir die Lambda-Ausdrücke also einfach in pre-order Reihenfolge
durch und können somit das Geforderte leisten.

Wie wird nun konkret ein REC Ausdruck durch den algebraischen Datentyp |Exp|
dargestellt? Wir betrachten ein Beispiel: der REC Ausdruck $x + y$ wird durch
den Haskell Ausdruck |Ap "+" [Var "x", Var "y"]| dargestellt. Ein Variablenname
ist nichts weiter als eine Zeichenkette:

\begin{code}
type Name = String
\end{code}

Ein REC Programm ist lediglich eine Liste von Funktionsdefinitionen:

\begin{code}
type Program = [Def]
\end{code}

Eine Funktionsdefinition beinhaltet den Namen der Funktion, ihre (formalen)
Parameter\footnote{Die Parameterliste darf natürlich auch leer sein im Falle
einer Funktion ohne Parameter.} und den Funktionskörper:

\begin{code}
type Def = (Name, [Name], Exp)
\end{code}

Zum Abschluss betrachten wird noch ein kleines REC Programm:
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


\section{Pretty Printer}

Sobald der Wert des Typs |Program| festgestellt worden ist, ist es oft
praktisch diesen lesbar zu präsentieren. Das heißt, eine \emph{Pretty Printing}
Funktion wird benötigt! Dabei sollten wir insbesondere darauf achten, dass
keine überflüssigen Klammern bei Infix-Operatoren ausgegeben werden, weshalb
wir folgende zwei Hilfsfunktionen definieren:

\begin{code}
parensOrNotL op l s
  | getOpPrecedence op > getOpPrecedence l = "(" ++ s ++ ")"
  | getOpPrecedence op < getOpPrecedence l = s
  | otherwise = case getOpAssociativity op of
                  AssocLeft  -> s
                  _          -> "(" ++ s ++ ")"
parensOrNotR op r s
  | getOpPrecedence op > getOpPrecedence r = "(" ++ s ++ ")"
  | getOpPrecedence op < getOpPrecedence r = s
  | otherwise = case getOpAssociativity op of
                  AssocRight  -> s
                  _          -> "(" ++ s ++ ")"
\end{code}

Wir nutzen also die Informationen aus der Operatortabelle um zu entscheiden, ob
Klammern gesetzt werden müssen oder nicht. Mit diesen Funktionen können wir
schon einen Pretty Printer für REC Ausdrücke angeben:

\begin{code}
pprExp :: Exp -> String
pprExp (Num n) = show n
pprExp (Var v) = v
pprExp (Ap op [a, b])
  | op `elem` operators
  = case [a, b] of
      [Num  _, Num  _] -> pprExp a ++ space op ++ pprExp b
      [Num  _, Var  _] -> pprExp a ++ space op ++ pprExp b
      [Var  _, Num  _] -> pprExp a ++ space op ++ pprExp b
      [Var  _, Var  _] -> pprExp a ++ space op ++ pprExp b
      [Ap l _, Num  _] ->
        parensOrNotL op l (pprExp a) ++ space op ++ pprExp b
      [Ap l _, Var  _] ->
        parensOrNotL op l (pprExp a) ++ space op ++ pprExp b
      [Num  _, Ap r _] ->
        pprExp a ++ space op ++ parensOrNotR op r (pprExp b)
      [Var  _, Ap r _] ->
        pprExp a ++ space op ++ parensOrNotR op r (pprExp b)
      [Ap l _, Ap r _] ->
        parensOrNotL op l (pprExp a) ++ space op ++ parensOrNotR op r (pprExp b)
      _                ->
        "(" ++ pprExp a ++ space op ++ pprExp b ++ ")"
  | otherwise = op ++ "(" ++ pprExp a ++ ", " ++ pprExp b ++ ")"
  where
  space "^" = "^"
  space op  = " " ++ op ++ " "
pprExp (Ap f es)
  = f ++ "(" ++ intercalate ", " (map pprExp es) ++ ")"
pprExp (HAp e1 e2)
  = "(" ++ pprExp e1 ++ ")" ++ "(" ++ pprExp e2 ++ ")"
pprExp (Lam _ x e)
  = "\\" ++ x ++ ". " ++ pprExp e
pprExp (If e1 e2 e3)
  = "if " ++ pprExp e1
          ++ " then " ++ pprExp e2
          ++ " else " ++ pprExp e3
\end{code}

Jetzt ist es ein leichtes einen Pretty Printer für Funktionsdefinitionen und
für das gesamte Programm zu schreiben:

\begin{code}
pprDef :: Def -> String
pprDef (name, args, rhs)
  = name ++ "(" ++ intercalate ", " args ++ ")" ++ " := " ++ pprExp rhs
pprint :: Program -> String
pprint = intercalate ";\n" . map pprDef
\end{code}


\section{Parser}

Natürlich soll es möglich sein als Klartext vorliegende REC Programme als
solche zu interpretieren. In diesem Abschnitt wird deshalb ein \emph{Parser}
für REC Programme definiert.

Die ``Parser Combinator''-Bibliothek \emph{Parsec} wird hierfür verwendet.
Vorteile eine schon existierende Bibliothek zu verwenden sind zum Beispiel eine
integrierte Fehlerberichterstattung und eine Reihe schon vordefinierter,
nützlicher Hilfsfunktionen bzw. Kombinatoren. Außerdem können die Parser direkt
als Haskell Programme geschrieben werden - ein zusätzlicher Generierungsschritt
aus anderweitigen Beschreibungen entfällt. Die generelle Vorgehensweise bei der
Verwendung einer ``Parser Combinator''-Bibliothek ist es einen großen Parsers
aus mehreren kleinen Parsern zusammenzufügen.

Anzumerken ist, dass kein echter, separater \emph{Lexing} Schritt gemacht wird.
Stattdessen greifen wir auf einige komfortable Hilfskonstrukte von Parsec
zurück. Im Folgenden wird ein Lexer-Stil und die verwendeten Tokens
beschrieben, sowie eine Reihe Hilfsparser definiert:

\begin{code}
lexDef
  = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.nestedComments  = True
  , Token.identStart      = letter <|> oneOf "_"
  , Token.identLetter     = alphaNum <|> oneOf "_"
  , Token.opStart         = Token.opLetter lexDef
  , Token.opLetter        = oneOf (concat (Token.reservedOpNames lexDef))
  , Token.reservedOpNames = ":=" : operators
  , Token.reservedNames   = [ "if", "then", "else" ]
  , Token.caseSensitive   = True
  }

lexer = Token.makeTokenParser lexDef

parens     = Token.parens lexer
semiSep1   = Token.semiSep1 lexer
commaSep   = Token.commaSep lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
natural    = Token.natural lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol lexer
\end{code}

Kommen wir nun zum eigentlichen Parsen. Die Funktion |parse| erhält als Eingabe
den Programmtext und liefert entweder den entsprechenden Wert vom Typ |Program|
oder eine Fehlermeldung. Um die Stellen im Quellcode, an denen eine
Fehlerbehandlung notwendig ist um Laufzeitfehlern vorzubeugen, zur
Kompilierzeit feststellbar zu machen, wird der Rückgabewert im |Either|
Datentyp umschlossen. |mkStdParser| ist hierbei eine importierte Hilfsfunktion.
Im Gegensatz zu |parse| wirft |parse'| einen Laufzeitfehler bei invalider
Eingabe.

\begin{code}
parse :: String -> Either String Program
parse source = mkStdParser pProgram (parseDefs source, [1..], []) whiteSpace source

parse' :: String -> Program
parse' source = mkStdParser' pProgram (parseDefs source, [1..], []) whiteSpace source
\end{code}

Aus verschiedenen Gründen müssen wir uns während des Parsens einige Dinge
merken. Um syntaktischen Zucker korrekt parsen zu können, müssen wir das
Programm sogar zweimal Parsen. Im ersten Durchgang sammeln wir die Namen der
top-level Funktionsdefinitionen zusammen mit ihrer jeweiligen Parameteranzahl in
einer |Map Name Int|. Dies leistet die Funktion |parseDefs|. Wie schon angedeutet
müssen wir die Lambda-Ausdrücke durchzählen. Wir verwenden dafür einen unendlichen
Stream von aufsteigenden Zahlen (|[Int]|), von dem wir immer entsprechende Zahlen
abschneiden, wenn wir auf einen Lambda-Ausdruck treffen. Weiterhin merken wir uns
in einer Liste |[Name]| die gebundenen Variablen von äußeren Lambda-Ausdrücken.
Dies ist wichtig, für den Fall, dass eine gebundene Variable im Lambda-Kopf denselben
Namen hat wie eine top-level Funktion. In dem Fall wird die Variable \emph{nicht}
als |Ap| geparst!

An dieser Stelle sollten wir schon einmal auf einen kleinen syntaktischen Zucker
eingehen, damit |parseDefs| ein bisschen verständlicher wird. Falls eine
Funktionsdefinition keine Parameter hat, wie z.B.
`\lstinline[language=Rec]$ten() := 10$', wollen wir erlauben, die leeren
Klammern weglassen zu dürfen: `\lstinline[language=Rec]$ten := 10$'.

\begin{code}
type Parser a = GenParser Char (M.Map Name Int, [Int], [Name]) a

parseDefs :: String -> M.Map Name Int
parseDefs source = case mkStdParser (semiSep1 p) (M.empty, [], []) whiteSpace source of
                     Right r -> M.fromList r
                     Left  _ -> M.empty -- errors are found in 2. pass anyway
  where
  p = do
    n <- identifier
    t <- option Nothing $ Just <$> symbol "("
    case t of
      Nothing -> do
        reservedOp ":="
        _ <- pExp
        return (n, 0)
      Just _  -> do
        l <- length <$> (commaSep identifier)
        _ <- symbol ")"
        reservedOp ":="
        _ <- pExp
        return (n, l)
\end{code}

Rufen wir uns noch einmal die Syntaxdefinition von REC ins Gedächtnis. Diese
können ziemlich direkt nach Haskell übersetzt werden. Betrachten wir zum
Beispiel die Produktionen für $\l prog \r$ und $\l fn \r$:

\begin{myindent}{3mm}
\begin{tabular}{ l c l l }
$\l prog \r$  &$\to$&     $\l fn_1 \r$;$\ldots$;$\l fn_n \r$ & // $n \ge 1$\\
$\l fn \r$    &$\to$&     $\l var \r$($\l var_1 \r$,$\ldots$,$\l var_n \r$)
\end{tabular}
\end{myindent}

Diese werden folgendermaßen übersetzt:

\begin{code}
pProgram :: Parser Program
pProgram = semiSep1 pFn

pFn :: Parser Def
pFn = do
  name <- identifier
  t <- option Nothing $ Just <$> symbol "("
  case t of
    Nothing -> do
      reservedOp ":="
      exp <- pExp
      return $ (name, [], exp)
    Just _  -> do
      args <- commaSep identifier
      updateState $ \(m, is, _) -> (m, is, args)
      _ <- symbol ")"
      reservedOp ":="
      exp <- pExp
      return $ (name, args, exp)
\end{code}

Leider kann nicht jede Produktion so direkt übersetzt werden - insbesondere im
Falle von Linksrekursion. Zur Veranschaulichung soll die Produktion für
Infix-Funktionsanwendungen herhalten:

\begin{myindent}{3mm}
\begin{tabular}{ l c l l }
$\l exp \r$ &$\to$&  $\l exp_1 \r$ $\l op \r$ $\l exp_2 \r$     &
\end{tabular}
\end{myindent}

Falls die Produktion einfach zu |pExp = liftM Ap (pExp pOp pExp)| übersetzt
werden würde, dann würde |pExp| leider nie terminieren, da es sich unaufhörlich
selbst aufrufen würde. Zum Glück ist es üblicherweise möglich eine Grammatik
für Programmiersprachen so umzuformen, dass sie nicht mehr linksrekursiv ist,
aber immernoch dieselbe Sprache erzeugt. Jedoch ist diese Umformung recht
mühsam. Parsec schafft aber auch hier Abhilfe und zwar mit dem Hilfskonstrukt
|buildExpressionParser|.

|buildExpressionParser| erhält als Eingabe eine Tabelle von Operatorparsern (in
der Reihenfolge der Operatorpriorität) und erzeugt einen Parser für Ausdrücke,
der die angegebene Priorität und Assoziativität korrekt beachtet. Wir erinnern
uns zudem an die Operatortabelle von REC; diese kann ziemlich direkt übersetzt
werden:

\begin{code}
pExp :: Parser Exp
pExp = buildExpressionParser opTable pTerm
  where
  opTable =
    [ [ op "^"  AssocRight ]
    , [ op "*"  AssocLeft, op "/"  AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "%"  AssocLeft ]
    , [ op "="  AssocNone
      , op "!=" AssocNone
      , op "<"  AssocNone
      , op "<=" AssocNone
      , op ">"  AssocNone
      , op ">=" AssocNone
      ]
    , [ op "&&" AssocRight ]
    , [ op "||" AssocRight ]
    ]
  op name = Infix (reservedOp name >> return (\x y -> Ap name [x, y]))

pTerm :: Parser Exp
pTerm = choice
      [ try pHAp
      , try pLam
      , try pAp
      , try pIf
      , pVar
      , pNum
      , parens pExp <?> "term"
      ]
\end{code}

Zugegebenermaßen ist die Definition von |pTerm| etwas trickreich, speziell was
das Einfügen des |try|-Parsers und der Reihenfolge angeht.

Um |pLam| gänzlich verstehen zu können, betrachten wir den syntaktischen Zucker,
der für Lambda-Ausdrücke vorgesehen ist. Und zwar wollen wir erlauben im Kopf
eines Lambda-Ausdruckes gleich mehrere gebundene Variablen angeben zu können.
`\lstinline[language=Rec]$\a b c. a + b + c$' soll dabei nichts anderes heißen
als `\lstinline[language=Rec]$\a.\b.\c. a + b + c$':

\begin{code}
pLam :: Parser Exp
pLam = do
  _ <- symbol "\\"
  xs <- sepBy1 identifier whiteSpace
  _ <- symbol "."
  (is, rest) <- (splitAt (length xs) . (\(_, x, _)->x)) <$> getState
  updateState (\(m, _, bound) -> (m, rest, bound ++ xs))
  e <- pExp
  return $ mkLamChain is xs e
  where
  mkLamChain [i] [x] e = Lam i x e
  mkLamChain (i:is) (x:xs) e = Lam i x (mkLamChain is xs e)
  mkLamChain _ _ _
    = error "Impossible! Parsing guarantees at least one argument!"
\end{code}

Auch beim Parsen von Funktionsanwendungen höherer Ordnung erlauben
wir syntaktischen Zucker. Um Klammern ``zu sparen'' sollen Ausdrücke folgender
Form:
\begin{myindent}{3mm}
`\lstinline[language=Rec]T(...((f(p$_1$,...,$p_n$))(e$_1$))...(e$_m$)T', für eine
beliebige\footnote{Top-level oder nicht. Für den Fall, dass
`\lstinline[language=Rec]TfT' \emph{keine} top-level Definition ist wird das
Anhängsel |try pVar| benötigt.} Funktion `\lstinline[language=Rec]TfT' mit $n
\in \mathbb{N}$ Parametern und für $m \in \mathbb{N}$ beliebige REC-Ausdrücke
`\lstinline[language=Rec]Te$_1$,...,e$_m$T'.
\end{myindent}
auch in dieser Form erlaubt sein:
\begin{myindent}{3mm}
`\lstinline[language=Rec]Tf(p$_1$,...,$p_n$)(e$_1$,...,e$_m$)T'
\end{myindent}

\begin{code}
pHAp :: Parser Exp
pHAp
  = choice
    [ try $
      do l <- try pAp <|> try (parens $ pLam) <|> try pVar
         pars <- concat <$> (many1 $ (parens $ commaSep pExp))
         return $ mkHApChain (reverse (l:pars))
    , do l <- parens pExp
         e <- parens pExp
         return $ HAp l e
    ]
  where
  mkHApChain [e]    = e
  mkHApChain (e:es) = HAp (mkHApChain es) e
  mkHApChain [] = error "Impossible due to parsing!"
\end{code}

Die letzte Form von syntaktischem Zucker, die wir erlauben wollen, hat
es in sich, weshalb der |pAp|-Parser auch etwas komplizierter ist.
Wenn wir einer top-level Funktion mehr oder weniger Parameter übergeben,
als ihre Definition vorsieht, möchten wir keinen Fehler ausgeben sondern
vielmehr diese Applikation anders interpretieren. Konkret:

Sei `\lstinline[language=Rec]Tf(p$_1$,...,p$_n$) := eT' eine Funktionsdefinition
`\lstinline[language=Rec]TfT' mit $n \in \mathbb{N}$ Parametern. Wir unterscheiden
folgende Fälle bei der Applikation von `\lstinline[language=Rec]TfT':
\begin{enumerate}
\item `\lstinline[language=Rec]Tf(q$_1$,...,q$_m$)T' mit $m < n$. Dann ist diese
Applikation wie folgt zu verstehen:
`\lstinline[language=Rec]T\x$_1$ ... x$_{n-m}$. f(q$_1$,...,q$_m$,x$_1$,...,x$_{n-m}$)T'
\item `\lstinline[language=Rec]Tf(q$_1$,...,q$_m$)T' mit $m > n$. Dann ist diese
Applikation wie folgt zu verstehen:
`\lstinline[language=Rec]Tf(q$_1$,...,q$_n$)(q$_{n+1}$,...,q$_{m}$)T'
\end{enumerate}

Vorteile dieser Vereinbarung ergeben sich dann, wenn eine top-level Funktion
als Wert behandelt werden soll. Denn dann muss der top-level Funktionsaufruf
nicht umständlich in einer Lambda-Abstraktion umschlossen werden. Statt also:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
id(x) := x;
compose(f, g) := \x. f(g(x));
main(a) := compose(\x. id(x), \x. id(x))(a)
\end{lstlisting}
\end{myindent}

darf nun einfach

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
id(x) := x;
compose(f, g) := \x. f(g(x));
main(a) := compose(id, id, a)
\end{lstlisting}
\end{myindent}

geschrieben werden. Es ist hoffensichtlich ersichtlich, dass die zweite Version
deutlich übersichtlicher wirkt. Ein weiterer Vorteil ist, dass \emph{curried}
Funktionen syntaktisch sehr leicht realisierbar sind.

Durch diesen syntaktischen Zucker scheint es nun egal zu sein, ob z.B.
`\lstinline[language=Rec]TcomposeT' so
`\lstinline[language=Rec]Tcompose(f, g, x) := f(g(x))T' oder so
`\lstinline[language=Rec]Tcompose := \f g x. f(g(x))T'
definiert wird. Folgende Richtlinie soll aber als Anhaltspunkt dienen. Auf der
linken Seite des `\lstinline[language=Rec]T:=T'-Zeichens soll die Anzahl der
Parameter erscheinen, mit der die Funktion im Allgemeinen auch in den meisten
Fällen aufgerufen wird. Nicht nur macht diese Richtlinie ein Programm
konsistenter, es bieten sich auch gewisse Geschwindigkeitsvorteile bei der
Übersetzung, da, wenn die Parameteranzahl beim Aufruf mit der Parameteranzahl der
Definition übereinstimmt, keine Closure erzeugt wird.

\begin{code}
pAp :: Parser Exp
pAp = do
  fn <- identifier
  (_, _, bound) <- getState
  when (fn `elem` bound) (fail "d'oh!")
  t <- M.lookup fn . (\(x,_,_)->x) <$> getState
  case t of
    Nothing -> fail "definition for application not found!"
    Just 0  -> do
      t <- option Nothing $ Just <$> (symbol "(")
      case t of
        Nothing -> return $ Ap fn []
        Just _  -> do
          args <- commaSep pExp
          _ <- symbol ")"
          return $ mkHApChain (reverse $ (Ap fn []) : args)
    Just n  -> do
      t <- option Nothing $ Just <$> (symbol "(")
      args <-
        case t of
          Nothing -> return []
          Just _  -> do
            xs <- commaSep pExp
            _ <- symbol ")"
            return xs
      let n0 = length args
      case () of
        _ | n0 == n   -> return $ Ap fn args
          | n0 >  n   -> return $
              mkHApChain (reverse $ (Ap fn (take n args)) : (drop n args))
          | otherwise -> do
              let d = n - n0
              (is, rest) <- (splitAt d . (\(_,x,_)->x)) <$> getState
              updateState (\(m, _, bound) -> (m, rest, bound))
              let as  = map ('x' :) (map show [1..])
                  ls  = take d (as \\ (getNames args))
                  ls' = map Var ls
              return $ mkLamChain fn (args ++ ls') ls is
   where
   mkHApChain [e]    = e
   mkHApChain (e:es) = HAp (mkHApChain es) e
   mkHApChain [] = error "Impossible due to parsing!"
   mkLamChain fn as [] [] = Ap fn as
   mkLamChain fn as (l:ls) (i:is) = Lam i l $ mkLamChain fn as ls is
   mkLamChain _ _ _ _ = error "Impossible!"
\end{code}

Hier folgen nun die restlichen Parser, die relativ selbsterklärend
sein sollten.

\begin{code}
pIf = do
  reserved "if"
  e1 <- pExp
  reserved "then"
  e2 <- pExp
  reserved "else"
  e3 <- pExp
  return $ If e1 e2 e3

pVar :: Parser Exp
pVar = liftM Var (identifier <?> "variable")

pNum :: Parser Exp
pNum = liftM Num (natural <?> "number")
\end{code}


\section{Übersetzung nach GOTO}

In diesem Teil wird die Generierung eines semantisch äquivalenten GOTO-Programms
aus einem gegebenen REC Programm beschrieben. Es wird versucht die Überlegungen
hinter dem Nutzen einer jeden definierten Funktion hinsichtlich des Ziels der
Übersetzung immerhin knapp darzustellen. Doch zuerst sollen einige
Hilfsfunktionen eingeführt werden.

|getDefNames| liefert bei Eingabe eines REC Programms einfach die Namensliste
der im Programm definierten Funktionen. Analog liefert |getDefRhss| eine Liste
aller Ausdrücke die sich in den Funktionskörpern befinden und zwar entspricht
ein Listeneintrag der rechten Seite einer Funktionsdefinition. `Rhss` steht
hierbei für `Right hand sides`.

\begin{code}
getDefNames :: Program -> [Name]
getDefNames p = [ name | (name, _, _) <- p ]
getDefRhss :: Program -> [Exp]
getDefRhss p = [ exp | (_, _, exp) <- p ]
\end{code}

|getCalledFnNames| liefert bei Eingabe einer Liste von Ausdrücken eine Liste von
allen Bezeichnern eines Funktionsaufrufes (also |Name| in |Ap Name [Exp]|), die
innerhalb der Ausdrücke vorkommen. Diese Funktion wird später unter anderem
dafür nützlich sein, Spezialcode nativer Operationen wie `$+$` nur dann in das
generierte GOTO-Programm einzubinden, wenn sie auch wirklich benutzt worden
sind.

\begin{code}
getCalledFnNames :: [Exp] -> [Name]
getCalledFnNames [] = []
getCalledFnNames (Num _:rest) = getCalledFnNames rest
getCalledFnNames (Var _:rest) = getCalledFnNames rest
getCalledFnNames (Ap fn args:rest)
  = fn : getCalledFnNames args ++ getCalledFnNames rest
getCalledFnNames (Lam _ _ e:rest)
  = getCalledFnNames [e] ++ getCalledFnNames rest
getCalledFnNames (HAp e1 e2:rest)
  = getCalledFnNames [e1] ++ getCalledFnNames [e2] ++ getCalledFnNames rest
getCalledFnNames (If e1 e2 e3:rest)
  = getCalledFnNames [e1, e2, e3] ++ getCalledFnNames rest
\end{code}

|getNames| ähnelt |geCalledFnNames| sehr mit dem Unterschied, dass
Variablenbezeichner und die Bezeichnung der gebundenen Variable eines Lambda-
Ausdrucks mit in die  Liste der Bezeichner genommen werden. Diese Funktion wird
später dazu nützlich sein, zwischen freien und gebundenen Variablen innerhalb
des Körpers eines Lambda-Ausdrucks unterscheiden zu können.

\begin{code}
getNames :: [Exp] -> [Name]
getNames [] = []
getNames (Num _:rest) = getNames rest
getNames (Var v:rest) = v : getNames rest
getNames (Ap fn args:rest)
  = fn : getNames args ++ getNames rest
getNames (Lam _ x e:rest)
  = x : getNames [e] ++ getNames rest
getNames (HAp e1 e2:rest)
  = getNames [e1] ++ getNames [e2] ++ getNames rest
getNames (If e1 e2 e3:rest)
  = getNames [e1, e2, e3] ++ getNames rest
\end{code}

|findDef| liefert bei Eingabe eines Funktionsnamens und eines REC Programms
die vollständige Definition der gesuchten Funktion. Es wird davon ausgegangen,
dass |findDef| im Quellcode nur so benutzt wird, dass ein Nichtfinden einer
Definition nicht möglich ist. Im Grunde wird |findDef| nur benötigt, um die
Definition der `\lstinline[language=Rec]$main$` Funktion zu finden, da ja nicht
vorgeschrieben ist, an welcher Stelle im Quellcode sich die Definition befinden
muss.

\begin{code}
findDef :: Name -> Program -> Def
findDef _ [] = error "Impossible! Definition not found!"
findDef n ((n', args, exp):ps)
  | n == n'   = (n', args, exp)
  | otherwise = findDef n ps
\end{code}

Um eine einzelne Funktionsdefinition zu übersetzen muss diese Funktion
``irgendwie'' auf ein GOTO-Programm abgebildet werden. Dazu erhält der
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
`\lstinline[language=Goto]$a1,a2,...$` zwischenspeichert. Außerdem
muss diese Sequenz nach jeder `\lstinline[language=Goto]$CALL$'-Anweisung
wiederholt werden, da die Variablen zwischenzeitlich andere Werte
zugewiesen bekommen haben können. |genArgSeq|
erzeugt diese Sequenz:

\begin{code}
genArgSeq :: Int -> G.Program
genArgSeq 0 = mempty
genArgSeq numberOfArgs
  = G.Seq $ map f [1..numberOfArgs]
  where
  f i = G.Peek ('a':show i)
        $ G.AOp "-" (G.Var "fp") (G.Num $ toInteger (numberOfArgs - i + 1))
\end{code}

Wenn wir uns dazu auf machen den Funktionsrumpf zu übersetzen, dann müssen
wir den formalen Parametern einer REC Definition positionell die
entsprechenden GOTO-Variablen `\lstinline[language=Goto]$a1,a2,...$` zuordnen.
|mkParamMap| erzeugt diese Zuordnung:

\begin{code}
type ParamMap = M.Map Name Name
mkParamMap :: [Name] -> ParamMap
mkParamMap args = M.fromList $ zip args $ map ("a"++) $ map show [1..]
\end{code}

Ähnliche Funktionen müssen wir für Closures erstellen. Betrachten wir dazu
eine Beispielübersetzung:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
call1(f) := \x. f(x)
\end{lstlisting}
\end{myindent}
wird übersetzt zu

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
call1: a1 := PEEK (fp - 1);
       MAKE_CLOSURE 1,a1;
       PUSH (hp - 1);
       RETURN;

lambda1: h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         a1 := PEEK (fp - 1);
         PUSH a1;
         PUSH h1;
         t := POP;
         CALL_CLOSURE t, 1;
         h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         a1 := PEEK (fp - 1);
         RETURN;

lamret: IF (cp = 1) THEN
          GOTO lambda1
        END
\end{lstlisting}
\end{myindent}

`\lstinline[language=Goto]$h1,h2,...$' speichern hierbei den Inhalt der
freien Variablen (hier die Funktion `\lstinline[language=Rec]$f$' im
Lambda-Ausdruck) zwischen. Dabei verwenden wir per Konvention die GOTO-Variable
`\lstinline[language=Goto]Th0T', in der die Adresse zum aktuell betrachteten
Closureintrag im Heap gespeichert wird. |genHArgSeq| erstellt diese Sequenz:

\begin{code}
genHArgSeq :: Int -> G.Program
genHArgSeq 0 = mempty
genHArgSeq numberOfArgs
  = G.Seq $ map f [1..numberOfArgs]
  where
  f i = G.PeekHeap ('h':show i)
        $ G.AOp "+" (G.Var "h0") (G.Num $ toInteger i)
\end{code}

Wie auch davor ordnen wir den freien Variablen die temporären
Variablen `\lstinline[language=Goto]$h1,h2,...$' zu mit Hilfe von
|mkFreeMap|:

\begin{code}
mkFreeMap :: [Name] -> ParamMap
mkFreeMap args = M.fromList $ zip args $ map ("h"++) $ map show [1..]
\end{code}

Wir übersetzen Lambda-Ausdrücke, so, dass jeder Lambda-Ausdruck einen eigenen
Abschnitt in dem erzeugten GOTO-Programm erhält. Da wir beim Parsen den Lambda-
Ausdrücken eindeutige Zahlen zugeordnet haben, können wir einem Abschnitt
einfach die Marke `\lstinline[language=Goto]Tlambda$i$T' zuordnen, wenn es sich
um den $i$-ten Lambda-Ausdruckk im Programm handelt\footnote{Eigentlich
müssen wir noch vorsichtiger sein, da eine Programm auch eine Funktion mit
dem Namen ``\lstinline[language=Rec]Tlambda1T'' verwenden könnte. Da es sich
bei REC aber eher um ein Proof of Concept handelt, gehen wir einfach davon aus,
dass dieser Fall nie eintritt.}. |genLamSec| ist die Funktion, die dieses leistet.
Sie erhält eine Liste der Namen aller top-level Funktionsdefinitionen\footnote{Um
native Operatoren speziell zu übersetzen} und den AST des geparsten Programms vom
Typ |Program| und liefert letztlich das entsprechende GOTO-Programm, in dem
für jeden Lambda-Ausdruck ein eigener Abschnitt erzeugt worden ist. Wie wird
ein Abschnitt jedoch konkret erzeugt?

Zahlen und Variablen werden einfach ``übersprungen'':

\begin{code}
genLamSec :: [Name] -> Program -> G.Program
genLamSec _ [] = mempty
genLamSec fnNames ((_, _, Num _) : rest)
  = genLamSec fnNames rest
genLamSec fnNames ((_, _, Var _) : rest)
  = genLamSec fnNames rest
\end{code}

Bei einem `\lstinline[language=Rec]TifT'-Ausdruck werden einfach die entsprechenden
REC-Ausdrücke |e1|,|e2| und |e3| durchlaufen. Dabei müssen wir darauf achten uns
die, in diesem Kontext, freien Variablen |topargs| zu merken:

\begin{code}
genLamSec fnNames ((_, topargs, If e1 e2 e3) : rest)
  = let t1 = genLamSec fnNames [("dontcare", topargs, e1)]
        t2 = genLamSec fnNames [("dontcare", topargs, e2)]
        t3 = genLamSec fnNames [("dontcare", topargs, e3)]
        t4 = genLamSec fnNames rest
    in  t1 <> t2 <> t3 <> t4
\end{code}

Normale Funktionsaufrufe und Funktionsaufrufe höherer Ordnung werden analog zu
`\lstinline[language=Rec]IifI'-Ausdrücken behandelt:

\begin{code}
genLamSec fnNames ((_, topargs, Ap _ args) : rest)
  = let t1 = genLamSec fnNames $ map (\x->("dontcare", topargs, x)) args
        t2 = genLamSec fnNames rest
    in  t1 <> t2
genLamSec fnNames ((_, topargs, HAp e1 e2) : rest)
  = let t1 = genLamSec fnNames [("dontcare", topargs, e1)]
        t2 = genLamSec fnNames [("dontcare", topargs, e2)]
        t3 = genLamSec fnNames rest
    in  t1 <> t2 <> t3
\end{code}

Nun kommen wir zum interessanten Teil. In |topargs| haben wir uns alle freien
Variablen in Bezug auf den jetzt zu übersetzenden Lambda-Ausdruck gemerkt,
unabhängig davon, ob diese überhaupt in dem Rumpf des Lambda-Ausdrucks
auftauchen. Um die tatsächlich auftauchenden freien Variablen zu bestimmen,
analysieren wir die im Rumpf verwendeten Namen und schneiden diese Menge mit
|topargs|. |x| ist die gebundene Variable des Lambda-Ausdrucks, weshalb sie von
der Menge der freien Variablen abgezogen wird. Zuletzt müssen wir den freien
Variablen eine kanonische Ordnung verleihen (lexikographische Sortierung) und
Duplikate entfernen, damit die Reihenfolge der freien Variablen mit der im Heap
übereinstimmt. Dies alles leistet die lokale Funktion |getFreeVars| und das
Ergebnis wird in |free| gespeichert.

Wir wissen, dass, wenn der Lambda-Ausdruck tatsächlich freie Variablen enthält,
wir bei Eintritt des entsprechenden Abschnittes im GOTO-Programm im aktuellen
Activation-Record die Adresse der passenden Closure im Heap finden. Wir
speichern die Adresse in `\lstinline[language=Goto]Ih0I' zwischen. Der erste
Wert des Eintrags mit der Adresse `\lstinline[language=Goto]Ih0I' im Heap ist
bekanntlich ein Zeiger zum entsprechenden Codeabschnitt. Dieser interessiert uns
aber nicht, da wir uns schon im entsprechenden Codeabschnitt befinden. Jedoch
interessieren uns die nachfolgenden Werte des Heapeintrags, also der Werte der
freien Variablen der Closure. Da wir von einer kanonischen Reihenfolge der
freien Variablen ausgehen, können die Werte der |length free| freien Variablen
einfach in den GOTO-Variablen `\lstinline[language=Goto]Ih1,h2,...I' speichern.
Da jeder Lambda-Ausdruck ein Argument erhält, speichern wir dieses ebenfalls in
`\lstinline[language=Goto]Ia1I' zwischen. Danach können wir endlich den Rumpf
|e| übersetzten. Dabei ist nun nur noch |x| die einzige gebundene Variable im
Kontext von |e|. Darüber hinaus müssen wir die freien Variablen bei |genExpSeq|
korrekt setzen. Zu guter Letzt folgt wie bei allen Abschnitten eine
`\lstinline[language=Goto]IRETURNI'-Anweisung.

Da sich innerhalb von |e| noch weitere Lambda-Ausdrücke verstecken können,
müssen wir für |e| auch noch einmal |genLamSec| aufrufen. Nun ist |x| aber als
freie Variable zu interpretieren.

\begin{code}
genLamSec fnNames ((_, topargs, Lam i x e) : rest)
  = let t1 = G.Label ("lambda" ++ show i)
             $  mempty
             <> (if (length free) /= 0
                   then G.Peek "h0" (G.AOp "+" (G.Var "fp") (G.Num 2)) -- heap adress
                   else mempty)
             <> genHArgSeq (length free)
             <> genArgSeq 1
             <> genExpSeq fnNames (mkParamMap [x]) (mkFreeMap free) [e]
             <> G.Return
        free = getFreeVars topargs [x] e
        t2 = genLamSec fnNames [("dontcare", topargs ++ [x], e)]
        t3 = genLamSec fnNames rest
    in     mempty
        <> t1
        <> t2
        <> t3
  where
  getFreeVars :: [Name] -> [Name] -> Exp -> [Name]
  getFreeVars outer bound e = nub $ sort ((outer \\ bound) `intersect` (getNames [e]))
\end{code}

Wie wir wissen verhält sich die Variable `\lstinline[language=Goto]IcpI' so
ähnlich wie `\lstinline[language=Goto]IpcI'. In ihr speichern wir quasi die
Adresse des Lambda-Abschnitts im GOTO-Programm zwischen, auf den wir während
einer `\lstinline[language=Goto]ICALL_CLOSUREI' springen möchten. Die passende
``Sprungtabelle'' erstellen wir mit |genLamRetSec|. Da wir beim Parsen die
Lambda-Ausdrücke gezählt haben ist dies auch ohne weiteres möglich:

\begin{code}
genLamRetSec [] = mempty
genLamRetSec (Num _ : rest)
  = genLamRetSec rest
genLamRetSec (Var _ : rest)
  = genLamRetSec rest
genLamRetSec (Ap _ args : rest)
  = let t1 = genLamRetSec args
        t2 = genLamRetSec rest
    in  t1 <> t2
genLamRetSec (HAp e1 e2 : rest)
  = let t1 = genLamRetSec [e1]
        t2 = genLamRetSec [e2]
        t3 = genLamRetSec rest
    in  t1 <> t2 <> t3
genLamRetSec (If e1 e2 e3 : rest)
  = let t1 = genLamRetSec [e1]
        t2 = genLamRetSec [e2]
        t3 = genLamRetSec [e3]
        t4 = genLamRetSec rest
    in  t1 <> t2 <> t3 <> t4
genLamRetSec (Lam i _ e : rest)
  = let t1 = genLamRetSec [e]
        t2 = genLamRetSec rest
    in  mempty
        <> (G.If (G.ROp "=" (G.Var "cp") (G.Num (toInteger i)))
                 (G.Goto ("lambda" ++ show i)))
        <> t1
        <> t2
\end{code}

Nun widmen wir uns dem Kern der Übersetzung; der Übersetzung eines REC
Ausdrucks. Der erste Parameter von |genExpSeq| ist eine Liste der im Programm
definierten Funktionsnamen. Wie auch sonst dient der erste Parameter dazu den
Abschnitten für etwaige benutzte Operatoren alphanumerische Namen geben zu
können. Die nächsten beiden Parameter von |genExpSeq| sind eine Abbildung von
Parametern der zum Ausdruck gehörigen Funktionsdefinition (auf der linken Seite)
auf die entsprechenden GOTO-Variablen `\lstinline[language=Goto]$a1,a2,...$`
sowie eine Abbildung der freien Variablen innerhalb des Ausdrucks von freien auf
die entsprechenden GOTO-Variablen `\lstinline[language=Goto]$h1,h2,...$'. Der
dritte Parameter von |genExpSeq| ist nicht einfach ein Ausdruck, sondern eine
Liste von Ausdrücken. Das hat einfach den Grund, dass |genExpSeq| auch für die
Argumentliste des |Ap| Konstruktors verwendet wird und ohne die
Verlistifizierung würden zwei Funktionen benötigt werden, die ohnehin fast das
selbe leisten würden. Wir merken uns als Regel, das ein REC Ausdruck nach
seiner Auswertung die Ergebniszahl auf den Stack legt.

Eine Zahl wird einfach in den Stack gepusht:

\begin{code}
genExpSeq :: [Name] -> ParamMap -> ParamMap -> [Exp] -> G.Program
genExpSeq _ _ _ []
  = mempty
genExpSeq fnNames paramMap freeMap (Num n : rest)
  =  G.Push (G.Num n)
  <> genExpSeq fnNames paramMap freeMap rest
\end{code}

Variablen werden ebenfalls einfach in den Stack gepusht. Dabei wird darauf
geachtet, dass die Variablen innerhalb des Ausdrucks auch definiert sind,
also als Wert im zweiten bzw. dritten Parameter von |genExpSeq| auftauchen:

\begin{code}
genExpSeq fnNames paramMap freeMap (Var a : rest)
  =  mempty
  <> case M.lookup a freeMap of
       Just h  -> G.Push $ G.Var h
       Nothing ->
         case M.lookup a paramMap of
           Just x  -> G.Push $ G.Var x
           Nothing -> error "Blow up!"
  <> genExpSeq fnNames paramMap freeMap rest
\end{code}

Die Übersetzung atomarer Ausdrücke gestaltet sich also problemlos. Bei der
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

\begin{code}
genExpSeq fnNames paramMap freeMap (Ap fn args : rest)
  =  genExpSeq fnNames paramMap freeMap args
  <> G.Call (labelizeIfOp fnNames fn) (length args)
  <> genArgSeq (M.size paramMap)
  <> genExpSeq fnNames paramMap freeMap rest
\end{code}

Bei der Übersetzung eines Lambda-Ausdrucks erstellen wir der Einfachheit
halber immer eine Closure, auch wenn es Situationen gibt, bei denen
es überflüssig wäre (z.B. `\lstinline[language=Rec]$\x. x * 2$'). Dazu
müssen wir natürlich die freien Variablen im Heap speichern. Hier
sollte uns eine kleine Inkonsistenz auffallen. Nämlich werden die
Parameter auf der linken Seite einer Funktionsdefinition auch als
freie Variablen interpretiert, was ja auch korrekt ist. Jedoch suggeriert
der Name |freeMap|, dass sich dort alle freien Variablen befinden,
was nicht der Fall ist. Jedenfalls ist es wichtig die freien Variablen
in der selben Reihenfolge in den Heap zu speichern, die auch später
beim Aufruf der Closure erwartet wird. Dazu werden die freien
Variablen einfach lexikographisch sortiert. Danach wird ein
Zeiger auf den Heapeintrag in den Stack gepusht.

\begin{code}
genExpSeq fnNames paramMap freeMap (Lam i x e : rest)
  =  G.MakeClosure i
       (map (G.Var . (\x -> case M.lookup x freeMap of
                              Just v  -> v
                              Nothing -> lookup' x paramMap))
            (sort (free1 ++ free2))
       )
  <> G.Push (G.AOp "-" (G.Var "hp") (G.Num (genericLength (free1++free2))))
  <> genExpSeq fnNames paramMap freeMap rest
  where free1 = (((M.keys paramMap) \\ [x]) `intersect` (getNames [e])) \\ free2
        free2 = ((M.keys freeMap) \\ [x]) `intersect` (getNames [e])
\end{code}

Funktionsaufrufe höherer Ordnung bestehen aus zwei Ausdrücken |e1| und |e2|. Der
erste Ausdruck steht für eine (erst zur Laufzeit bestimmte) Funktion bzw.
genauer: der erste Ausdruck liefert nach Auswertung die Adresse des Heapeintrags
einer Closure (mit einer gebundenen Variable - andere Formen gibt es bei uns
nicht). Der Ausdruck |e2| liefert nach Auswertung den Wert für die gebundene
Variable der Closure. Deshalb wird zuerst der Ausdruck |e2| ausgewertet. Wie uns
bekannt sein sollte steht nach Auswertung von |e2| der bestimmte Wert oben auf
dem Stack. Danach wird |e1| ausgewertet. Danach befindet sich die Adresse des
Heapeintrags oben auf dem Stack. Diese wird vom Stack in die Variable
`\lstinline[language=Goto]$t$' gepoppt. Nun wird die Closure mit der Heap-
Adresse `\lstinline[language=Goto]$t$' aufgerufen, wobei sich das zugehörige
Argument für die gebundene Variable korrekt oben auf dem Stack befindet. Da
während des Closureaufrufs noch weitere Closureaufrufe und normale Aufrufe
geschehen können, müssen die Variablen `\lstinline[language=Goto]$a1,a2,...$'
und `\lstinline[language=Goto]$h1,h2,...$' resetted werden.

\begin{code}
genExpSeq fnNames paramMap freeMap (HAp e1 e2 : rest)
  =  mempty
  <> genExpSeq fnNames paramMap freeMap [e2]
  <> genExpSeq fnNames paramMap freeMap [e1]
  <> G.Pop "t" -- return value, i.e. heap adress of closure
  <> G.CallClosure (G.Var "t") 1
  <> (if (M.size freeMap) /= 0
         then G.Peek "h0" (G.AOp "+" (G.Var "fp") (G.Num 2)) -- heap adress
         else mempty)
  <> genHArgSeq (M.size freeMap) -- reset free vars
  <> genArgSeq (M.size paramMap) -- reset args
  <> genExpSeq fnNames paramMap freeMap rest
\end{code}

Zur Erinnerung: \emph{false} wird in REC als $0$ kodiert und alle anderen
Werte sind \emph{true}. Deshalb wird der pseudo REC Ausdruck
`\lstinline[language=Rec]$if 0 then a else b$' in den pseudo GOTO-Ausdruck
`\lstinline[language=Rec]$IF 0 != 0 THEN a ELSE b END$' übersetzt. Analog für
einzelne Variablen:

\begin{code}
genExpSeq fnNames paramMap freeMap (If (Num n) e2 e3 : rest)
  =  G.IfElse (G.ROp "!=" (G.Num n) (G.Num 0))
              (genExpSeq fnNames paramMap freeMap [e2])
              (genExpSeq fnNames paramMap freeMap [e3])
  <> genExpSeq fnNames paramMap freeMap rest
genExpSeq fnNames paramMap freeMap (If (Var a) e2 e3 : rest)
  =  G.IfElse (G.ROp "!=" (G.Var $ lookup' a paramMap) (G.Num 0))
              (genExpSeq fnNames paramMap freeMap [e2])
              (genExpSeq fnNames paramMap freeMap [e3])
  <> genExpSeq fnNames paramMap freeMap rest
\end{code}

Im Allgemeinen kann sich im `\lstinline[language=Rec]$if$'-Kopf ein beliebiger
REC Ausdruck befinden, der nach Auswertugn eine Ergebniszahl oben auf den
Stack platziert. Dabei werden die relationalen bzw. boolschen Operatoren als
Funktionen interpretiert, die entweder eine $0$ (\emph{false}) oder eine
beliebige andere Zahl wie zum Beispiel $1$ (\emph{true}) liefern:

\begin{code}
genExpSeq fnNames paramMap freeMap (If e1 e2 e3 : rest)
  =  genExpSeq fnNames paramMap freeMap [e1]
  <> G.Pop "t" -- return value of e1
  <> G.IfElse (G.ROp "!=" (G.Var "t") (G.Num 0))
              (genExpSeq fnNames paramMap freeMap [e2])
              (genExpSeq fnNames paramMap freeMap [e3])
  <> genExpSeq fnNames paramMap freeMap rest
\end{code}

Nun können wir endlich die Funktion beschreiben, die eine einzelne REC
Definition übersetzt. Zu beachten ist, dass am Ende eines Abschnitts eine
`\lstinline[language=Goto]$RETURN$` Anweisung vorhanden sein muss um zum
\emph{Caller} zurückzuspringen:

\begin{code}
genDefSec :: [Name] -> Def -> G.Program
genDefSec fns (name, args, exp)
  = G.Label name
    $  genArgSeq (length args)
    <> genExpSeq fns (mkParamMap args) (mkFreeMap []) [exp]
    <> G.Return
\end{code}

Wir haben bisher die Übersetzung nativer Operatoren etwas vernachlässigt.
Ein REC Ausdruck wie `\lstinline[language=Rec]$2 + 3$' könnte eins zu eins in
eine GOTO Addition übersetzt werden. Leider gilt dies nicht für einen Ausdruck
wie `\lstinline[language=Rec]$f(2, 3) + a$', weil erst der Rückgabewert des
Funktionsaufrufes berechnet werden muss, was nicht direkt in GOTO darstellbar
ist. Aus diesem Grund und aus Gründen der Einheitlichkeit werden native
Operatoren fast wie benutzerdefinierte Funktionen übersetzt. Und zwar erhält
jeder Operator seinen eigenen Abschnitt im generierten GOTO Programm. Die
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

Zu beachten ist, dass nur diejenigen Abschnitte in das generierte GOTO Programm
eingefügt werden, deren zugehöriger Operator auch wirklich im REC
Ausgangsprogramm benutzt worden ist. Der Sinn dabei ist es, das generierte
Programm nicht unnötig lang werden zu lassen.

\begin{code}
genOpSec :: [Name] -> [Exp] -> G.Program
genOpSec fns exps
  =  foldr (<>) mempty (map genAorRorBSection calledOps)
  where
  calledFns = getCalledFnNames exps
  calledOps = nub $ calledFns `intersect` operators
  genAorRorBSection op
    | op `elem` ["+","-","*","/","^","%"]    = genArithSection op
    | op `elem` ["=","!=","<","<=",">",">="] = genRelSection   op
    | op `elem` ["&&","||"]                  = genBoolSection  op
    | otherwise                              = error "Impossible!"
  genArithSection op
    = G.Label (labelizeIfOp fns op)
      $  genArgSeq 2
      <> G.Push (G.AOp op (G.Var "a1") (G.Var "a2"))
      <> G.Return
  genRelSection op
    = G.Label (labelizeIfOp fns op)
      $  genArgSeq 2
      <> G.IfElse (G.ROp op (G.Var "a1") (G.Var "a2"))
                  (G.Push (G.Num 1))
                  (G.Push (G.Num 0))
      <> G.Return
  genBoolSection op
    = G.Label (labelizeIfOp fns op)
      $  genArgSeq 2
      <> G.IfElse (G.BOp op
                     (G.ROp "!=" (G.Var "a1") (G.Num 0))
                     (G.ROp "!=" (G.Var "a2") (G.Num 0)))
                  (G.Push (G.Num 1))
                  (G.Push (G.Num 0))
      <> G.Return
\end{code}

Da z.B. `$+$' nicht direkt als Labelbezeichner in GOTO möglich ist, müssen
eindeutige, alphanumerische Namen für die Operatoren im GOTO Programm erzeugt
werden. Dieses leistet |labelizeIfOp|\footnote{Da die Funktion oft mit den
selben Argumente aufgerufen wird, würde sich im Allgemeinen \emph{Memoization}
lohnen, hier verzichten wir aber darauf, da die Eingabeprogrammtexte sehr klein
sind.}:

\begin{code}
labelizeIfOp :: [Name] -> Name -> Name
labelizeIfOp defNames op
  | op `elem` operators = head $ mkLIdStream (opToLabel op) \\ defNames
  | otherwise           = op -- op isn't operator symbol
  where
  mkLIdStream l = l : map (l++) (map show [1..])
  opToLabel op  = lookup' op $ M.fromList
    [ ("+" , "add"), ("-" , "sub")
    , ("*" , "mul"), ("/" , "div")
    , ("^" , "exp"), ("%" , "mod")
    , ("=" , "eq" ), ("!=", "neq")
    , ("<" , "lt" ), ("<=", "leq")
    , (">" , "gt" ), (">=", "geq")
    , ("&&", "and"), ("||", "or" )
    ]
\end{code}

Bisher haben wir uns ebenfalls noch nicht überlegt wie externe Parameter
behandelt werden sollen und überhaupt der Anfang des übersetzten Programs
aussehen soll. Das gestaltelt sich jedoch recht einfach. Die $n$ Parameter der
`\lstinline[language=Rec]$main$'-Funktion sollen bekanntlich mit den Werten der
Variablen `\lstinline[language=Goto]Ix$_1$,...,x$_n$I' ersetzt werden. Dazu werden
diese Variablen am Anfang des Programms einfach in den Stack gepusht. Zusätzlich
müssen wir darauf achten den Frampointer `\lstinline[language=Goto]$fp$' zu
Beginn korrekt zu initialisieren. Da `\lstinline[language=Rec]$main$' der
Eintrittspunkt der Auswertung ist, wird als nächstes der
`\lstinline[language=Rec]$main$'-Abschnitt ``gecallt''. Wir wissen, dass sich
nach der Auswertung das Ergebnis als oberstes Element auf dem Stack befindet.
Dieses wird einfach gepoppt und in `\lstinline[language=Goto]Ix$_0$I' gespeichert.
Schliesslich wird die Rechnung mit dem `\lstinline[language=Goto]$HALT$'-Befehl
beendet:

\begin{code}
genExtArgsSection :: Program -> G.Program
genExtArgsSection defs
  =  G.Seq (map (\i -> G.Push (G.Var $ 'x':show i)) [1..argsLen])
  <> G.Assign "fp" (G.AOp "+" (G.Var "sp") (G.Num 1))
  <> G.Call "main" argsLen
  <> G.Pop "x0"
  <> G.Halt
  where
  (_, mainArgs, _) = findDef "main" defs
  argsLen = length mainArgs
\end{code}

Endlich nun können wir alle Teile zusammenfügen und die Funktion |genGoto|
definieren, die ein beliebiges REC Programm in ein semantisch äquivalentes
GOTO Programm übersetzt:

\begin{code}
genGoto :: Program -> G.Program
genGoto defs = mempty
  <> genExtArgsSection defs
  <> G.Seq (map (genDefSec defNames) defs)
  <> genOpSec defNames defRhss
  <> genLamSec defNames defs
  <> let lamRetSec = genLamRetSec defRhss
     in  if lamRetSec == mempty
            then mempty
            else G.Label "lamret" lamRetSec
  where
  defNames = getDefNames defs
  defRhss  = getDefRhss  defs
\end{code}

Zum Abschluss wollen wir uns die vollständige Übersetzung nach GOTO des
folgenden REC Programms anschauen:

\begin{myindent}{3mm}
\begin{lstlisting}[language=Rec]
compose(f, g) := \x. f(g(x));
twice(f) := \x. compose(f, f, x);
main(a) := twice(\x. x * 2, a)
\end{lstlisting}
\end{myindent}
wird übersetzt zu

\begin{myindent}{3mm}
\begin{lstlisting}[language=Goto]
PUSH x1;
fp := (sp + 1);
CALL main, 1;
x0 := POP;
HALT;

compose: a1 := PEEK (fp - 2);
         a2 := PEEK (fp - 1);
         MAKE_CLOSURE 1,a1,a2;
         PUSH (hp - 2);
         RETURN;

twice: a1 := PEEK (fp - 1);
       MAKE_CLOSURE 2,a1;
       PUSH (hp - 1);
       RETURN;

main: a1 := PEEK (fp - 1);
      PUSH a1;
      MAKE_CLOSURE 3;
      PUSH (hp - 0);
      CALL twice, 1;
      a1 := PEEK (fp - 1);
      t := POP;
      CALL_CLOSURE t, 1;
      a1 := PEEK (fp - 1);
      RETURN;

mul: a1 := PEEK (fp - 2);
     a2 := PEEK (fp - 1);
     PUSH (a1 * a2);
     RETURN;

lambda1: h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         h2 := PEEK_HEAP (h0 + 2);
         a1 := PEEK (fp - 1);
         PUSH a1;
         PUSH h2;
         t := POP;
         CALL_CLOSURE t, 1;
         h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         h2 := PEEK_HEAP (h0 + 2);
         a1 := PEEK (fp - 1);
         PUSH h1;
         t := POP;
         CALL_CLOSURE t, 1;
         h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         h2 := PEEK_HEAP (h0 + 2);
         a1 := PEEK (fp - 1);
         RETURN;

lambda2: h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         a1 := PEEK (fp - 1);
         PUSH a1;
         PUSH h1;
         PUSH h1;
         CALL compose, 2;
         a1 := PEEK (fp - 1);
         t := POP;
         CALL_CLOSURE t, 1;
         h0 := PEEK (fp + 2);
         h1 := PEEK_HEAP (h0 + 1);
         a1 := PEEK (fp - 1);
         RETURN;

lambda3: a1 := PEEK (fp - 1);
         PUSH a1;
         PUSH 2;
         CALL mul, 2;
         a1 := PEEK (fp - 1);
         RETURN;

lamret: IF (cp = 1) THEN
          GOTO lambda1
        END;
        IF (cp = 2) THEN
          GOTO lambda2
        END;
        IF (cp = 3) THEN
          GOTO lambda3
        END
\end{lstlisting}
\end{myindent}


\section{Auswertung}

In diesem Abschnitt werden die Funktionen zur direkten Auswertung eines REC
Programms definiert.

|eval| erhält als Eingabe ein REC Program vom Typ |Program| und eine Liste von
Eingabeparametern. Nach erfolgreicher Auswertung wird, falls das Programm nicht
in eine Endlosschleife geraten ist, die Ergebniszahl zurückgegeben. Und zwar
geschieht die Auswertung so: Zuerst wird aus dem REC Programm das
entsprechende GOTO Programm generiert und anschließend wird das GOTO Programm
mit dem GOTO Auswerter ausgewertet.

\begin{code}
eval :: Program -> [Integer] -> Integer
eval p input = G.eval (genGoto p) input
\end{code}

|run| verknüpft die beiden Funktionen |parse| und |eval|. Im Gegensatz zu
|eval| erwartet |run| also ein REC Programm im Klartext und liefert entweder
die Ergebniszahl oder einen Fehlernachricht. |run'| dagegen liefert bei
jeglichem (Parse-)Fehler als Ergebnis die Zahl $-1$.

\begin{code}
run :: String -> [Integer] -> Either String Integer
run = mkStdRunner parse eval
run' :: String -> [Integer] -> Integer
run' = mkStdRunner' parse eval
\end{code}