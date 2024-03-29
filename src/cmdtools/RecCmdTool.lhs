\section{Kommandozeilenprogramm}

Zu guter Letzt wird in diesem Abschnitt das Kommandozeilenprogram @rec@
definiert mit Hilfe der Bibliothek \emph{CmdArgs}. Es würde zu weit
gehen im Detail auf den Quellcode einzugehen. Es wird empfohlen auf
die Dokumentation von CmdArgs zurückzugreifen. Trotzdem wird hier
kurz angerissen welche Operationen das Kommandozeilenprogramm @rec@
aufweisen soll.

\begin{itemize}
\item Ausführung von Programmtexten: Dazu einfach @rec [DATEIPFAD]@\\
(z.B. @rec examples/churchifs.rec@). Möchte man Argumente übergeben,
so geht das so @rec examples/fac.rec -a 1,2,3@. Man kann auch
explizit sein und @rec eval examples/fac.rec -a 1,2,3@ schreiben.

\item Generierung von GOTO-Code: Dazu einfach @rec gengoto [DATEIPFAD]@
(z.B. @rec gengoto examples/churchifs.rec@).

\item Pretty Printing von Programmtexten: Dazu einfach @rec print [DATEIPFAD]@
(z.B. @rec print examples/churchifs.rec@).

\item Falls man jemals etwas vergisst hilft der Befehl @rec --help@.
\end{itemize}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Exit
import System.Console.CmdArgs
import Text.PrettyPrint.HughesPJ hiding (mode)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)

import Rec
import qualified Goto as G

data Mode
  = Eval    { argList :: String, file :: FilePath }
  | GenGoto { file :: String }
  | Print   { file :: String }
  deriving (Show, Data, Typeable)

evalMode
  = Eval
  { file    = def &= args &= typFile
  , argList = def &= typ "INTLIST"
  } &= help "Evaluate the program" &= auto

genGotoMode
  = GenGoto
  { file = def &= args &= typFile
  } &= help "Generate Goto code"

printMode
  = Print
  { file = def &= args &= typFile
  } &= help "Pretty print code"

mode =  cmdArgsMode $ modes [evalMode, genGotoMode, printMode]
     &= program "rec"
     &= summary "Rec interpreter"
     &= versionArg [ignore]
main = do
  a <- cmdArgsRun mode
  case a of
    Eval {file=f,argList=as} -> do
      as <- case parseArgList as of
                 Left _    -> do putStrLn "Error parsing arguments! Example: 4,10,2"
                                 exitFailure
                                 return [0]
                 Right as' -> return as'
      code <- case f of
                "" -> getContents
                _  -> readFile f
      case run code as of
        Right v -> putStrLn $ show v
        Left  e -> putStrLn e
    GenGoto {file=f} -> do
      code <- case f of
                "" -> getContents
                _  -> readFile f
      case parse code of
        Right p -> putStrLn $ show $ pprint' $ genGoto p
        Left  e -> putStrLn e
    Print {file=f} -> do
      code <- case f of
                "" -> getContents
                _  -> readFile f
      case parse code of
        Right p -> putStrLn $ pprint p
        Left  e -> putStrLn e
  where
  parseArgList input = Parsec.parse argList "(unknown)" input
  argList = commaSep natural

lexer    = Token.makeTokenParser haskellDef
commaSep = Token.commaSep lexer
natural  = Token.natural  lexer
\end{code}

Es ist sinnvoll den generierten Goto Code speziell zu formatieren und zwar so,
dass die Abschnitte durch freie Zeilen getrennt sind.

\begin{code}
pprint' :: G.Program -> Doc
pprint' p@(G.Label _ _)
  = zeroWidthText "\n" <> text (let r = G.pprint p in case last r of
      ';' -> init r
      _   -> r)
pprint' (G.Seq ps)
  = vcat $ punctuate semi $ map pprint' ps
pprint' x = text $ G.pprint x
\end{code}