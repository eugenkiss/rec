\section{Kommandozeilenprogramm}

Wie auch beim Kommandozeilenprogramm für REC wird hier nur kurz
aufgelistet, welche Operationen das Kommandozeilenprogramm @goto@
anbietet:

\begin{itemize}
\item Ausführung von Programmtexten: Dazu einfach @goto [DATEIPFAD]@\\
(z.B. @goto examples/fac.goto@). Möchte man Argumente übergeben,
so geht das so @goto examples/fac.goto -a 1,2,3@. Man kann auch
explizit sein und @goto eval examples/fac.goto -a 1,2,3@ schreiben.

\item Generierung von REC-Code: Dazu einfach @goto genrec [DATEIPFAD]@
(z.B. @goto genrec examples/fac.goto@).

\item Pretty Printing von Programmtexten: Dazu einfach @goto print [DATEIPFAD]@
(z.B. @goto print examples/fac.goto@).

\item Falls man jemals etwas vergisst hilft der Befehl @goto --help@.
\end{itemize}

% TODO: Einige Sachen nach Util auslagern

Zu guter Letzt wird in diesem Abschnitt das Kommandozeilenprogram @goto@
definiert..

> {-# LANGUAGE DeriveDataTypeable #-}
>
> module Main where
>
> import System.Exit
> import System.Console.CmdArgs
> import qualified Text.Parsec as Parsec
> import qualified Text.Parsec.Token as Token
> import Text.Parsec.Language (haskellDef)
> import Goto
> import GotoToRec
> import qualified Rec

> data Mode
>   = Eval       { argList :: String, file :: FilePath }
>   | Desugar    { file :: String }
>   | Simplify   { file :: String }
>   | Strictify  { file :: String }
>   | GenRec     { file :: String }
>   | Print      { file :: String }
>   deriving (Show, Data, Typeable)
>
> evalMode
>   = Eval
>   { file    = def &= args &= typFile
>   , argList = def &= typ "INTLIST"
>   } &= help "Evaluate the program" &= auto
>
> desugarMode
>   = Desugar
>   { file = def &= args &= typFile
>   } &= help "Desugar code"
>
> simplifyMode
>   = Simplify
>   { file = def &= args &= typFile
>   } &= help "Simplify code"
>
> strictifyMode
>   = Strictify
>   { file = def &= args &= typFile
>   } &= help "Convert code to strict subset"
>
> genRecMode
>   = GenRec
>   { file = def &= args &= typFile
>   } &= help "Generate Rec code"
>
> printMode
>   = Print
>   { file = def &= args &= typFile
>   } &= help "Pretty print code"
>
> mode =  cmdArgsMode $ modes [evalMode, desugarMode, simplifyMode, strictifyMode, genRecMode, printMode]
>      &= program "goto"
>      &= summary "Goto interpreter"
>      &= versionArg [ignore]
> main = do
>   a <- cmdArgsRun mode
>   case a of
>     Eval {file=f,argList=as} -> do
>       as <- case parseArgList as of
>                  Left _    -> do putStrLn "Error parsing arguments! Example: 4,10,2"
>                                  exitFailure
>                                  return [0]
>                  Right as' -> return as'
>       code <- case f of
>                 "" -> getContents
>                 _  -> readFile f
>       case run code as of
>         Right v -> putStrLn $ show v
>         Left  e -> putStrLn e
>     Desugar {file=f} -> do
>       code <- case f of
>                 "" -> getContents
>                 _  -> readFile f
>       case parse code of
>         Right p -> putStrLn $ show $ desugar p
>         Left  e -> putStrLn e
>     Simplify {file=f} -> do
>       code <- case f of
>                 "" -> getContents
>                 _  -> readFile f
>       case parse code of
>         Right p -> putStrLn $ show $ simplify p
>         Left  e -> putStrLn e
>     Strictify {file=f} -> do
>       code <- case f of
>                 "" -> getContents
>                 _  -> readFile f
>       case parse code of
>         Right p -> putStrLn $ show $ strictify p
>         Left  e -> putStrLn e
>     GenRec {file=f} -> do
>       code <- case f of
>                 "" -> getContents
>                 _  -> readFile f
>       case parse code of
>         Right p -> putStrLn $ Rec.pprint $ genRec p
>         Left  e -> putStrLn e
>     Print {file=f} -> do
>       code <- case f of
>                 "" -> getContents
>                 _  -> readFile f
>       case parse code of
>         Right p -> putStrLn $ show p
>         Left  e -> putStrLn e
>   where
>   parseArgList input = Parsec.parse argList "(unknown)" input
>   argList = commaSep natural
>
> lexer    = Token.makeTokenParser haskellDef
> commaSep = Token.commaSep lexer
> natural  = Token.natural  lexer
