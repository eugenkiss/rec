\subsection{Kommandozeilenprogramm}

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
