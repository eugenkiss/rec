import Language.LoopGotoWhile.Goto.Extended
import Language.LoopGotoWhile.Goto.Transform

main = do
  s <- readFile "stack.goto"
  case parse s of
    Left e  -> putStrLn e
    Right ast -> putStrLn $ show $ toStrict ast
  case run s [0] of
    Left e  -> putStrLn e
    Right n -> putStrLn $ show n
