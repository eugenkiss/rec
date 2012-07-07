\chapter{Hilfsfunktionen}

\begin{code}
-- | General utility functions for parsing/evaluation.
module Util
  ( mkStdRunner
  , mkStdRunner'
  , mkStdParser
  , mkStdParser'
  ) where

import Text.ParserCombinators.Parsec hiding (Parser)


type Parser a = String -> Either String a

type Evaluator a = a -> [Integer] -> Integer

-- | Given a parser and evaluator of type 'a', a string representation of
-- a program and a list of the values for 'x1,x2,...' try to evaluate the
-- program. If succesful return the value stored in 'x0' otherwise return an
-- error message.
mkStdRunner :: Parser a -> Evaluator a -> String -> [Integer] -> Either String Integer
mkStdRunner parser evaluator = \code args ->
    case parser code of
      Left err  -> Left err
      Right ast -> Right $ evaluator ast args

-- | A variation of 'mkStdRunner'. Instead of returning an error message the
-- value '-1' is returned. Since '-1' is not an allowed value for any variable
-- it represents an error. By using this function some "non-critical" functions
-- are easier to write because they do not need to pattern match against an
-- Either.
mkStdRunner' :: Parser a -> Evaluator a -> String -> [Integer] -> Integer
mkStdRunner' parser evaluator = \code args ->
    case (mkStdRunner parser evaluator) code args of
      Left _    -> -1
      Right res -> res
\end{code}

On the one hand `mkStdParser` discards any leading whitespace and forces the
passed in parser to parse the rest of the input string. On the other hand
`mkStdParser` wraps the result of the passed in parser in an `Either` data type so
that error handling is possible.

\begin{code}
-- | Given a parser 'p' of program 'a', an initial state 'st' and an associated
-- "whiteSpace"-parser 'ws', i.e. a parser that skips whitespace, newlines,
-- comments etc., make a "standard" parser that receives a string of a program
-- and returns either an AST of type 'a' or an error message. The parser skips
-- whitespace at the beginning of the input and consumes input according to 'p'
-- until the end of the file/string.
mkStdParser :: GenParser Char st a -> st -> GenParser Char st () -> String -> Either String a
mkStdParser p st ws = \code ->
    case runParser p' st "" code of
      Left err  -> Left $ show err
      Right val -> Right val
  where p' = do ws
                x <- p
                eof
                return x

mkStdParser' :: GenParser Char st a -> st -> GenParser Char st () -> String -> a
mkStdParser' p st ws = \code ->
    case runParser p' st "" code of
      Left err  -> error $ show err
      Right val -> val
  where p' = do ws
                x <- p
                eof
                return x
\end{code}