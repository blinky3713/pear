module Pear.Language.Utils where

import Text.Parsec as P

data SourceSpan =
  SourceSpan { ssStart :: P.SourcePos
             , ssEnd :: P.SourcePos
             } deriving  (Show)

-- | Read source position information
withSourceSpan
  :: (SourceSpan -> a -> b)
  -> P.Parsec s u a
  -> P.Parsec s u b
withSourceSpan f p = do
  start <- P.getPosition
  x <- p
  end <- P.getPosition
  let sp = SourceSpan start end
  return $ f sp x

withSourceSpanF
  :: P.Parsec s u (SourceSpan -> a)
  -> P.Parsec s u a
withSourceSpanF = withSourceSpan (\ss f -> f ss)


-- | Build a parser from a smaller parser and a list of parsers for postfix operators
buildPostfixParser :: P.Stream s m t => [a -> P.ParsecT s u m a] -> P.ParsecT s u m a -> P.ParsecT s u m a
buildPostfixParser fs first' = do
  a <- first'
  go a
  where
  go a = do
    maybeA <- P.optionMaybe $ P.choice (map ($ a) fs)
    case maybeA of
      Nothing -> return a
      Just a' -> go a'
