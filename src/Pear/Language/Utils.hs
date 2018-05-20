module Pear.Language.Utils where

import Text.Parsec as P

data SourceSpan =
  SourceSpan { ssStart :: P.SourcePos
             , ssEnd :: P.SourcePos
             } deriving  (Show)
