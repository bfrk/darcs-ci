-- | This module is a subset of the defunct regex-compat-tdfa.
module Darcs.Util.Regex
    ( Regex
    , mkRegex
    , mkRegexWithOpts
    , matchRegex
    ) where

import Darcs.Prelude

import Text.Regex.Base
    ( RegexContext(matchM)
    , RegexMaker(makeRegexOpts)
    , defaultCompOpt
    , defaultExecOpt
    )
import Text.Regex.TDFA (Regex, caseSensitive, multiline, newSyntax)

-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions).
mkRegex :: String -> Regex
mkRegex s = makeRegexOpts opt defaultExecOpt s
  where
    opt = defaultCompOpt {newSyntax = True, multiline = True}

-- | Makes a regular expression, where the multi-line and
-- case-sensitive options can be changed from the default settings.
mkRegexWithOpts
   :: String  -- ^ The regular expression to compile
   -> Bool    -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and 
              -- end of individual lines respectively, and @\'.\'@ does /not/
              -- match the newline character.
   -> Bool    -- ^ 'True' @\<=>@ matching is case-sensitive
   -> Regex   -- ^ Returns: the compiled regular expression
mkRegexWithOpts s single_line case_sensitive
  = let opt = defaultCompOpt
                { multiline    = (if single_line then True else False)
                , caseSensitive = (if case_sensitive then True else False)
                , newSyntax     = True }
    in makeRegexOpts opt defaultExecOpt s

-- | Match a regular expression against a string
matchRegex ::
     Regex -- ^ The regular expression
  -> String -- ^ The string to match against
  -> Maybe [String] -- ^ Returns: @'Just' strs@ if the match succeeded
                      -- (and @strs@ is the list of subexpression matches),
                      -- or 'Nothing' otherwise.
matchRegex p str = fmap go (matchM p str)
  where
    go :: (String, String, String, [String]) -> [String]
    go (_, _, _, ss) = ss
