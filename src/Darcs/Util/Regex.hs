-- | This module is a subset of the defunct regex-compat-tdfa.
{-# LANGUAGE CPP #-}
module Darcs.Util.Regex
    ( Regex
    , mkRegex
    , mkRegexWithOpts
    , matchRegex
    ) where

import Darcs.Prelude

import Control.Exception ( throw )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif
import Text.Regex.Base
    ( RegexContext(matchM)
    , RegexMaker(makeRegexOptsM)
    , defaultCompOpt
    , defaultExecOpt
    )
import Text.Regex.TDFA ( Regex, caseSensitive, multiline, newSyntax )

-- | The "sane" API for regex ('makeRegexOptM') requires 'MonadFail'
-- but we want a pure one for compatibility with e.g. "Darcs.Patch.Match".
newtype RegexFail a = RegexFail { runRegexFail :: Either String a }
  -- The subtlety here is that only in base-4.13.0 the fail method
  -- in class Monad was removed. For earlier versions, regex-tdfa
  -- calls the fail from class Monad, not the one from class MonadFail.
#if MIN_VERSION_base(4,13,0)
  deriving (Functor, Applicative, Monad)
#else
  deriving (Functor, Applicative)

instance Monad RegexFail where
  RegexFail (Left e) >>= _ = RegexFail (Left e)
  RegexFail (Right r) >>= k = k r
  fail = RegexFail . Left
#endif

instance MonadFail RegexFail where
  fail = RegexFail . Left

-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions).
mkRegex :: String -> Regex
mkRegex s = mkRegexInternal opt s
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
    in mkRegexInternal opt s

mkRegexInternal :: RegexMaker p compOpt execOpt String => compOpt -> String -> p
mkRegexInternal opt s =
  case runRegexFail (makeRegexOptsM opt defaultExecOpt s) of
    Left e -> throw (userError ("Invalid regular expression:\n" ++ e))
    Right r -> r

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
