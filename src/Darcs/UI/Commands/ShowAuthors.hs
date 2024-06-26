--  Copyright (C) 2004-2009 David Roundy, Eric Kow, Simon Michael, Tomas Caithaml
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

-- to suppress an irrelevant warning GHC 9.10 and 9.12
-- NE.unzip is shifting to being monomorphic but our usage already is
{-# OPTIONS_GHC -Wno-x-data-list-nonempty-unzip #-}
module Darcs.UI.Commands.ShowAuthors
    ( showAuthors, Spelling, compiledAuthorSpellings, canonizeAuthor, rankAuthors
    ) where

import Control.Arrow ( (&&&), (***) )
import Data.Char ( toLower, isSpace )
import Data.Function ( on )
import Data.List ( isInfixOf, sortBy, sort )
import Data.List.NonEmpty ( group, groupBy )
import qualified Data.List.NonEmpty as NE
import Data.Maybe( isJust )
import Data.Ord ( comparing )
import System.IO.Error ( catchIOError )
import Text.ParserCombinators.Parsec hiding ( lower, count, Line )
import Text.ParserCombinators.Parsec.Error

import Darcs.Prelude

import Darcs.UI.Flags ( DarcsFlag, useCache, verbose )
import Darcs.UI.Options ( oid, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, putWarning, amInRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.External ( viewDoc )
import Darcs.Patch.PatchInfoAnd ( info )
import Darcs.Patch.Info ( piAuthor )
import Darcs.Patch.Set ( patchSet2RL )
import Darcs.Repository ( readPatches, withRepository, RepoJob(..) )
import Darcs.Patch.Witnesses.Ordered ( mapRL )
import Darcs.Util.Lock ( readTextFile )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Regex ( Regex, mkRegexWithOpts, matchRegex )

data Spelling = Spelling String String [Regex] -- name, email, regexps
type ParsedLine = Maybe Spelling -- Nothing for blank lines

showAuthorsDescription :: String
showAuthorsDescription = "List authors by patch count."

showAuthorsHelp :: Doc
showAuthorsHelp = text $
 "The `darcs show authors` command lists the authors of the current\n" ++
 "repository, sorted by the number of patches contributed.  With the\n" ++
 "`--verbose` option, this command simply lists the author of each patch\n" ++
 "(without aggregation or sorting).\n" ++
 "\n" ++
 "An author's name or email address may change over time.  To tell Darcs\n" ++
 "when multiple author strings refer to the same individual, create an\n" ++
 "`.authorspellings` file in the root of the working tree.  Each line in\n" ++
 "this file begins with an author's canonical name and address, and may\n" ++
 "be followed by a comma separated list of extended regular expressions.\n" ++
 "Blank lines and lines beginning with two hyphens are ignored.\n" ++
 "The format of `.authorspelling` can be described by this pattern:\n" ++
 "\n" ++
 "    name <address> [, regexp ]*\n" ++
 "\n" ++
 "There are some pitfalls concerning special characters:\n" ++
 "Whitespaces are stripped, if you need space in regexp use [ ]. \n" ++
 "Because comma serves as a separator you have to escape it if you want\n" ++
 "it in regexp. Note that `.authorspelling` use extended regular\n" ++
 "expressions so +, ? and so on are metacharacters and you need to \n" ++
 "escape them to be interpreted literally.\n" ++
 "\n" ++
 "Any patch with an author string that matches the canonical address or\n" ++
 "any of the associated regexps is considered to be the work of that\n" ++
 "author.  All matching is case-insensitive and partial (it can match a\n" ++
 "substring). Use ^,$ to match the whole string in regexps\n" ++
 "\n" ++
 "Currently this canonicalization step is done only in `darcs show\n" ++
 "authors`.  Other commands, such as `darcs log` use author strings\n" ++
 "verbatim.\n" ++
 "\n" ++
 "An example `.authorspelling` file is:\n" ++
 "\n" ++
 "    -- This is a comment.\n" ++
 "    Fred Nurk <fred@example.com>\n" ++
 "    John Snagge <snagge@bbc.co.uk>, John, snagge@, js@(si|mit).edu\n" ++
 "    Chuck Jones\\, Jr. <chuck@pobox.com>, cj\\+user@example.com\n"

showAuthors :: DarcsCommand
showAuthors = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "authors"
    , commandHelp = showAuthorsHelp
    , commandDescription = showAuthorsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = authorsCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandOptions = showAuthorsOpts
    }
  where
    showAuthorsBasicOpts = O.repoDir
    showAuthorsOpts = showAuthorsBasicOpts `withStdOpts` oid

authorsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
authorsCmd _ flags _ = withRepository (useCache ? flags) $ RepoJob $ \repository -> do
    patches <- readPatches repository
    spellings <- compiledAuthorSpellings flags
    let authors = mapRL (piAuthor . info) $ patchSet2RL patches
    viewDoc $ text $ unlines $
        if verbose flags
            then authors
            else rankAuthors spellings authors

rankAuthors :: [Spelling] -> [String] -> [String]
rankAuthors spellings authors =
              -- A list of the form ["#<rank> <count> <canonical name>"].
              -- Turn the final result into a list of strings.
              map (\ (rank, (count, name)) -> "#" ++ show rank ++ "\t"
                                              ++ show count ++ "\t" ++ name) .
              zip ([1..] :: [Int]) .
              -- Sort by descending patch count.
              reverse $ sortBy (comparing fst) .
              -- Combine duplicates from a list [(count, canonized name)]
              -- with duplicates canonized names (see next comment).
              map ((sum *** NE.head) . NE.unzip) .
              groupBy ((==) `on` snd) .
              sortBy  (comparing snd) .
              -- Because it would take a long time to canonize "foo" into
              -- "foo <foo@bar.baz>" once per patch, the code below
              -- generates a list [(count, canonized name)].
              map (length &&& (canonizeAuthor spellings . NE.head)) .
              group $ sort authors

canonizeAuthor :: [Spelling] -> String -> String
canonizeAuthor spells author = getName canonicals
  where
    getName [] = author
    getName (Spelling name email _ : _) = name ++ " <" ++ email ++ ">"
    canonicals = filter (ismatch author) spells
    ismatch s (Spelling _ mail regexps) =
        s `correspondsTo` mail || any (s `contains_regex`) regexps
    contains_regex a r = isJust $ matchRegex r a
    correspondsTo a b = lower b `isInfixOf` lower a
    lower = map toLower

compiledAuthorSpellings :: [DarcsFlag] -> IO [Spelling]
compiledAuthorSpellings flags = do
    let as_file = ".authorspellings"
    content_lines <- readTextFile as_file `catchIOError` (const (return []))
    let parse_results = map (parse sentence as_file) content_lines
    clean 1 parse_results
  where
    clean :: Int -> [Either ParseError ParsedLine] -> IO [Spelling]
    clean _ [] = return []
    -- print parse error
    clean n (Left err : xs) = do
      let npos = setSourceLine (errorPos err) n
      putWarning flags . text . show $ setErrorPos npos err
      clean (n + 1) xs
    -- skip blank line
    clean n (Right Nothing : xs)  = clean (n + 1) xs
    -- unwrap Spelling
    clean n (Right (Just a) : xs) = do
      as <- clean (n + 1) xs
      return (a : as)

----------
-- PARSERS

sentence :: Parser ParsedLine
sentence = spaces >> (comment <|> blank <|> addressline)
  where
    comment = string "--" >> return Nothing
    blank = eof >> return Nothing

addressline :: Parser ParsedLine
addressline = do
    name <- canonicalName <?> "Canonical name"
    addr <- between (char '<') (char '>') (many1 (noneOf ">")) <?> "Address"
    spaces
    rest <- option [] (char ',' >> regexp `sepBy` char ',')
            <?> "List of regexps"
    return $ Just $ Spelling (strip name) addr (compile rest)
  where
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    makeRegex s = mkRegexWithOpts s True False
    compile = map makeRegex . filter (not . null) . map strip

    parseComma = string "\\," >> return ','

    regexp :: Parser String
    regexp = many1 p <?> "Regular expression"
      where
        p = try parseComma <|> noneOf ","

    canonicalName :: Parser String
    canonicalName = many1 p
      where
        p = try parseComma <|> noneOf ",<"
