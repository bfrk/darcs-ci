--  Copyright (C) 2004-2005 David Roundy
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

-- | /First matcher, Second matcher and Nonrange matcher/
--
-- When we match for patches, we have a PatchSet, of which we want a
-- subset. This subset is formed by the patches in a given interval
-- which match a given criterion. If we represent time going left to
-- right, then we have (up to) three 'Matcher's:
--
-- * the 'firstMatcher' is the left bound of the interval,
--
-- * the 'secondMatcher' is the right bound, and
--
-- * the 'nonrangeMatcher' is the criterion we use to select among
--   patches in the interval.
---
-- Each of these matchers can be present or not according to the
-- options. The patches we want would then be the ones that all
-- present matchers have in common.
--
-- Alternatively, match flags can also be understood as a 'patchSetMatch'.
-- This (ab-)uses match flags that normally denote a 'nonrangeMatcher',
-- (additionally including the 'OneIndex' flag --index=n), to denote
-- selection of a full 'PatchSet' up to the latest matching patch. This
-- works similar to 'secondMatcher' except for tag matches, which in this
-- case mean to select only the tag and all its dependencies. In other
-- words, the tag will be clean in the resulting 'PatchSet'.
--
-- (Implementation note: keep in mind that the PatchSet is written
-- backwards with respect to the timeline, ie., from right to left)
module Darcs.Patch.Match
    ( helpOnMatchers
    , matchFirstPatchset
    , matchSecondPatchset
    , splitSecondFL
    , matchAPatch
    , rollbackToPatchSetMatch
    , firstMatch
    , secondMatch
    , haveNonrangeMatch
    , PatchSetMatch
    , patchSetMatch
    , checkMatchSyntax
    , hasIndexRange
    , getMatchingTag
    , matchAPatchset
    , matchOnePatchset
    , MatchFlag(..)
    , matchingHead
    , Matchable
    , MatchableRP
    ) where

import Darcs.Prelude

import Text.ParserCombinators.Parsec
    ( parse
    , CharParser
    , (<?>)
    , (<|>)
    , noneOf
    , option
    , eof
    , many
    , try
    , between
    , spaces
    , char
    , oneOf
    , string
    , choice
    )
import Text.ParserCombinators.Parsec.Expr
    ( OperatorTable
    , Assoc( AssocLeft )
    , Operator ( Infix, Prefix )
    , buildExpressionParser
    )
import Darcs.Util.Regex ( mkRegex, matchRegex )

import Control.Exception ( Exception, throw )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafePerformIO )
import Data.List ( isPrefixOf, intercalate )
import Data.Char ( toLower )
import Data.Typeable ( Typeable )

import Darcs.Util.Path ( AbsolutePath, toFilePath )
import Darcs.Patch ( hunkMatches, listTouchedFiles )
import Darcs.Patch.Bundle ( readContextFile )
import Darcs.Patch.Info ( justName, justAuthor, justLog, makePatchname,
                          piDate, piTag )

import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Set
    ( Origin
    , PatchSet(..)
    , SealedPatchSet
    , Tagged(..)
    , patchSetDrop
    )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Depends ( splitOnTag, contextPatches )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Inspect ( PatchInspect )

import Darcs.Patch.Witnesses.Ordered
    ( RL(..), FL(..), (:>)(..), reverseRL, mapRL, (+<+) )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed2(..), seal, seal2, unseal2, unseal )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )

import Darcs.Util.DateMatcher ( parseDateMatcher )
import Darcs.Util.Path ( anchorPath )
import Darcs.Util.Tree ( Tree )

-- | Patches that can be matched.
type Matchable p =
  ( Apply p
  , PatchInspect p
  , Ident p
  , PatchId p ~ PatchInfo
  )

-- | Constraint for a patch type @p@ that ensures @'PatchInfoAnd' p@
-- is 'Matchable'.
type MatchableRP p =
  ( Apply p
  , Commute p
  , PatchInspect p
  )

-- | A type for predicates over patches which do not care about
-- contexts
data MatchFun = MatchFun (forall p. Matchable p => Sealed2 p -> Bool)

-- | A @Matcher@ is made of a 'MatchFun' which we will use to match
-- patches and a @String@ representing it.
data Matcher = MATCH String MatchFun

instance Show Matcher where
    show (MATCH s _) = '"':s ++ "\""

data MatchFlag
    = OnePattern String
    | SeveralPattern String
    | AfterPattern String
    | UpToPattern String
    | OnePatch String
    | SeveralPatch String
    | AfterPatch String
    | UpToPatch String
    | OneHash String
    | AfterHash String
    | UpToHash String
    | OneTag String
    | SeveralTag String
    | AfterTag String
    | UpToTag String
    | LastN Int
    | OneIndex Int
    | IndexRange Int Int
    | Context AbsolutePath
    deriving (Show)

makeMatcher :: String -> MatchFun -> Matcher
makeMatcher = MATCH

-- | @applyMatcher@ applies a matcher to a patch.
applyMatcher :: Matchable p => Matcher -> p wX wY -> Bool
applyMatcher (MATCH _ (MatchFun m)) = m . seal2

parseMatch :: String -> Either String Matcher
parseMatch pattern =
    case parse matchParser "match" pattern of
    Left err -> Left $ "Invalid --match pattern '"++ pattern ++
                "'.\n"++ unlines (map ("    "++) $ lines $ show err) -- indent
    Right m -> Right (makeMatcher pattern m)

patternmatch :: String -> Matcher
patternmatch pattern =
    case parseMatch pattern of
    Left err -> error err
    Right m -> m

matchParser :: CharParser st MatchFun
matchParser = (option matchAnyPatch expr <* eof) <?> helpfulErrorMsg
  where
    -- When using <?>, Parsec prepends "expecting " to the given error message,
    -- so the phrasing below makes sense.
    helpfulErrorMsg =
      "valid expressions over: "
      ++ intercalate ", " (map (\(name, _, _, _, _) -> name) primitiveMatchers)
      ++ "\nfor more help, see `darcs help patterns`."

    -- matchAnyPatch is returned if expr fails without consuming any
    -- input, i.e. if we pass --match '', we want to match anything
    matchAnyPatch = MatchFun (const True)

    -- parse a non-empty full match expression
    expr :: CharParser st MatchFun
    expr = buildExpressionParser table term

    table :: OperatorTable Char st MatchFun
    table =
      [ [ prefix "not" negate_match, prefix "!" negate_match ]
      , [ binary "||" or_match
        , binary "or" or_match
        , binary "&&" and_match
        , binary "and" and_match
        ]
      ]
      where
        binary name result = Infix (operator name result) AssocLeft
        prefix name result = Prefix $ operator name result
        operator name result = try (string name) >> spaces >> return result
        negate_match (MatchFun m) = MatchFun $ \p -> not (m p)
        or_match (MatchFun m1) (MatchFun m2) = MatchFun $ \p -> m1 p || m2 p
        and_match (MatchFun m1) (MatchFun m2) = MatchFun $ \p -> m1 p && m2 p

    -- parse a term, i.e. anything we can combine with operators:
    -- an expression in parentheses or a primitive match expression
    term :: CharParser st MatchFun
    term = between spaces spaces (parens expr <|> choice prims)
      where
        -- the primitive match expression parsers
        prims = map prim primitiveMatchers
        -- a primitive match expression is a keyword followed by an argument;
        -- the result is the passed matcher applied to the argument
        prim (key, _, _, _, matcher) =
          fmap matcher $ try (string key) >> spaces >> argument
        -- an argument in a primitive match expression
        argument :: CharParser st String
        argument = quoted <|> unquoted <?> "string"
        -- quoted string
        quoted =
          between (char '"') (char '"') (many $ try escaped <|> noneOf "\"")
        -- bare (unquoted) string
        unquoted = between spaces spaces (many $ noneOf " ()")
        -- backslash escaped double quote or backslash
        escaped = char '\\' >> oneOf "\\\""
        -- any expression in parentheses
        parens = between (string "(") (string ")")

-- | The string that is emitted when the user runs @darcs help patterns@.
helpOnMatchers :: [String]
helpOnMatchers =
  ["Selecting Patches:",
   "",
   "The --patches option yields patches with names matching an *extended*",
   "regular expression.  See regex(7) for details.  The --matches option",
   "yields patches that match a logical (Boolean) expression: one or more",
   "primitive expressions combined by grouping (parentheses) and the",
   "complement (not), conjunction (and) and disjunction (or) operators.",
   "The C notation for logic operators (!, && and ||) can also be used.",
   "",
   "    --patches=regex is a synonym for --matches='name regex'",
   "    --hash=HASH is a synonym for --matches='hash HASH'",
   "    --from-patch and --to-patch are synonyms for",
   "      --from-match='name... and --to-match='name...",
   "    --from-hash and --to-hash are synonyms for",
   "      --from-match='hash...' and --to-match='hash...'",
   "    sensible combinations of --from-* and --to-* options are possible:",
   "      `darcs log --from-patch='html.*docu' --to-match='date 20040212'`",
   "      `darcs log --from-hash=368089c6969 --to-patch='^fix.*renamed or moved\\.$'`",
   "",
   "The following primitive Boolean expressions are supported:"
   ,""]
  ++ keywords
  ++ ["", "Here are some examples:", ""]
  ++ examples
  where ps = primitiveMatchers
        keywords = [showKeyword (unwords [k,a]) d | (k,a,d,_,_) <- ps]
        examples = [showExample k e | (k,_,_,es,_) <- ps, e <- es]
        showKeyword keyword description =
            "    " ++ keyword ++ " - " ++ description ++ "."
        showExample keyword example =
            "    darcs log --match "
            ++ "'" ++ keyword ++ " " ++ example ++ "'"

primitiveMatchers :: [(String, String, String, [String], String -> MatchFun)]
                     -- ^ keyword (operator), argument name, help description, list
                     -- of examples, matcher function
primitiveMatchers =
 [ ("exact", "STRING", "check literal STRING is equal to patch name"
           , ["\"Resolve issue17: use dynamic memory allocation.\""]
           , exactmatch )
 , ("name", "REGEX", "match REGEX against patch name"
          , ["issue17", "\"^[Rr]esolve issue17\\>\""]
          , namematch )
 , ("author", "REGEX", "match REGEX against patch author"
            , ["\"David Roundy\"", "droundy", "droundy@darcs.net"]
            , authormatch )
 , ("hunk", "REGEX", "match REGEX against contents of a hunk patch"
            , ["\"foo = 2\"", "\"^instance .* Foo where$\""]
            , hunkmatch )
 , ("comment", "REGEX", "match REGEX against the full log message"
         , ["\"prevent deadlocks\""]
         , logmatch )
 , ("hash", "HASH", "match HASH against (a prefix of) the hash of a patch"
          ,  ["c719567e92c3b0ab9eddd5290b705712b8b918ef","c7195"]
          ,  hashmatch )
 , ("date", "DATE", "match DATE against the patch date"
          , ["\"2006-04-02 22:41\"", "\"tea time yesterday\""]
          , datematch )
 , ("touch", "REGEX", "match file paths for a patch"
          , ["src/foo.c", "src/", "\"src/*.(c|h)\""]
          , touchmatch ) ]

datematch, hashmatch, authormatch, exactmatch, namematch, logmatch,
  hunkmatch, touchmatch :: String -> MatchFun

namematch r =
  MatchFun $ \(Sealed2 hp) ->
    isJust $ matchRegex (mkRegex r) $ justName (ident hp)

exactmatch r = MatchFun $ \(Sealed2 hp) -> r == justName (ident hp)

authormatch a =
  MatchFun $ \(Sealed2 hp) ->
    isJust $ matchRegex (mkRegex a) $ justAuthor (ident hp)

logmatch l =
  MatchFun $ \(Sealed2 hp) ->
    isJust $ matchRegex (mkRegex l) $ justLog (ident hp)

hunkmatch r =
  MatchFun $ \(Sealed2 hp) ->
    let regexMatcher = isJust . matchRegex (mkRegex r) . BC.unpack
     in hunkMatches regexMatcher hp

hashmatch h =
  MatchFun $ \(Sealed2 hp) ->
    let rh = show $ makePatchname (ident hp)
        lh = map toLower h
     in (lh `isPrefixOf` rh) || (lh == rh ++ ".gz")

datematch d =
  MatchFun $ \(Sealed2 hp) ->
    let dm = unsafePerformIO $ parseDateMatcher d
     in dm $ piDate (ident hp)

touchmatch r =
  MatchFun $ \(Sealed2 hp) ->
    let files = listTouchedFiles hp
     in any (isJust . matchRegex (mkRegex r)) (map (anchorPath ".") files)

-- | @haveNonrangeMatch flags@ tells whether there is a flag in
-- @flags@ which corresponds to a match that is "non-range". Thus,
-- @--match@, @--patch@, and @--hash@ make @haveNonrangeMatch@
-- true, but not @--from-patch@ or @--to-patch@.
haveNonrangeMatch :: [MatchFlag] -> Bool
haveNonrangeMatch fs = isJust (nonrangeMatcher fs)

data PatchSetMatch
  = IndexMatch Int
  | PatchMatch Matcher
  | TagMatch Matcher
  | ContextMatch AbsolutePath

patchSetMatch :: [MatchFlag] -> Maybe PatchSetMatch
patchSetMatch [] = Nothing
patchSetMatch (OneTag t:_) = strictJust $ TagMatch $ tagmatch t
patchSetMatch (OnePattern m:_) = strictJust $ PatchMatch $ patternmatch m
patchSetMatch (OnePatch p:_) = strictJust $ PatchMatch $ patchmatch p
patchSetMatch (OneHash h:_) = strictJust $ PatchMatch $ hashmatch' h
patchSetMatch (OneIndex n:_) = strictJust $ IndexMatch n
patchSetMatch (Context p:_) = strictJust $ ContextMatch p
patchSetMatch (_:fs) = patchSetMatch fs

-- | @firstMatch fs@ tells whether @fs@ implies a "first match", that
-- is if we match against patches from a point in the past on, rather
-- than against all patches since the creation of the repository.
firstMatch :: [MatchFlag] -> Bool
firstMatch fs = isJust (hasLastn fs)
                 || isJust (firstMatcher fs)
                 || isJust (hasIndexRange fs)

-- | @secondMatch fs@ tells whether @fs@ implies a "second match", that
-- is if we match against patches up to a point in the past on, rather
-- than against all patches until now.
secondMatch :: [MatchFlag] -> Bool
secondMatch fs =
  isJust (secondMatcher fs) ||
  isJust (hasIndexRange fs)

checkMatchSyntax :: [MatchFlag] -> IO ()
checkMatchSyntax opts =
  case getMatchPattern opts of
    Nothing -> return ()
    Just p ->
      either
        fail
        (const $ return ())
        (parseMatch p)

getMatchPattern :: [MatchFlag] -> Maybe String
getMatchPattern [] = Nothing
getMatchPattern (OnePattern m:_) = Just m
getMatchPattern (SeveralPattern m:_) = Just m
getMatchPattern (AfterPattern m:_) = Just m
getMatchPattern (UpToPattern m:_) = Just m
getMatchPattern (_:fs) = getMatchPattern fs

tagmatch :: String -> Matcher
tagmatch r = makeMatcher ("tag-name "++r) (MatchFun tm)
  where
    tm (Sealed2 p) =
      case piTag (ident p) of
        Just t -> isJust (matchRegex (mkRegex r) t)
        Nothing -> False

patchmatch :: String -> Matcher
patchmatch r = makeMatcher ("patch-name "++r) (namematch r)

hashmatch' :: String -> Matcher
hashmatch' r = makeMatcher ("hash "++r) (hashmatch r)


-- | strictJust is a strict version of the Just constructor, used to ensure
-- that if we claim we've got a pattern match, that the pattern will
-- actually match (rathern than fail to compile properly).
strictJust :: a -> Maybe a
strictJust x = Just $! x

-- | @nonrangeMatcher@ is the criterion that is used to match against
-- patches in the interval. It is 'Just m' when the @--patch@, @--match@,
-- @--tag@ options are passed (or their plural variants).
nonrangeMatcher :: [MatchFlag] -> Maybe Matcher
nonrangeMatcher [] = Nothing
nonrangeMatcher (OnePattern m:_) = strictJust $ patternmatch m
nonrangeMatcher (OneTag t:_) = strictJust $ tagmatch t
nonrangeMatcher (OnePatch p:_) = strictJust $ patchmatch p
nonrangeMatcher (OneHash h:_) = strictJust $ hashmatch' h
nonrangeMatcher (SeveralPattern m:_) = strictJust $ patternmatch m
nonrangeMatcher (SeveralTag t:_) = strictJust $ tagmatch t
nonrangeMatcher (SeveralPatch p:_) = strictJust $ patchmatch p
nonrangeMatcher (_:fs) = nonrangeMatcher fs

-- | @firstMatcher@ returns the left bound of the matched interval.
-- This left bound is also specified when we use the singular versions
-- of @--patch@, @--match@ and @--tag@. Otherwise, @firstMatcher@
-- returns @Nothing@.
firstMatcher :: [MatchFlag] -> Maybe Matcher
firstMatcher [] = Nothing
firstMatcher (OnePattern m:_) = strictJust $ patternmatch m
firstMatcher (AfterPattern m:_) = strictJust $ patternmatch m
firstMatcher (AfterTag t:_) = strictJust $ tagmatch t
firstMatcher (OnePatch p:_) = strictJust $ patchmatch p
firstMatcher (AfterPatch p:_) = strictJust $ patchmatch p
firstMatcher (OneHash h:_) = strictJust $ hashmatch' h
firstMatcher (AfterHash h:_) = strictJust $ hashmatch' h
firstMatcher (_:fs) = firstMatcher fs

firstMatcherIsTag :: [MatchFlag] -> Bool
firstMatcherIsTag [] = False
firstMatcherIsTag (AfterTag _:_) = True
firstMatcherIsTag (_:fs) = firstMatcherIsTag fs

secondMatcher :: [MatchFlag] -> Maybe Matcher
secondMatcher [] = Nothing
secondMatcher (OnePattern m:_) = strictJust $ patternmatch m
secondMatcher (UpToPattern m:_) = strictJust $ patternmatch m
secondMatcher (OnePatch p:_) = strictJust $ patchmatch p
secondMatcher (UpToPatch p:_) = strictJust $ patchmatch p
secondMatcher (OneHash h:_) = strictJust $ hashmatch' h
secondMatcher (UpToHash h:_) = strictJust $ hashmatch' h
secondMatcher (UpToTag t:_) = strictJust $ tagmatch t
secondMatcher (_:fs) = secondMatcher fs

secondMatcherIsTag :: [MatchFlag] -> Bool
secondMatcherIsTag [] = False
secondMatcherIsTag (UpToTag _:_) = True
secondMatcherIsTag (_:fs) = secondMatcherIsTag fs

-- | Whether a patch matches the given 'MatchFlag's. This should be
-- invariant under inversion:
--
-- prop> matchAPatch (invert p) = matchAPatch p
matchAPatch :: Matchable p => [MatchFlag] -> p wX wY -> Bool
matchAPatch fs p =
  case nonrangeMatcher fs of
    Nothing -> True
    Just m -> applyMatcher m p

-- | @hasLastn fs@ return the @--last@ argument in @fs@, if any.
hasLastn :: [MatchFlag] -> Maybe Int
hasLastn [] = Nothing
hasLastn (LastN (-1):_) = error "--last requires a positive integer argument."
hasLastn (LastN n:_) = Just n
hasLastn (_:fs) = hasLastn fs

hasIndexRange :: [MatchFlag] -> Maybe (Int,Int)
hasIndexRange [] = Nothing
hasIndexRange (IndexRange x y:_) = Just (x,y)
hasIndexRange (_:fs) = hasIndexRange fs

-- | @matchFirstPatchset fs ps@ returns the part of @ps@ before its
-- first matcher, ie the one that comes first dependencywise. Hence,
-- patches in @matchFirstPatchset fs ps@ are the context for the ones
-- we want.
matchFirstPatchset :: MatchableRP p
                   => [MatchFlag] -> PatchSet p wStart wX
                   -> Maybe (SealedPatchSet p wStart)
matchFirstPatchset fs patchset
  | Just n <- hasLastn fs = Just $ patchSetDrop n patchset
  | Just (_, b) <- hasIndexRange fs = Just $ patchSetDrop b patchset
  | Just m <- firstMatcher fs =
    Just $ unseal (patchSetDrop 1) $
    if firstMatcherIsTag fs
      then getMatchingTag m patchset
      else matchAPatchset m patchset
  | otherwise = Nothing

-- | @matchSecondPatchset fs ps@ returns the part of @ps@ before its
-- second matcher, ie the one that comes last dependencywise.
matchSecondPatchset :: MatchableRP p
                    => [MatchFlag] -> PatchSet p wStart wX
                    -> Maybe (SealedPatchSet p wStart)
matchSecondPatchset fs ps
  | Just (a, _) <- hasIndexRange fs = Just $ patchSetDrop (a - 1) ps
  | Just m <- secondMatcher fs =
    Just $
    if secondMatcherIsTag fs
      then getMatchingTag m ps
      else matchAPatchset m ps
  | otherwise = Nothing

-- | Split on the second matcher. Note that this picks up the first match
-- starting from the earliest patch in a sequence, as opposed to
-- 'matchSecondPatchset' which picks up the first match starting from the
-- latest patch
splitSecondFL :: Matchable p
              => (forall wA wB . q wA wB -> Sealed2 p)
              -> [MatchFlag]
              -> FL q wX wY
              -> (FL q :> FL q) wX wY -- ^The first element is the patches before
                                      --  and including the first patch matching the
                                      --  second matcher, the second element is the
                                      --  patches after it
splitSecondFL extract fs ps =
   case hasIndexRange fs of
   Just _ -> -- selecting the last n doesn't really make sense if we're starting
             -- from the earliest patches
             error "index matches not supported by splitSecondPatchesFL"
   Nothing ->
     case secondMatcher fs of
     Nothing -> error "Couldn't splitSecondPatches"
     Just m -> splitMatchFL extract m ps

splitMatchFL
  :: Matchable p
  => (forall wA wB. q wA wB -> Sealed2 p)
  -> Matcher
  -> FL q wX wY
  -> (FL q :> FL q) wX wY
splitMatchFL _extract m NilFL = error $ "Couldn't find a patch matching " ++ show m
splitMatchFL extract m (p :>: ps)
   | unseal2 (applyMatcher m) . extract $ p = (p :>: NilFL) :> ps
   | otherwise = case splitMatchFL extract m ps of
                    before :> after -> (p :>: before) :> after

-- | Using a special exception type here means that is is treated as
-- regular failure, and not as a bug in Darcs.
data MatchFailure = MatchFailure String
  deriving Typeable

instance Exception MatchFailure

instance Show MatchFailure where
  show (MatchFailure m) =
    "Couldn't find a patch matching " ++ m

-- | @matchAPatchset m ps@ returns a prefix of @ps@
-- ending in a patch matching @m@, and calls 'error' if there is none.
matchAPatchset
  :: MatchableRP p
  => Matcher
  -> PatchSet p wStart wX
  -> SealedPatchSet p wStart
matchAPatchset m (PatchSet NilRL NilRL) =
  throw $ MatchFailure $ show m
matchAPatchset m (PatchSet (ts :<: Tagged ps t _) NilRL) =
  matchAPatchset m (PatchSet ts (ps :<: t))
matchAPatchset m (PatchSet ts (ps :<: p))
  | applyMatcher m p = seal (PatchSet ts (ps :<: p))
  | otherwise = matchAPatchset m (PatchSet ts ps)

splitOnMatchingTag :: MatchableRP p
                   => Matcher
                   -> PatchSet p wStart wX
                   -> PatchSet p wStart wX
splitOnMatchingTag _ s@(PatchSet NilRL NilRL) = s
splitOnMatchingTag m s@(PatchSet (ts :<: Tagged ps t _) NilRL)
    | applyMatcher m t = s
    | otherwise = splitOnMatchingTag m (PatchSet ts (ps:<:t))
splitOnMatchingTag m (PatchSet ts (ps:<:p))
    -- found a non-clean tag, need to commute out the things that it doesn't depend on
    | applyMatcher m p =
        case splitOnTag (info p) (PatchSet ts (ps:<:p)) of
          Just x -> x
          Nothing -> error "splitOnTag failed"
    | otherwise =
        case splitOnMatchingTag m (PatchSet ts ps) of
          PatchSet ts' ps' -> PatchSet ts' (ps' :<: p)

-- | @getMatchingTag m ps@, where @m@ is a 'Matcher' which matches tags
-- returns a 'SealedPatchSet' containing all patches in the last tag which
-- matches @m@. Last tag means the most recent tag in repository order,
-- i.e. the last one you'd see if you ran darcs log -t @m@. Calls
-- 'error' if there is no matching tag.
getMatchingTag :: MatchableRP p
               => Matcher
               -> PatchSet p wStart wX
               -> SealedPatchSet p wStart
getMatchingTag m ps =
  case splitOnMatchingTag m ps of
    PatchSet NilRL _ -> throw $ userError $ "Couldn't find a tag matching " ++ show m
    PatchSet ps' _ -> seal $ PatchSet ps' NilRL

-- | Return the patches in a 'PatchSet' up to the given 'PatchSetMatch'.
matchOnePatchset
  :: MatchableRP p
  => PatchSet p Origin wR
  -> PatchSetMatch
  -> IO (SealedPatchSet p Origin)
matchOnePatchset ps (IndexMatch   n   ) = return $ patchSetDrop (n - 1) ps
matchOnePatchset ps (PatchMatch   m   ) = return $ matchAPatchset m ps
matchOnePatchset ps (TagMatch     m   ) = return $ getMatchingTag m ps
matchOnePatchset ps (ContextMatch path) = readContextFile ps (toFilePath path)

-- | Rollback (i.e. apply the inverse) of what remains of a 'PatchSet' after we
-- extract a 'PatchSetMatch'. This is the counterpart of 'getOnePatchset' and
-- is used to create a matching state. In particular, if the match is --index=n
-- then rollback the last (n-1) patches; if the match is --tag, then rollback
-- patches that are not depended on by the tag; otherwise rollback patches that
-- follow the latest matching patch.
rollbackToPatchSetMatch :: ( ApplyMonad (ApplyState p) m
                           , MatchableRP p, ApplyState p ~ Tree
                           )
                        => PatchSetMatch
                        -> PatchSet p Origin wX
                        -> m ()
rollbackToPatchSetMatch psm repo =
  case psm of
    IndexMatch n -> applyNInv (n-1) repo
    TagMatch m ->
      case splitOnMatchingTag m repo of
        PatchSet NilRL _ -> throw $ MatchFailure $ show m
        PatchSet _ extras -> unapply extras
    PatchMatch m -> applyInvToMatcher m repo
    ContextMatch _ -> error "rollbackToPatchSetMatch: unexpected context match"

-- | @applyInvToMatcher@ m ps applies the inverse of the patches in @ps@,
-- starting at the end, until we hit a patch that matches the 'Matcher' @m@.
applyInvToMatcher :: (MatchableRP p, ApplyMonad (ApplyState p) m)
                  => Matcher
                  -> PatchSet p Origin wX
                  -> m ()
applyInvToMatcher m (PatchSet NilRL NilRL) =
  throw $ MatchFailure $ show m
applyInvToMatcher m (PatchSet (ts :<: Tagged ps t _) NilRL) =
  applyInvToMatcher m (PatchSet ts (ps :<: t))
applyInvToMatcher m (PatchSet xs (ps :<: p))
  | applyMatcher m p = return ()
  | otherwise = unapply p >> applyInvToMatcher m (PatchSet xs ps)

-- | @applyNInv@ n ps applies the inverse of the last @n@ patches of @ps@.
applyNInv :: (MatchableRP p, ApplyMonad (ApplyState p) m)
          => Int -> PatchSet p Origin wX -> m ()
applyNInv n _ | n <= 0 = return ()
applyNInv _ (PatchSet NilRL NilRL) = throw $ userError "Index out of range"
applyNInv n (PatchSet (ts :<: Tagged ps t _) NilRL) =
  applyNInv n (PatchSet ts (ps :<: t))
applyNInv n (PatchSet xs (ps :<: p)) =
  unapply p >> applyNInv (n - 1) (PatchSet xs ps)

-- | matchingHead returns the repository up to some tag. The tag t is the last
-- tag such that there is a patch after t that is matched by the user's query.
matchingHead :: forall p wR. MatchableRP p
             => [MatchFlag] -> PatchSet p Origin wR
             -> (PatchSet p :> FL (PatchInfoAnd p)) Origin wR
matchingHead matchFlags set =
    case mh set of
        (start :> patches) -> start :> reverseRL patches
  where
    mh :: forall wX . PatchSet p Origin wX
       -> (PatchSet p :> RL (PatchInfoAnd p)) Origin wX
    mh s@(PatchSet _ x)
        | or (mapRL (matchAPatch matchFlags) x) = contextPatches s
    mh (PatchSet (ts :<: Tagged ps t _) x) =
        case mh (PatchSet ts (ps :<: t)) of
            (start :> patches) -> start :> patches +<+ x
    mh ps = ps :> NilRL
