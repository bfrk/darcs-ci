module Darcs.UI.Defaults ( applyDefaults ) where

import Darcs.Prelude

import Control.Monad.Writer
import Data.Char ( isLetter, isSpace )
import Data.Either ( partitionEithers )
import Data.Functor.Compose ( Compose(..) )
import Data.List ( nub )
import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import System.Console.GetOpt
import Text.Regex.Applicative
    ( (<|>)
    , match, many, some, sym
    , psym, anySym, string )

import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( DarcsOptDescr, OptMsg(..), withDashes )

import Darcs.UI.Commands
    ( DarcsCommand
    , commandAlloptions
    , commandCheckOptions
    , commandDefaults
    , commandName
    , extractAllCommands
    )
import Darcs.UI.TheCommands ( commandControlList )
import Darcs.Util.Path ( AbsolutePath )

-- | Apply defaults from all sources to a list of 'DarcsFlag's (e.g. from the
-- command line), given the command (and possibly super command) name, and a
-- list of all options for the command.
-- 
-- Sources for defaults are
-- 
--  * the builtin (hard-coded) defaults,
-- 
--  * the defaults file in the user's configuration, and
-- 
--  * the defaults file in the current repository.
-- 
-- Note that the pseudo command @ALL@ is allowed in defaults files to specify
-- that an option should be the default for all commands to which it applies.
-- 
-- The order of precedence for conflicting options (i.e. those belonging to
-- same group of mutually exclusive options) is from less specific to more
-- specific. In other words, options from the command line override all
-- defaults, per-repo defaults override per-user defaults, which in turn
-- override the built-in defaults. Inside the options from a defaults file,
-- options for the given command override options for the @ALL@ pseudo command.
-- 
-- Conflicting options at the same level of precedence are not allowed.
--
-- Errors encountered during processing of command line or defaults flags
-- are formatted and added as (separate) strings to the list of error messages
-- that are returned together with the resulting flag list.
applyDefaults
  :: Maybe String -- ^ maybe name of super command
  -> DarcsCommand -- ^ the darcs command
  -> AbsolutePath -- ^ the original working directory, i.e.
                  --   the one from which darcs was invoked
  -> [String]     -- ^ lines from user defaults
  -> [String]     -- ^ lines from repo defaults
  -> [DarcsFlag]  -- ^ flags from command line
  -> ([DarcsFlag], ([String], [String])) -- new flags, warnings, errors
applyDefaults msuper cmd cwd user repo flags =
  splitMessages $ runWriter $ do
    cl_flags  <- runChecks "Command line" check_opts flags
    user_defs <- get_flags "User defaults" user
    repo_defs <- get_flags "Repo defaults" repo
    return $ cl_flags ++ repo_defs ++ user_defs ++ builtin_defs
  where
    cmd_name = mkCmdName msuper (commandName cmd)
    builtin_defs = commandDefaults cmd
    check_opts = commandCheckOptions cmd
    opts = uncurry (++) $ commandAlloptions cmd
    get_flags source = parseDefaults source cwd cmd_name opts check_opts
    splitMessages (r,ms) = (r,partitionOptMsgs ms)

-- | Name of a normal command, or name of super and sub command.
data CmdName = NormalCmd String | SuperCmd String String

-- | Make a 'CmdName' from a possible super command name and a sub command name.
mkCmdName :: Maybe String -> String -> CmdName
mkCmdName Nothing cmd = NormalCmd cmd
mkCmdName (Just super) sub = SuperCmd super sub

-- | Turn a 'CmdName' into a 'String'. For a 'SuperCmd' concatenate with a space in between.
showCmdName :: CmdName -> String
showCmdName (SuperCmd super sub) = unwords [super,sub]
showCmdName (NormalCmd name) = name

runChecks :: String -> ([DarcsFlag] -> [OptMsg]) -> [DarcsFlag] -> Writer [OptMsg] [DarcsFlag]
runChecks source check fs = do
  tell $ map (mapOptMsg ((source++": ")++)) $ check fs
  return fs

mapOptMsg :: (String -> String) -> OptMsg -> OptMsg
mapOptMsg f (OptWarning s) = OptWarning (f s)
mapOptMsg f (OptError s) = OptError (f s)

partitionOptMsgs :: [OptMsg] -> ([String], [String])
partitionOptMsgs = partitionEithers . map toEither where
  toEither (OptWarning s) = Left s
  toEither (OptError s) = Right s

-- | Parse a list of lines from a defaults file, returning a list of 'DarcsFlag',
-- given the current working directory, the command name, and a list of 'DarcsOption'
-- for the command.
--
-- In the result, defaults for the given command come first, then come defaults
-- for @ALL@ commands.
--
-- We check that matching options actually exist.
--
--  * lines matching the command name: the option must exist in the command's
--    option map.
--
--  * lines matching @ALL@: there must be at least *some* darcs command with
--    that option.
--
parseDefaults :: String
              -> AbsolutePath
              -> CmdName
              -> [DarcsOptDescr DarcsFlag]
              -> ([DarcsFlag] -> [OptMsg])
              -> [String]
              -> Writer [OptMsg] [DarcsFlag]
parseDefaults source cwd cmd opts check_opts def_lines = do
    cmd_flags <- flags_for (M.keys opt_map) cmd_defs >>=
      runChecks (source++" for command '"++showCmdName cmd++"'") check_opts
    all_flags <- flags_for allOptionSwitches all_defs >>=
      runChecks (source++" for ALL commands") check_opts
    return $ cmd_flags ++ all_flags
  where
    opt_map = optionMap opts
    cmd_defs = parseDefaultsLines cmd def_lines
    all_defs = parseDefaultsLines (NormalCmd "ALL") def_lines
    to_flag all_switches (switch,arg) =
      if switch `notElem` all_switches then do
        tell [ OptWarning $ source++": command '"++showCmdName cmd
             ++"' has no option '"++switch++"'."]
        return Nothing
      else
        mapErrors ((OptWarning $ source++" for command '"++showCmdName cmd++"':"):)
          $ defaultToFlag cwd opt_map (switch,arg)
    -- the catMaybes filters out options that are not defined
    -- for this command
    flags_for all_switches = fmap catMaybes . mapM (to_flag all_switches)
    mapErrors f = mapWriter (\(r, es) -> (r, if null es then [] else f es))

-- | Result of parsing a defaults line: switch and argument(s).
type Default = (String, String)

-- | Extract 'Default's from lines of a defaults file that match the given 'CmdName'.
-- 
-- The syntax is
--
-- @
--  supercmd subcmd [--]switch [args...]
-- @
--
-- for (super) commands with a sub command, and
--
-- @
--  cmd default [--]default [args...]
-- @
--
-- for normal commands (including the @ALL@ pseudo command).
parseDefaultsLines :: CmdName -> [String] -> [Default]
parseDefaultsLines cmd = catMaybes . map matchLine
  where
    matchLine = match $ (,) <$> (match_cmd cmd *> spaces *> option) <*> rest
    match_cmd (NormalCmd name) = string name
    match_cmd (SuperCmd super sub) = string super *> spaces *> string sub
    option = short <|> long
    short = (\c1 c2 -> [c1,c2]) <$> sym '-' <*> psym isLetter
    long = (++) <$> opt_dashes <*> word
    opt_dashes = string "--" <|> pure "--"
    word = (:) <$> psym isLetter <*> many (psym (not.isSpace))
    spaces = some $ psym isSpace
    rest = spaces *> many anySym <|> pure ""

-- | Search an option list for a switch. If found, apply the flag constructor
-- from the option to the arg, if any. The first parameter is the current working
-- directory, which, depending on the option type, may be needed to create a flag
-- from an argument.
-- 
-- Fails if (default has argument /= corresponding option has argument).
defaultToFlag :: AbsolutePath
              -> OptionMap
              -> Default
              -> Writer [OptMsg] (Maybe DarcsFlag)
defaultToFlag cwd opts (switch, arg) = case M.lookup switch opts of
    -- This case is not impossible! A default flag defined for ALL commands
    -- is not necessarily defined for the concrete command in question.
    Nothing -> return Nothing
    Just opt -> flag_from $ getArgDescr $ getCompose opt
  where
    getArgDescr (Option _ _ a _) = a
    flag_from (NoArg mkFlag) = do
      if not (null arg) then do
        tell
          [ OptWarning $
            "'"++switch++"' takes no argument, but '"++arg++"' argument given." ]
        return Nothing
      else
        return $ Just $ mkFlag cwd
    flag_from (OptArg mkFlag _) =
      return $ Just $ mkFlag (if null arg then Nothing else Just arg) cwd
    flag_from (ReqArg mkFlag _) = do
      if null arg then do
        tell
          [ OptError $
            "'"++switch++"' requires an argument, but no argument given." ]
        return Nothing
      else
        return $ Just $ mkFlag arg cwd

-- | Get all the flag names from an options
optionSwitches :: DarcsOptDescr DarcsFlag -> [String]
optionSwitches (Compose (Option short long _ _)) = withDashes short long

-- | A finite map from flag names to 'DarcsOptDescr's.
type OptionMap = M.Map String (DarcsOptDescr DarcsFlag)

-- | Build an 'OptionMap' from a list of 'DarcsOption's.
optionMap :: [DarcsOptDescr DarcsFlag] -> OptionMap
optionMap = M.fromList . concatMap sel where
  add_option opt switch = (switch, opt)
  sel o = map (add_option o) (optionSwitches o)

-- | List of option switches of all commands (except help but that has no options).
allOptionSwitches :: [String]
allOptionSwitches = nub $ concatMap optionSwitches $
  concatMap (uncurry (++) . commandAlloptions) $
            extractAllCommands commandControlList
