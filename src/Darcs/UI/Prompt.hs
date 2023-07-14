-- | A more high-level API for what "Darcs.Util.Prompt" offers
module Darcs.UI.Prompt
    ( PromptChoice(..)
    , PromptConfig(..)
    , runPrompt
    , promptYornorq
    ) where

import Darcs.Prelude
import Data.List ( find, intercalate )
import System.Exit ( exitSuccess )
import qualified Darcs.Util.Prompt as P

data PromptChoice a = PromptChoice
  { pcKey :: Char
  , pcWhen :: Bool
  , pcAction :: IO a
  , pcHelp :: String
  }

data PromptConfig a = PromptConfig
  { pPrompt :: String               -- what to ask the user
  , pVerb :: String                 -- command (what we are doing)
  , pChoices :: [[PromptChoice a]]  -- list of choice groups
  , pDefault :: Maybe Char          -- default choice, capitalized
  }

-- | Generate the help string from a verb and list of choice groups
helpFor :: String -> [[PromptChoice a]] -> Maybe Char -> String
helpFor jn choices def =
  unlines $
    [ "How to use " ++ jn ++ ":" ] ++
    intercalate [""] (map (map help . filter pcWhen) choices) ++
    [ ""
    , "?: show this help"
    ] ++ defaultHelp
  where
    help i = pcKey i : (": " ++ pcHelp i)
    defaultHelp =
      case def of
        Nothing -> []
        Just _ ->
          [ ""
          , "<Space>: accept the current default (which is capitalized)"
          ]

lookupAction :: Char -> [PromptChoice a] -> Maybe (IO a)
lookupAction key choices = pcAction <$> find ((==key).pcKey) choices

runPrompt :: PromptConfig a -> IO a
runPrompt pcfg@PromptConfig{..} = do
  let choices = filter pcWhen $ concat pChoices
  key <-
    P.promptChar $
      P.PromptConfig pPrompt (map pcKey choices) [] Nothing "?h"
  case lookupAction key choices of
    Just action -> action
    Nothing -> putStrLn (helpFor pVerb pChoices pDefault) >> runPrompt pcfg

-- | Prompt the user for a yes or no or cancel
promptYornorq :: String -> String -> IO a -> IO a -> IO a
promptYornorq prompt verb yes no =
    runPrompt (PromptConfig prompt verb choices Nothing)
  where
    quit = putStrLn "Command cancelled." >> exitSuccess
    choices =
      [ [ PromptChoice 'y' True yes ("yes, do " ++ verb)
        , PromptChoice 'n' True no ("no, don't " ++ verb)
        , PromptChoice 'q' True quit "quit (cancel command)"
        ]
      ]
