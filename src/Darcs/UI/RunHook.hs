--  Copyright (C) 2002-2005 David Roundy
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

module Darcs.UI.RunHook
    ( runPosthook
    , runPrehook
    )
where

import Darcs.Prelude

import System.Exit ( ExitCode(..) )
import System.Process ( system )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( when )

import Darcs.UI.Options.All ( HookConfig(..), Verbosity(..) )

import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Prompt ( promptYorn )

runPosthook :: HookConfig -> Verbosity -> AbsolutePath -> IO ExitCode
runPosthook (HookConfig mPostHook askPostHook) verb repodir
    = do ph <- getHook "Posthook" mPostHook askPostHook
         withCurrentDirectory repodir $ runHook verb "Posthook" ph

runPrehook :: HookConfig -> Verbosity -> AbsolutePath -> IO ExitCode
runPrehook (HookConfig mPreHookCmd askPreHook) verb repodir =
    do ph <- getHook "Prehook" mPreHookCmd askPreHook
       withCurrentDirectory repodir $ runHook verb "Prehook" ph

getHook :: String -> Maybe String -> Bool -> IO (Maybe String)
getHook name mPostHookCmd askHook =
 case mPostHookCmd of
   Nothing -> return Nothing
   Just command ->
     if askHook
      then do yorn <-
                promptYorn
                  ("The following command is set to execute:\n"++command++
                   "\nExecute this command now?")
              if yorn
                then return $ Just command
                else putStrLn (name ++ " cancelled...") >> return Nothing
      else return $ Just command

runHook :: Verbosity -> String -> Maybe String -> IO ExitCode
runHook _ _ Nothing = return ExitSuccess
runHook verb cname (Just command) =
    do ec <- system command
       when (verb /= Quiet) $
         if ec == ExitSuccess
         then putStrLn $ cname++" ran successfully."
         else hPutStrLn stderr $ cname++" failed!"
       return ec

