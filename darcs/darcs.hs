-- Copyright (C) 2002-2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Copyright   : 2002-2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Main ( main ) where

import Darcs.Prelude

import Control.Exception ( handle, ErrorCall )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs )
import System.IO ( hPutStr, stderr )

import Darcs.UI.RunCommand ( runTheCommand )
import Darcs.UI.Commands.Help ( helpCmd, listAvailableCommands, printVersion,
                             commandControlList )
import Darcs.Util.AtExit ( withAtexit, atexit )
import Darcs.Repository( reportBadSources )
import Darcs.Util.SignalHandler ( withSignalsHandled )

import Darcs.UI.External ( setDarcsEncodings )
import Darcs.Util.Exec ( ExecException(..) )
import Darcs.Util.Path ( getCurrentDirectory )
import Version ( version, context, weakhash )

execExceptionHandler :: ExecException -> IO a
execExceptionHandler (ExecException cmd args redirects reason) = do
    hPutStr stderr . unlines $
        [ "Failed to execute external command: " ++ unwords (cmd:args)
        , "Lowlevel error: " ++ reason
        , "Redirects: " ++ show redirects
        ]
    exitWith $ ExitFailure 3

errorExceptionHandler :: ErrorCall -> IO a
errorExceptionHandler e = do
    hPutStr stderr . unlines $
        [ "This is a bug! Please report it at http://bugs.darcs.net " ++
          "or via email to bugs@darcs.net:"
        , show e
        ]
    exitWith $ ExitFailure 4

main :: IO ()
main = handleErrors . handleExecFail . withSignalsHandled . withAtexit $ do
    atexit reportBadSources
    setDarcsEncodings
    argv <- getArgs
    here <- getCurrentDirectory
    let runHelpCmd = helpCmd (here, here) [] []
    -- Explicitly handle no-args and special "help" arguments.
    case argv of
        [] -> printVersion >> runHelpCmd
        ["-h"] -> runHelpCmd
        ["--help"] -> runHelpCmd
        ["--commands"] -> listAvailableCommands
        ["-v"] -> putStrLn version
        ["-V"] -> putStrLn version
        ["--version"] -> putStrLn version
        ["--exact-version"] -> printExactVersion
        cmd:args -> runTheCommand commandControlList cmd args
  where
    handleErrors = handle errorExceptionHandler
    handleExecFail = handle execExceptionHandler
    printExactVersion =  do
        putStrLn $ "Weak Hash: " ++ weakhash
        putStrLn context
