-- Copyright (C) 2003 David Roundy
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

module Darcs.UI.PrintPatch
    ( contextualPrintPatchWithPager
    , printContent
    , printContentWithPager
    , printFriendly
    , printSummary
    , showFriendly
    , showWithSummary
    ) where

import Darcs.Prelude

import Darcs.Patch ( description, content, summary )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.ApplyMonad ( ApplyMonadTrans )
import Darcs.Patch.Show
    ( ShowContextPatch
    , ShowPatch
    , ShowPatchFor(ForDisplay)
    , showPatchWithContext
    )
import Darcs.UI.External ( viewDocWith )
import Darcs.UI.Options.All ( Verbosity(..), WithSummary(..) )
import Darcs.Util.Printer ( Doc, prefix, putDocLnWith, ($$) )
import Darcs.Util.Printer.Color ( fancyPrinters )

-- | @'printFriendly' opts patch@ prints @patch@ in accordance with the flags
-- in opts, ie, whether @--verbose@ or @--summary@ were passed at the
-- command-line.
printFriendly :: ShowPatch p => Verbosity -> WithSummary -> p wX wY -> IO ()
printFriendly v s = putDocLnWith fancyPrinters . showFriendly v s

-- | @'showFriendly' flags patch@ returns a 'Doc' representing the right
-- way to show @patch@ given the list @flags@ of flags darcs was invoked with.
showFriendly :: ShowPatch p => Verbosity -> WithSummary -> p wX wY -> Doc
showFriendly Verbose _          = showWithContents
showFriendly _       YesSummary = showWithSummary
showFriendly _       NoSummary  = description

showWithSummary :: ShowPatch p => p wX wY -> Doc
showWithSummary p = description p $$ prefix "    " (summary p)

showWithContents :: ShowPatch p => p wX wY -> Doc
showWithContents p = description p $$ prefix "    " (content p)

printSummary :: ShowPatch p => p wX wY -> IO ()
printSummary = putDocLnWith fancyPrinters . prefix "    " . summary

printContent :: ShowPatch p => p wX wY -> IO ()
printContent = putDocLnWith fancyPrinters . prefix "    " . content

printContentWithPager :: ShowPatch p => p wX wY -> IO ()
printContentWithPager = viewDocWith fancyPrinters . prefix "    " . content

-- | Print a patch, together with its context, on standard output, using a
-- pager.
contextualPrintPatchWithPager
  :: (ApplyMonadTrans (ApplyState p) IO, ShowContextPatch p)
  => ApplyState p IO
  -> p wX wY
  -> IO ()
contextualPrintPatchWithPager s p = do
    showPatchWithContext ForDisplay s p >>= viewDocWith fancyPrinters
