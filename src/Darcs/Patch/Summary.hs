module Darcs.Patch.Summary
    ( plainSummary
    , plainSummaryFL
    , plainSummaryPrim
    , plainSummaryPrims
    , xmlSummary
    , Summary(..)
    , ConflictState(..)
    , IsConflictedPrim(..)
    , listConflictedFiles
    ) where

import Darcs.Prelude

import Data.List.Ordered ( nubSort )
import Data.Maybe ( catMaybes )
import qualified Text.XML.Light as XML

import Darcs.Patch.Format ( FileNameFormat(FileNameFormatDisplay) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Prim ( PrimDetails(..) )
import Darcs.Patch.Show ( formatFileName )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )
import Darcs.Patch.Witnesses.Show

import Darcs.Util.Path ( AnchoredPath, anchorPath )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , (<+>)
    , empty
    , text
    , vcat
    )

-- | This type tags a patch with a 'ConflictState' and also hides the context
-- witnesses (as in 'Sealed2'), so we can put them in a list.
data IsConflictedPrim prim where
    IsC :: !ConflictState -> !(prim wX wY) -> IsConflictedPrim prim
data ConflictState = Okay | Conflicted | Duplicated deriving ( Eq, Ord, Show, Read)

class Summary p where
    conflictedEffect :: p wX wY -> [IsConflictedPrim (PrimOf p)]

instance Summary p => Summary (FL p) where
    conflictedEffect = concat . mapFL conflictedEffect

instance Show2 prim => Show (IsConflictedPrim prim) where
    showsPrec d (IsC cs prim) =
        showParen (d > appPrec) $
            showString "IsC " . showsPrec (appPrec + 1) cs .
            showString " " . showsPrec2 (appPrec + 1) prim

listConflictedFiles
  :: (Summary p, PatchInspect (PrimOf p)) => p wX wY -> [AnchoredPath]
listConflictedFiles =
    nubSort . concat . catMaybes . map conflictedFiles . conflictedEffect
  where
    conflictedFiles (IsC Conflicted p) = Just (listTouchedFiles p)
    conflictedFiles _ = Nothing

plainSummaryPrim :: PrimDetails prim => prim wX wY -> Doc
plainSummaryPrim = vcat . map (summChunkToLine False) . genSummary . (:[]) . IsC Okay

plainSummaryPrims :: PrimDetails prim => Bool -> FL prim wX wY -> Doc
plainSummaryPrims machineReadable =
 vcat . map (summChunkToLine machineReadable) . genSummary . mapFL (IsC Okay)

plainSummary :: (Summary e, PrimDetails (PrimOf e)) => e wX wY -> Doc
plainSummary = vcat . map (summChunkToLine False) . genSummary . conflictedEffect

plainSummaryFL :: (Summary e, PrimDetails (PrimOf e)) => FL e wX wY -> Doc
plainSummaryFL = vcat . map (summChunkToLine False) . genSummary . concat . mapFL conflictedEffect

xmlSummary :: (Summary p, PrimDetails (PrimOf p)) => p wX wY -> XML.Element
xmlSummary p = XML.unode "summary" (catMaybes . map summChunkToXML . genSummary . conflictedEffect $ p)

-- | High-level representation of a piece of patch summary
data SummChunk = SummChunk SummDetail ConflictState
   deriving (Ord, Eq)

genSummary :: forall p . PrimDetails p => [IsConflictedPrim p] -> [SummChunk]
genSummary p
    = combine $ concatMap s2 p
    where s2 :: IsConflictedPrim p -> [SummChunk]
          s2 (IsC c x) = map (`SummChunk` c) $ summarizePrim x
          combine (x1@(SummChunk d1 c1) : x2@(SummChunk d2 c2) : ss)
              = case combineDetail d1 d2 of
                  Nothing -> x1 : combine (x2:ss)
                  Just d3 -> combine $ SummChunk d3 (combineConflictStates c1 c2) : ss
          combine (x:ss) = x  : combine ss
          combine [] = []
          --
          combineDetail (SummFile o1 f1 r1 a1 x1) (SummFile o2 f2 r2 a2 x2) | f1 == f2 =
            do o3 <- combineOp o1 o2
               return $ SummFile o3 f1 (r1 + r2) (a1 + a2) (x1 + x2)
          combineDetail _ _ = Nothing
          --
          combineConflictStates Conflicted _ = Conflicted
          combineConflictStates _ Conflicted = Conflicted
          combineConflictStates Duplicated _ = Duplicated
          combineConflictStates _ Duplicated = Duplicated
          combineConflictStates Okay Okay = Okay
          -- Don't combine AddFile and RmFile: (maybe an old revision of) darcs
          -- allows a single patch to add and remove the same file, see issue 185
          combineOp SummAdd SummRm  = Nothing
          combineOp SummRm  SummAdd = Nothing
          combineOp SummAdd _ = Just SummAdd
          combineOp _ SummAdd = Just SummAdd
          combineOp SummRm  _ = Just SummRm
          combineOp _ SummRm  = Just SummRm
          combineOp SummMod SummMod = Just SummMod

summChunkToXML :: SummChunk -> Maybe XML.Element
summChunkToXML (SummChunk detail c) =
  case detail of
    SummRmDir f -> Just $ xconf c "remove_directory" [] [cdata (xfn f)]
    SummAddDir f -> Just $ xconf c "add_directory" [] [cdata (xfn f)]
    SummFile SummRm f _ _ _ -> Just $ xconf c "remove_file" [] [cdata (xfn f)]
    SummFile SummAdd f _ _ _ -> Just $ xconf c "add_file" [] [cdata (xfn f)]
    SummFile SummMod f r a x ->
      Just $ xconf c "modify_file" [] ([cdata (xfn f)] <> xrm r <> xad a <> xrp x)
    SummMv f1 f2 ->
      Just $
        xconf c "move"
          [XML.Attr (XML.unqual "from") (xfn f1), XML.Attr (XML.unqual "to") (xfn f2)] []
    SummNone -> Nothing
  where
    xconf :: ConflictState -> String -> [XML.Attr] -> [XML.Content] -> XML.Element
    xconf Okay t as cs = XML.unode t (as, cs)
    xconf Conflicted t as cs =
      XML.unode t (XML.Attr (XML.unqual "conflict") "true":as, cs)
    xconf Duplicated t as cs =
      XML.unode t (XML.Attr (XML.unqual "suplicate") "true":as, cs)
    xfn = anchorPath ""
    cdata s = XML.Text (XML.blank_cdata {XML.cdData = s})
    xad 0 = []
    xad a = [XML.Elem $ XML.unode "added_lines" (XML.Attr (XML.unqual "num") (show a))]
    xrm 0 = []
    xrm a = [XML.Elem $ XML.unode "removed_lines" (XML.Attr (XML.unqual "num") (show a))]
    xrp 0 = []
    xrp a = [XML.Elem $ XML.unode "replaced_tokens" (XML.Attr (XML.unqual "num") (show a))]

summChunkToLine :: Bool -> SummChunk -> Doc
summChunkToLine machineReadable (SummChunk detail c) =
  case detail of
   SummRmDir f   -> lconf c "R" $ formatFileName FileNameFormatDisplay f <> text "/"
   SummAddDir f  -> lconf c "A" $ formatFileName FileNameFormatDisplay f <> text "/"
   SummFile SummRm  f r a x
     | machineReadable -> lconf c "R" $ formatFileName FileNameFormatDisplay f
     | otherwise       -> lconf c "R" $ formatFileName FileNameFormatDisplay f <+> rm r <+> ad a <+> rp x
   SummFile SummAdd f r a x
     | machineReadable -> lconf c "A" $ formatFileName FileNameFormatDisplay f
     | otherwise       -> lconf c "A" $ formatFileName FileNameFormatDisplay f <+> rm r <+> ad a <+> rp x
   SummFile SummMod f r a x
     | machineReadable -> lconf c "M" $ formatFileName FileNameFormatDisplay f
     | otherwise       -> lconf c "M" $ formatFileName FileNameFormatDisplay f <+> rm r <+> ad a <+> rp x
   SummMv f1 f2
     | machineReadable -> text "F " <> formatFileName FileNameFormatDisplay f1
                       $$ text "T " <> formatFileName FileNameFormatDisplay f2
     | otherwise       -> text " "    <> formatFileName FileNameFormatDisplay f1
                       <> text " -> " <> formatFileName FileNameFormatDisplay f2
   SummNone -> case c of
               Okay -> empty
               _    -> lconf c ""  empty
  where
   lconf Okay       t x = text t <+> x
   lconf Conflicted t x = text (t ++ "!") <+> x
   lconf Duplicated t x
     | machineReadable = text t <+> x
     | otherwise       = text t <+> x <+> text "duplicate"
   --
   ad 0 = empty
   ad a = text "+" <> text (show a)
   rm 0 = empty
   rm a = text "-" <> text (show a)
   rp 0 = empty
   rp a = text "r" <> text (show a)
