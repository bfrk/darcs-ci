{-# LANGUAGE OverloadedStrings #-}
module Darcs.Patch.Prim.V1.FilePatch
    ( FilePatchType(..)
    , With(..)
    , ApplyFilePatchError
    , showApplyFilePatchError
    , applyFilePatch
    ) where

import Darcs.Prelude

import Control.Monad ( guard )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Commute ( Commute(..), invertCommute, trivialCommute )
import Darcs.Patch.FileHunk ( FileHunk(..), showFileHunk )
import Darcs.Patch.Format ( FileNameFormat(FileNameFormatDisplay) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Prim.Class ( PrimDetails(..), PrimRead(..), PrimShow(..) )
import Darcs.Patch.Read ( readFileName )
import Darcs.Patch.Show ( formatFileName )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )
import Darcs.Patch.TokenReplace ( tryTokReplace )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Util.ByteString ( fromHex2PS, fromPS2Hex, unlinesPS )
import Darcs.Util.Parser
    ( Parser
    , char
    , choice
    , int
    , lexWord
    , linesStartingWith
    , option
    , skipSpace
    , string
    )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer
    ( Doc
    , blueText
    , invisiblePS
    , invisibleText
    , packedString
    , prefix
    , text
    , vcat
    , ($$)
    , (<+>)
    )


-- | Annotate a patch type @p@ with some (unwitnessed) data of type @a@
data With a p wX wY = With a (p wX wY)

-- | Change of file content
data FilePatchType wX wY
    = Hunk !Int [B.ByteString] [B.ByteString]
    | TokReplace !String !B.ByteString !B.ByteString
    | Binary B.ByteString B.ByteString
    deriving (Eq,Ord,Show)

type role FilePatchType nominal nominal

instance Eq2 FilePatchType where
    unsafeCompare a b = a == unsafeCoerceP b

instance Invert FilePatchType where
    invert (Hunk line old new) = Hunk line new old
    invert (TokReplace t o n) = TokReplace t n o
    invert (Binary o n) = Binary n o

type FileContents = B.ByteString

instance Invert p => Invert (With a p) where
  invert (With x p) = With x (invert p)

data ApplyFilePatchError where
  ApplyFilePatchError :: FilePatchType wX wY -> Maybe String -> ApplyFilePatchError

showApplyFilePatchError :: AnchoredPath -> ApplyFilePatchError -> Doc
showApplyFilePatchError path (ApplyFilePatchError p mmsg) =
  text "### Error applying:" $$
  prefix "###" (showPrim FileNameFormatDisplay (With path p)) $$
  case mmsg of
    Nothing -> mempty
    Just msg -> text "### Reason: " <> text msg

-- | We don't define an @instance 'Apply' ('With' 'AnchoredPath' 'FilePatchType')@
-- because even if we define @'ApplyState' 'FilePatchType' = 'B.ByteString'@,
-- the 'apply' method has the wrong type, i.e. @p wX wY -> m ()@ instead of the
-- @p wX wY -> B.ByteString -> m B.ByteString@ we need for 'mModifyFilePS'.
applyFilePatch
  :: FilePatchType wX wY -> FileContents -> Either ApplyFilePatchError FileContents
applyFilePatch p@(Hunk l o n) fc =
  case applyHunkLines (l,o,n) fc of
    Right fc' -> return fc'
    Left msg -> Left $ ApplyFilePatchError p (Just msg)
applyFilePatch p@(TokReplace t o n) fc =
  case tryTokReplace t o n fc of
    Nothing -> Left $ ApplyFilePatchError p Nothing
    Just fc' -> return fc'
applyFilePatch p@(Binary o n) fc
  | o == fc = return n
  | otherwise = Left $ ApplyFilePatchError p Nothing

{- The way darcs handles newlines is not easy to understand.

Everything seems pretty logical and conventional as long as files end in a
newline. In this case, the lines in a hunk can be regarded as newline
terminated, too. However, this view breaks down if we consider files that
are not newline terminated.

Here is a different view that covers the general case and explains,
conceptually, the algorithm below.

* Ever line (in a hunk or file) is regarded as being /preceded/ by a newline
  character.

* Every file starts out containing a single newline character, that is, a
  single empty line. A first empty line at the start of a file (if present)
  is /invisible/.

* When lines are appended to a file by a hunk, they are inserted /before/ a
  final empty line, if there is one. This results in a file that remains
  being terminated by a newline.

* In particular, when we start with an empty file and add a line, we push
  the invisible newline back, making it visible, and the newline that
  initiates our new content becomes invisible instead. This results in a
  newline terminated file, as above.

* However, if there is a newline at the end of a file (remember that this
  includes the case of an empty file), a hunk can /remove/ it by removing an
  empty line before adding anything. This results in a file that is /not/
  newline terminated.

The invisible newline character at the front is, of course, not present
anywhere in the representation of files, it is just a conceptual tool.

The algorithm below is highly optimized to minimize allocation of
intermediate ByteStrings. -}

applyHunkLines :: (Int, [B.ByteString], [B.ByteString])
               -> FileContents
               -> Either String FileContents
applyHunkLines (line, old, new) content
  | line == 1 =
      {- This case is subtle because here we have to deal with any invisible
      newline at the front of a file without it actually being present. We
      first try to drop everything up to the (length old)'th newline. 

      If this fails, we know that the content was not newline terminated. So
      we replace everything with the new content, interspersing but not
      terminating the lines with newline characters.

      If it succeeds, we insert the new content, interspersing /and/
      terminating the lines with newline characters before appending the
      rest of the content. -}
      case breakAfterNthNewline (length old) content of
        Nothing
          -- old content is not newline terminated
          | content == unlinesPS old -> Right $ unlinesPS new
          | otherwise -> Left "Hunk wants to remove content that isn't there"
        Just (should_be_old, suffix)
          -- old content is newline terminated
          | should_be_old == BC.unlines old ->
              Right $ unlinesPS $ new ++ [suffix]
          | otherwise ->
              Left "Hunk wants to remove content that isn't there"
  | line >= 2 = do
      {- This is the simpler case. We can be sure that we have at least one
      newline character at the point where we modify the file. This means we
      can apply the conceptual view literally, i.e. replace old content with
      new content /before/ this newline, where the lines in the old and new
      content are /preceded/ by newline characters. -}
      (pre, start) <- breakBeforeNthNewline (line-2) content
      let hunkContent ls = unlinesPS (B.empty:ls)
      post <- dropPrefix (hunkContent old) start
      return $ B.concat [pre, hunkContent new, post]
  | otherwise = Left "Hunk has zero or negative line number"
  where
    dropPrefix x y
      | x `B.isPrefixOf` y = Right $ B.drop (B.length x) y
      | otherwise =
        Left $ "Hunk wants to remove content that isn't there"

breakAfterNthNewline :: Int -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
breakAfterNthNewline 0 the_ps = Just (B.empty, the_ps)
breakAfterNthNewline n _ | n < 0 = error "precondition of breakAfterNthNewline"
breakAfterNthNewline n the_ps = go n (BC.elemIndices '\n' the_ps)
  where
    go _ [] = Nothing -- we have fewer than n newlines
    go 1 (i:_) = Just $ B.splitAt (i + 1) the_ps
    go !m (_:is) = go (m - 1) is

breakBeforeNthNewline :: Int -> B.ByteString -> Either String (B.ByteString, B.ByteString)
breakBeforeNthNewline n _ | n < 0 = error "precondition of breakBeforeNthNewline"
breakBeforeNthNewline n the_ps = go n (BC.elemIndices '\n' the_ps)
  where
    go 0 [] = Right (the_ps, B.empty)
    go 0 (i:_) = Right $ B.splitAt i the_ps
    go !m (_:is) = go (m - 1) is
    go _ [] = Left "Line number does not exist"

instance PrimDetails (With AnchoredPath FilePatchType) where
  summarizePrim (With f (Hunk _ o n)) = [SummFile SummMod f (length o) (length n) 0]
  summarizePrim (With f (Binary _ _)) = [SummFile SummMod f 0 0 0]
  summarizePrim (With f (TokReplace{})) = [SummFile SummMod f 0 0 1]

instance PrimShow (With AnchoredPath FilePatchType) where
  showPrim fmt (With f (Hunk line old new)) = showHunk fmt f line old new
  showPrim fmt (With f (TokReplace t old new)) = showTok fmt f t old new
  showPrim fmt (With f (Binary old new)) = showBinary fmt f old new

showHunk :: FileNameFormat -> AnchoredPath -> Int -> [B.ByteString] -> [B.ByteString] -> Doc
showHunk fmt f line old new = showFileHunk fmt (FileHunk () f line old new)

showTok
  :: FileNameFormat -> AnchoredPath -> String -> B.ByteString -> B.ByteString -> Doc
showTok fmt f t o n =
  blueText "replace" <+>
  formatFileName fmt f <+>
  text "[" <> text t <> text "]" <+> packedString o <+> packedString n

showBinary :: FileNameFormat -> AnchoredPath -> B.ByteString -> B.ByteString -> Doc
showBinary fmt f o n =
  blueText "binary" <+>
  formatFileName fmt f $$ invisibleText "oldhex" $$
  vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex o) $$
  invisibleText "newhex" $$
  vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex n)
  where
    makeprintable ps = invisibleText "*" <> invisiblePS ps

breakEvery :: Int -> B.ByteString -> [B.ByteString]
breakEvery n ps
  | B.length ps < n = [ps]
  | otherwise = B.take n ps : breakEvery n (B.drop n ps)

instance PrimRead (With AnchoredPath FilePatchType) where
  readPrim fmt =
    skipSpace >>
    choice
      [ seal <$> readHunk fmt
      , seal <$> readTok fmt
      , seal <$> readBinary fmt
      ]

readHunk :: FileNameFormat -> Parser (With AnchoredPath FilePatchType wX wY)
readHunk fmt = do
  string "hunk"
  fi <- readFileName fmt
  l <- int
  have_nl <- skipNewline
  if have_nl
    then do
      _ <- linesStartingWith ' ' -- skipping context
      old <- linesStartingWith '-'
      new <- linesStartingWith '+'
      _ <- linesStartingWith ' ' -- skipping context
      return $ With fi (Hunk l old new)
    else return $ With fi $ Hunk l [] []
  where
    skipNewline = option False (char '\n' >> return True)

readTok :: FileNameFormat -> Parser (With AnchoredPath FilePatchType wX wY)
readTok fmt = do
  string "replace"
  f <- readFileName fmt
  regstr <- do
    w <- lexWord
    guard $ B.length w > 2 && BC.head w == '[' && BC.last w == ']'
    return $ B.init $ B.tail w
  o <- lexWord
  n <- lexWord
  return $ With f $ TokReplace (BC.unpack regstr) o n

-- | Binary format:
--
-- > binary FILENAME
-- > oldhex
-- > *HEXHEXHEX
-- > ...
-- > newhex
-- > *HEXHEXHEX
-- > ...
readBinary :: FileNameFormat -> Parser (With AnchoredPath FilePatchType wX wY)
readBinary fmt = do
  string "binary"
  fi <- readFileName fmt
  _ <- lexWord
  skipSpace
  old <- linesStartingWith '*'
  r_old <- either fail return $ fromHex2PS $ B.concat old
  _ <- lexWord
  skipSpace
  new <- linesStartingWith '*'
  r_new <- either fail return $ fromHex2PS $ B.concat new
  return $ With fi $ Binary r_old r_new

instance Commute FilePatchType where
  -- empty hunks commute with any other patch;
  -- this needs to be retained for compatibility
  commute pair@(_ :> Hunk _ [] []) = trivialCommute pair
  commute pair@(Hunk _ [] [] :> _) = trivialCommute pair
  commute (Hunk line1 old1 new1 :> Hunk line2 old2 new2) = do
    (line2', line1') <-
      commuteHunkLines
        line1 (length old1) (length new1)
        line2 (length old2) (length new2)
    return (Hunk line2' old2 new2 :> Hunk line1' old1 new1)
  commute (Hunk line1 old1 new1 :> TokReplace t o n) = do
    let applyTokReplaces = mapM (tryTokReplace t o n)
    old1' <- applyTokReplaces old1
    new1' <- applyTokReplaces new1
    return (TokReplace t o n :> Hunk line1 old1' new1')
  commute pair@(TokReplace {} :> Hunk {}) = invertCommute commute pair
  commute (TokReplace t1 o1 n1 :> TokReplace t2 o2 n2)
    | t1 /= t2 = Nothing
    | o1 == o2 = Nothing
    | n1 == o2 = Nothing
    | o1 == n2 = Nothing
    | n1 == n2 = Nothing
    | otherwise = return (TokReplace t2 o2 n2 :> TokReplace t1 o1 n1)
  commute (Binary {} :> _) = Nothing
  commute (_ :> Binary {}) = Nothing

commuteHunkLines :: Int -> Int -> Int -> Int -> Int -> Int
                 -> Maybe (Int, Int)
commuteHunkLines line1 len_old1 len_new1 line2 len_old2 len_new2
  | line1 + len_new1 < line2  = Just (line2 - len_new1 + len_old1, line1)
  | line2 + len_old2 < line1  = Just (line2, line1 + len_new2 - len_old2)
  | len_old2 /= 0
  , len_old1 /= 0
  , len_new2 /= 0
  , len_new1 /= 0
  , line1 + len_new1 == line2 = Just (line2 - len_new1 + len_old1, line1)
  | len_old2 /= 0
  , len_old1 /= 0
  , len_new2 /= 0
  , len_new1 /= 0
  , line2 + len_old2 == line1 = Just (line2, line1 + len_new2 - len_old2)
  | otherwise                 = Nothing
