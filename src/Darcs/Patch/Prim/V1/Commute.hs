{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Patch.Prim.V1.Commute () where

import Darcs.Prelude

import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

import Darcs.Util.Path ( movedirfilename, isPrefix )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..) )
import Darcs.Patch.Prim.V1.Core
     ( Prim(..), FilePatchType(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( primCleanMerge )
import Darcs.Patch.TokenReplace ( tryTokReplace )

failed :: Maybe a
failed = Nothing

type CommuteFunction p = CommuteFn p p

-- | Use the invert-commute law.
invertCommute :: Invert p => CommuteFunction p -> CommuteFunction p
invertCommute c (p1:>p2) = do
    ip1' :> ip2' <- c (invert p2 :> invert p1)
    return (invert ip2' :> invert ip1')

instance Commute Prim where
    commute = commuteFiledir

commuteFiledir :: CommuteFunction Prim
commuteFiledir (FP f1 p1 :> FP f2 p2)
    | f1 == f2 = do
        p2' :> p1' <- commuteFP (p1 :> p2)
        return (FP f2 p2' :> FP f1 p1')
    | otherwise = return (FP f2 (unsafeCoerceP p2) :> FP f1 (unsafeCoerceP p1))
commuteFiledir (DP d1 p1 :> DP d2 p2)
    | isPrefix d1 d2 || isPrefix d2 d1 = failed
    | otherwise = return (DP d2 (unsafeCoerceP p2) :> DP d1 (unsafeCoerceP p1))
commuteFiledir (FP f fp :> DP d dp)
    | isPrefix d f = failed
    | otherwise = return (DP d (unsafeCoerceP dp) :> FP f (unsafeCoerceP fp))
commuteFiledir pair@(DP _ _ :> FP _ _) = invertCommute commuteFiledir pair
commuteFiledir (FP f1 p1 :> Move d d')
    | f1 == d' = failed
    | (p1 == AddFile || p1 == RmFile) && d == f1 = failed
    | otherwise =
        return (Move d d' :> FP (movedirfilename d d' f1) (unsafeCoerceP p1))
commuteFiledir pair@(Move _ _ :> FP _ _) = invertCommute commuteFiledir pair
commuteFiledir (DP d1 p1 :> Move d d')
    | isPrefix d1 d' || isPrefix d1 d = failed
    | otherwise =
        return (Move d d' :> DP (movedirfilename d d' d1) (unsafeCoerceP p1))
commuteFiledir pair@(Move _ _ :> DP _ _) = invertCommute commuteFiledir pair
commuteFiledir (Move f f' :> Move d d')
    | f == d' || f' == d = failed
    | f == d || f' == d' = failed
    | d `isPrefix` f && f' `isPrefix` d' = failed
    | otherwise =
      return
        (Move (movedirfilename f' f d) (movedirfilename f' f d') :>
         Move (movedirfilename d d' f) (movedirfilename d d' f'))
commuteFiledir (p1 :> ChangePref p f t) =
    return (ChangePref p f t :> unsafeCoerceP p1)
commuteFiledir (ChangePref p f t :> p2) =
    return (unsafeCoerceP p2 :> ChangePref p f t)

commuteFP :: CommuteFunction FilePatchType
commuteFP (p1 :> Hunk line1 [] []) =
    return (Hunk line1 [] [] :> unsafeCoerceP p1)
commuteFP (Hunk line1 [] [] :> p2) =
    return (unsafeCoerceP p2 :> Hunk line1 [] [])
commuteFP (Hunk line1 old1 new1 :> Hunk line2 old2 new2) =
    case commuteHunkLines line1 (length old1) (length new1) line2 (length old2) (length new2) of
      Just (line2', line1') ->
        return (Hunk line2' old2 new2 :> Hunk line1' old1 new1)
      Nothing -> failed
commuteFP (Hunk line1 old1 new1 :> TokReplace t o n) =
    let po = BC.pack o; pn = BC.pack n in
    case tryTokReplaces t po pn old1 of
    Nothing -> failed
    Just old1' ->
      case tryTokReplaces t po pn new1 of
        Nothing -> failed
        Just new1' -> return (TokReplace t o n :> Hunk line1 old1' new1')
commuteFP pair@(TokReplace {} :> Hunk {}) = invertCommute commuteFP pair
commuteFP (TokReplace t1 o1 n1 :> TokReplace t2 o2 n2)
    | t1 /= t2 = failed
    | o1 == o2 = failed
    | n1 == o2 = failed
    | o1 == n2 = failed
    | n1 == n2 = failed
    | otherwise = return (TokReplace t2 o2 n2 :> TokReplace t1 o1 n1)
commuteFP (AddFile :> _) = failed
commuteFP (RmFile :> _) = failed
commuteFP (Binary {} :> _) = failed
commuteFP (_ :> AddFile) = failed
commuteFP (_ :> RmFile) = failed
commuteFP (_ :> Binary {}) = failed

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

tryTokReplaces :: String -> B.ByteString -> B.ByteString
               -> [B.ByteString] -> Maybe [B.ByteString]
tryTokReplaces t o n = mapM (tryTokReplace t o n)

instance CleanMerge Prim where
  cleanMerge = primCleanMerge
