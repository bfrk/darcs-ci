-- | Deques a la Chris Okasaki
module Darcs.Patch.Witnesses.Balanced
  ( BL
  , lengthBL
  , nilBL
  , consL
  , consR
  , viewL
  , viewR
  , (+<>+)
  ) where

import Darcs.Prelude
import Darcs.Patch.Witnesses.Ordered

-- | Double ended queue.
data BL p wX wY where
  BL :: Int -> FL p wX wM -> Int -> RL p wM wY -> BL p wX wY

{-
-- | Left view of a 'BL'
data LBL p wX wY where
  NilLBL :: LBL p wX wX
  (:<|:) :: p wX wY -> BL p wY wZ -> LBL p wX wZ

-- | Right view of a 'BL'
data RBL p wX wY where
  NilRBL :: RBL p wX wX
  (:|>:) :: BL p wX wY -> p wY wZ -> RBL p wX wZ
-}

lengthBL :: BL p wX wY -> Int
lengthBL (BL llen _ rlen _) = llen + rlen

nilBL :: BL p wY wY
nilBL = BL 0 NilFL 0 NilRL

consL :: p wX wY -> BL p wY wZ -> BL p wX wZ
consL x (BL llen l rlen r) = makeBL (llen+1) (x:>:l) rlen r

consR :: BL p wX wY -> p wY wZ -> BL p wX wZ
consR (BL llen l rlen r) y = makeBL llen l (rlen+1) (r:<:y)

viewL :: BL p wX wY -> Maybe ((p :> BL p) wX wY)
viewL (BL _ NilFL _ NilRL) = Nothing
viewL (BL _ NilFL _ (NilRL:<:y)) = Just (y :> nilBL)
viewL (BL _ NilFL _ (_:<:_)) = error "impossible"
viewL (BL llen (x:>:xs) rlen r) = Just (x :> makeBL (llen-1) xs rlen r)

viewR :: BL p wX wY -> Maybe ((BL p :> p) wX wY)
viewR (BL _ NilFL _ NilRL) = Nothing
viewR (BL _ (x:>:NilFL) _ NilRL) = Just (nilBL :> x)
viewR (BL _ (_:>:_) _ NilRL) = error "impossible"
viewR (BL llen l rlen (ys:<:y)) = Just (makeBL llen l (rlen-1) ys :> y)

-- | Smart constructor that re-balances the sequences.
makeBL :: Int -> FL p wX wM -> Int -> RL p wM wY -> BL p wX wY
makeBL llen l rlen r
  | llen > c * rlen + 1 =
    case splitAtFL n l of
      l' :> l'' -> BL n l' (llen+rlen-n) (reverseFL l'' +<+ r)
  | rlen > c * llen + 1 =
    case splitAtRL n r of
      r'' :> r' -> BL (llen+rlen-n) (l +>+ reverseRL r'') n r'
  | otherwise = BL llen l rlen r
  where
    n = (llen + rlen) `div` 2
    c = 4

{-
(+<<>+) :: FL p wA wB -> RL p wB wC -> RL p wA wC
l +<<>+ r = reverseFL l +<+ r

(+<>>+) :: FL p wA wB -> RL p wB wC -> FL p wA wC
l +<>>+ r = l +>+ reverseRL r
-}

-- | Concatenate two 'BL's.
(+<>+) :: BL p wX wY -> BL p wY wZ -> BL p wX wZ
BL llen l xlen x +<>+ BL ylen y rlen r
  | llen + xlen < ylen + rlen =
    makeBL (llen+xlen+ylen) (l +>+ x +>>+ y) rlen r
  | otherwise =
    makeBL llen l (xlen+ylen+rlen) (x +<<+ y +<+ r)
