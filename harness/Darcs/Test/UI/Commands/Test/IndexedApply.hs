module Darcs.Test.UI.Commands.Test.IndexedApply
  ( IndexedApply(..)    
  ) where


import Darcs.Prelude hiding ( Monad(..) )

import Darcs.Util.IndexedMonad

import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..) )

import Darcs.UI.Commands.Test.Impl ( PatchSeq(..) )

-- our own indexed monad Apply class
class IndexedApply p where
  type ApplyState p :: Type -> Type -> Type -> Type
  apply :: Monad (ApplyState p) => p wX wY -> ApplyState p wX wY ()
  unapply :: Monad (ApplyState p) => p wX wY -> ApplyState p wY wX ()

instance IndexedApply p => IndexedApply (FL p) where
  type ApplyState (FL p) = ApplyState p
  apply NilFL = return ()
  apply (p :>: ps) = apply p >> apply ps
  unapply NilFL = return ()
  unapply (p :>: ps) = unapply ps >> unapply p

instance IndexedApply p => IndexedApply (RL p) where
  type ApplyState (RL p) = ApplyState p
  apply NilRL = return ()
  apply (ps :<: p) = apply ps >> apply p
  unapply NilRL = return ()
  unapply (ps :<: p) = unapply p >> unapply ps

instance IndexedApply p => IndexedApply (PatchSeq p) where
  type ApplyState (PatchSeq p) = ApplyState p
  apply (Single p) = apply p
  apply (Joined p1 p2) = apply p1 >> apply p2
  unapply (Single p) = unapply p
  unapply (Joined p1 p2) = unapply p2 >> unapply p1
