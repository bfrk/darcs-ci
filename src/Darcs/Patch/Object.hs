module Darcs.Patch.Object where

import Darcs.Prelude

import Darcs.Util.Path ( AnchoredPath, anchorPath )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree ( Tree )

-- | Given a state type (parameterized over a monad m :: Type -> Type), this gives us
-- the type of the key with which we can lookup an item (or object) in the
-- state.
type family ObjectIdOf (state :: (Type -> Type) -> Type)

-- | We require from such a key (an 'ObjectId') that it has a canonical way
-- to format itself to a 'Doc'.
class Eq oid => ObjectId oid where
  showObjectId :: oid -> Doc

type instance ObjectIdOf Tree = AnchoredPath

instance ObjectId AnchoredPath where
  showObjectId = text . anchorPath "."
