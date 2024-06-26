module Darcs.Patch.Format
    ( PatchListFormat(..)
    , ListFormat(..)
    , FileNameFormat(..)
    ) where

import Darcs.Prelude

-- | Showing and reading lists of patches. This class allows us to control how
-- lists of patches are formatted on disk. For legacy reasons V1 patches have
-- their own special treatment (see 'ListFormat'). Other patch types use the
-- default format which just puts them in a sequence without separators or any
-- prelude/epilogue.
--
-- This means that 'FL (FL p)' etc would be ambiguous, so there are no
-- instances for 'FL p' or other list types.
class PatchListFormat p where
    patchListFormat :: ListFormat p
    patchListFormat = ListFormatDefault

-- | This type is used to tweak the way that lists of 'p' are shown for a given
-- 'Patch' type 'p'. It is needed to maintain backwards compatibility for V1
-- and V2 patches.
data ListFormat (p :: Type -> Type -> Type)
    = ListFormatDefault -- ^ Show and read lists without braces.
    | ListFormatV1      -- ^ Show lists with a single layer of braces around
                        -- the outside, except for singletons which have no
                        -- braces. Read with arbitrary nested braces and parens
                        -- and flatten them out.
    | ListFormatV2      -- ^ Show lists without braces. Read with arbitrary
                        -- nested parens and flatten them out.
    | ListFormatV3      -- ^ Temporary hack to disable use of showContextSeries
                        -- for darcs-3 patches, until I find out how to fix this.

data FileNameFormat
    = FileNameFormatV1      -- ^ on-disk format for V1 patches
    | FileNameFormatV2      -- ^ on-disk format for V2 patches
    | FileNameFormatDisplay -- ^ display format
    deriving (Eq, Show)
