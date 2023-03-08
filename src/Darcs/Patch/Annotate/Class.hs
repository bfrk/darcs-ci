module Darcs.Patch.Annotate.Class where

import Darcs.Prelude

import Control.Monad.State ( State )
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Darcs.Patch.Info ( PatchInfo )
import Darcs.Util.Path ( AnchoredPath )

type AnnotateResult = V.Vector (Maybe PatchInfo, B.ByteString)

data Content2 f g
  = FileContent (f (g B.ByteString))
  | DirContent (f (g AnchoredPath))

data Annotated2 f g = Annotated2
  { annotated :: !AnnotateResult
  , current :: !(Content2 f g)
  , currentPath :: (Maybe AnchoredPath)
  , currentInfo :: PatchInfo
  }

type Content = Content2 [] ((,) Int)
type Annotated = Annotated2 [] ((,) Int)

deriving instance Eq Content
deriving instance Show Content

deriving instance Eq Annotated
deriving instance Show Annotated

type AnnotatedM = State Annotated

class Annotate p where
  annotate :: p wX wY -> AnnotatedM ()
