module Darcs.Patch.RepoType ( RepoType ) where

-- |This type is intended to be used as a phantom type via the 'DataKinds'
-- extension. It could be used to track different types of repositories.
-- This feature is currently not used.
data RepoType
