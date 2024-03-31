-- | What is the git commit used to build the code?
--

-- NOTE: this code is auto-generated from autoconf/lib-Git.hs.in

module Git (CommitId, fromCommitId, gitCommitId) where

import Data.Text (Text, pack)

-- | An identifier for the version of code.
newtype CommitId = CId Text deriving Eq

-- | Extract the identifier value.
fromCommitId :: CommitId -> Text
fromCommitId (CId cid) = cid

-- | This is an indication of what version of the code is being used.
--   It is not guaranteed to be correct for development builds, as
--   it only uses the git hash of the last commit. It really should
--   be an IO action, in that it is not guaranteed to be constant
--   between builds, but for now I leave it as below, since it is constant
--   within a build.
--
--   It is expected that this code wil lbe replaced by
--
--   > gitCommitId = Just (CId (pack "@@GITHASH@@"))
--
--   (after expansion of @@GITHASH@@) by add-commit.sh
--
gitCommitId :: Maybe CommitId
gitCommitId = Just (CId (pack "66aa9de18266094a9e9d0122061e40869512164f"))

