module LDtk
  ( module LDtk
  , module LDtk.Types
  ) where

import Data.Aeson
import LDtk.Types hiding (ldtkOpts, parseFieldValue)

------------------------------------------------------------------------------
-- | @since 1.2.3
loadLDtk :: FilePath -> IO (Either String LDtkRoot)
loadLDtk = eitherDecodeFileStrict

