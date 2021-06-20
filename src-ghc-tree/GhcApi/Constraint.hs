module GhcApi.Constraint
  ( Ct(..)
  , CtEvidence(..)
  , CtLoc
  , ctLoc
  , ctEvId
  , mkNonCanonical
  )
where

import GHC.Tc.Types.Constraint
  (Ct (..), CtEvidence (..), CtLoc, ctLoc, ctEvId, mkNonCanonical)
