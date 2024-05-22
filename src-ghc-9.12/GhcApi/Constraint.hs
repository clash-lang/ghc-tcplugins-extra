module GhcApi.Constraint
  ( Ct(..)
  , CtEvidence(..)
  , CtLoc
  , CanEqLHS(..)
  , ctLoc
  , ctEvId
  , mkNonCanonical
  )
where

import GHC.Tc.Types.Constraint
  (Ct (..), CtEvidence (..), CanEqLHS (..), ctLoc, ctEvId, mkNonCanonical)
import GHC.Tc.Types.CtLoc (CtLoc)
