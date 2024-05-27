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
  (Ct (..), CtEvidence (..), CanEqLHS (..), CtLoc, ctLoc, ctEvId, mkNonCanonical)
