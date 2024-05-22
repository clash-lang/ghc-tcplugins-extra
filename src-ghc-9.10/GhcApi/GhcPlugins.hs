module GhcApi.GhcPlugins (module GHC.Plugins, FindResult(..), findPluginModule) where

import GHC.Plugins hiding (TcPlugin)
import GHC.Unit.Finder (findPluginModule)
import GHC.Tc.Plugin (FindResult(..))
