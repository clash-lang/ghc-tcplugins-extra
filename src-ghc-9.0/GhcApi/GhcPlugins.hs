module GhcApi.GhcPlugins (module GHC.Plugins, findPluginModule, panicDoc) where

import GHC.Plugins hiding (TcPlugin, mkSubst)
import GHC.Driver.Finder (findPluginModule)
import GHC.Utils.Panic (panicDoc)
