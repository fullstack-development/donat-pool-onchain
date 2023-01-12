module Ext.Ply.Plutarch.TypedWriter where

import qualified Data.Text as T
import Plutarch
import Ply.Core.Internal.Reify
import Ply.Plutarch
import Ply.Plutarch.TypedWriter
import System.FilePath ((</>))

-- TODO: change the directory path when we know where to put compiled scripts
compiledDirectory :: FilePath
compiledDirectory = "compiled"

writeTypedScriptTraced ::
  ( ReifyVersion (VersionOf pt)
  , ReifyRole (RoleOf pt)
  , ReifyTypenames (PlyParamsOf (ParamsOf pt))
  ) =>
  T.Text ->
  FilePath ->
  ClosedTerm pt ->
  IO ()
writeTypedScriptTraced scriptPurpose fileName =
  writeTypedScript
    (Config {tracingMode = DoTracing})
    scriptPurpose
    (compiledDirectory </> fileName)
