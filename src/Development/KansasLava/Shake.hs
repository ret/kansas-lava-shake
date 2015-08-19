module Development.KansasLava.Shake
       ( lavaRules
       ) where

import Development.Shake
import Development.Shake.FilePath
import Language.KansasLava.VHDL (writeVhdlPrelude)
import Control.Monad

lavaRules :: FilePath -> [(FilePath, String)] -> Rules ()
lavaRules outDir genVHDLs = do
    outDir </> "gensrc/lava-prelude.vhdl" %> \out -> do
        alwaysRerun
        withTempFile $ \tempFile -> do
            liftIO $ writeVhdlPrelude tempFile
            copyFileChanged tempFile out

    forM_ genVHDLs $ \(modName, vhdl) -> do
        outDir </> "gensrc" </> modName <.> "vhdl" %> \out -> do
            alwaysRerun
            writeFileChanged out vhdl
