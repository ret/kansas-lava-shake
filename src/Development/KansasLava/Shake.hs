module Development.KansasLava.Shake
       ( lavaRules
       ) where

import Development.Shake
import Development.Shake.FilePath
import Language.KansasLava.VHDL (writeVhdlPrelude)
import System.IO.Temp

lavaRules :: FilePath -> String -> String -> Rules ()
lavaRules modName vhdl ucf = do
    "gensrc" </> modName <.> "vhdl" *> \target -> do
        alwaysRerun
        writeFileChanged target vhdl
    "gensrc" </> modName <.> "ucf" *> \target -> do
        alwaysRerun
        writeFileChanged target ucf
    "gensrc/lava-prelude.vhdl" *> \target -> do
        alwaysRerun
        let (dir, template) = splitFileName target
        (tempFile, _) <- liftIO $ openTempFile dir template
        liftIO $ writeVhdlPrelude tempFile
        copyFileChanged tempFile target
        liftIO $ removeFiles "." [tempFile]
