{-# LANGUAGE RecordWildCards #-}
module Development.KansasLava.Shake.Xilinx
       ( XilinxConfig(..), XilinxTarget(..)
       , xilinxRules
       ) where

import Development.Shake
import Development.Shake.FilePath

import qualified Data.Text.Lazy as TL
import Text.Hastache
import Text.Hastache.Context

import Paths_kansas_lava_shake

data XilinxTarget = XilinxTarget{ targetFamily, targetDevice, targetSpeed, targetPackage :: String }

data XilinxConfig = XilinxConfig{ xilinxRoot :: FilePath
                                , xilinxTarget :: XilinxTarget
                                }

xilinxRules :: XilinxConfig -> FilePath -> String -> [FilePath] -> [FilePath] -> Rules ()
xilinxRules XilinxConfig{..} outDir projName srcs ipcores = do
    outDir </> projName <.> "bit" %> \_out -> do
        need . concat $ [ [ outDir </> src | src <- srcs ]
                        , [ outDir </> "ipcore_dir" </> xco | xco <- ipcores ]
                        , [ outDir </> projName <.> "tcl" ]
                        ]

        xilinx "xtclsh" [projName <.> "tcl", "rebuild_project"]

    "build" </> "*.tcl" %> do
        hastache projCtxt
  where
    xilinx tool args = cmd (Cwd outDir) (xilinxRoot </> tool) args

    projCtxt = mkStrContext $ \key -> case key of
        "project" -> MuVariable projName
        "targetFamily" -> MuVariable targetFamily
        "targetDevice" -> MuVariable targetDevice
        "targetSpeed" -> MuVariable targetSpeed
        "targetPackage" -> MuVariable targetPackage
        "ipcores" -> MuList [ mkStrContext $ \key -> case key of
                                   "name" -> MuVariable name
                                   _ -> MuNothing
                            | xco <- ipcores
                            , let name = dropExtension xco
                            ]
        "srcs" -> MuList [ mkStrContext $ \key -> case key of
                                "fileName" -> MuVariable src
                                _ -> MuNothing
                         | src <- srcs
                         ]
        _ -> MuNothing
      where
        XilinxTarget{..} = xilinxTarget

hastache :: MuContext IO -> FilePath -> Action ()
hastache ctxt target = do
    alwaysRerun
    templateFile <- liftIO $ getDataFileName ("ise.template" </> templateName)
    t <- liftIO $ hastacheFile hastacheConfig templateFile ctxt
    writeFileChanged target $ TL.unpack t
  where
    hastacheConfig = MuConfig{ muEscapeFunc = emptyEscape
                             , muTemplateFileDir = Nothing
                             , muTemplateFileExt = Just "mustache"
                             , muTemplateRead = const $ return Nothing
                             }

    ext = drop 1 . takeExtension $ target
    templateName = ext <.> "mustache"
