module Publish where

import           System.FilePath.Posix ((</>))

import           Constants
import           Paths
import           Solutions
import           Types

publish :: EdenPublish ()
publish = do
    checkEden

    generateExplanationImports

generateExplanationImports :: EdenPublish ()
generateExplanationImports = do
    exp <- explanations
    let str = unlines $ map makeImport exp
    dir <- publishDir
    liftIO $ writeFile (dir </> publishImportsFileName) str
  where
    makeImport :: Problem -> String
    makeImport p = "\\subfile{../" ++ problemDirName p </> defaultExplanationName ++ "}"
