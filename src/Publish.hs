module Publish where

import           System.Directory      (setCurrentDirectory)
import           System.FilePath.Posix ((</>))

import           Constants
import           Paths
import           Solutions
import           Types
import           Utils

publish :: EdenPublish ()
publish = do
    checkEden

    generateExplanationImports
    buildWriteups

generateExplanationImports :: EdenPublish ()
generateExplanationImports = do
    exp <- explanations
    let str = unlines $ map makeImport exp
    dir <- publishDir
    liftIO $ writeFile (dir </> publishImportsFileName) str
  where
    makeImport :: Problem -> String
    makeImport p = "\\subfile{../" ++ problemDirName p </> defaultExplanationName ++ "}"

buildWriteups :: EdenPublish ()
buildWriteups = do
    dir <- publishDir
    liftIO $ setCurrentDirectory dir
    let cmd = unwords $ [
                "latexmk"
            ,   "-pdf"
            ,   "-pdflatex=\"pdflatex -shell-escape -halt-on-error -enable-pipes -enable-write18\""
            ,   mainWriteupFile
            ]
    runRaw cmd




