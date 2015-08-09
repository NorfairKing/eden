module Publish where

import           Data.List
import           System.Directory      (doesFileExist, removeFile,
                                        setCurrentDirectory)
import           System.FilePath.Posix (replaceExtension, (</>))

import           Constants
import           Eden
import           Paths
import           Solutions
import           Types
import           Utils

publish :: EdenPublish ()
publish = do
    target <- askEden publish_target

    case target of
        PublishProblem p -> buildWriteupForProblem p
        PublishAll -> do
            generateExplanationImports
            buildWriteups

generateExplanationImports :: EdenPublish ()
generateExplanationImports = do
    exp <- explanations
    let str = unlines . sort $ map makeImport exp
    dir <- publishDir
    liftIO $ writeFile (dir </> publishImportsFileName) str
  where
    makeImport :: Problem -> String
    makeImport p = "\\subfile{../" ++ problemDirName p </> defaultExplanationName ++ "}"

buildWriteups :: EdenPublish ()
buildWriteups = buildLatex $ mainWriteupFile

buildWriteupForProblem :: Problem -> EdenPublish ()
buildWriteupForProblem p = do
    path <- explanationPath p
    let resultpath = replaceExtension path "pdf"
    exists <- liftIO $ doesFileExist resultpath
    if exists
    then liftIO $ removeFile resultpath
    else return ()
    buildLatex path

buildLatex :: FilePath -> EdenPublish ()
buildLatex fp = do
    dir <- publishDir
    liftIO $ setCurrentDirectory dir
    runRaw $ unwords $ [
                  "latexmk"
                , "-pdf"
                , "-pdflatex=\"pdflatex -shell-escape -halt-on-error -enable-pipes -enable-write18\""
                , fp
                ]


