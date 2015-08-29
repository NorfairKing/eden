module Publish where

import           Data.List
import           System.Directory      (doesFileExist, getDirectoryContents,
                                        removeFile, setCurrentDirectory)
import           System.FilePath.Posix (replaceExtension, takeExtension, (<.>),
                                        (</>))

import           Constants
import           Solutions
import           Types
import           Utils

publish :: PublishTarget -> EdenPublish ()
publish (PublishProblem p)  = buildWriteupForProblem p
publish (PublishLibrary fp) = publishLibrary fp
publish (PublishPart fp)    = publishPart fp
publish PublishAll          = do
            generateExplanationImports
            generateLibraryImports
            buildWriteups

publishLibrary :: FilePath -> EdenPublish ()
publishLibrary fp = do
    ld <- libDir
    buildLatex $ ld </> fp <.> "tex"

publishPart :: FilePath -> EdenPublish ()
publishPart fp = do
    pd <- publishDir
    buildLatex $ pd </> fp <.> "tex"

generateExplanationImports :: EdenPublish ()
generateExplanationImports = do
    exp <- explanations
    let str = unlines . sort $ map makeImport exp
    dir <- publishDir
    liftIO $ writeFile (dir </> publishImportsFileName) str
  where
    makeImport :: Problem -> String
    makeImport p = "\\subfile{../" ++ problemDirName p </> defaultExplanationName ++ "}"

generateLibraryImports :: EdenPublish ()
generateLibraryImports = do
    ld <- libDir
    cts <- liftIO $ getDirectoryContents ld
    let files = filter (\f -> takeExtension f == ".tex") cts
    let str = unlines . sort $ map makeImport files
    dir <- publishDir
    liftIO $ writeFile (dir </> publishLibraryImportsFileName) str
  where
    makeImport :: FilePath -> String
    makeImport f = "\\subfile{../lib/" ++ f ++"}"

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
                , "-pdflatex=\"pdflatex -shell-escape -halt-on-error -enable-write18\""
                , fp
                ]


