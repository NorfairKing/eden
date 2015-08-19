module Constants where

import           System.FilePath.Posix ((<.>))

edenName :: String
edenName = "eden"

dotEdenRootName :: String
dotEdenRootName = '.' : edenName

versionFileName :: String
versionFileName = "VERSION"

paddingLength :: Int
paddingLength = 3

libDirName :: String
libDirName = "lib"

testDirName :: String
testDirName = "test"

buildDirName :: String
buildDirName = "build"

publishingDirName :: String
publishingDirName = "writeup"

defaultMakefileName :: String
defaultMakefileName = "makefile"

defaultExecutableName :: String
defaultExecutableName = "solution.bin"

defaultInputFileName :: String
defaultInputFileName = "input.txt"

defaultOutputFileName :: String
defaultOutputFileName = "output.txt"

defaultTestSuiteName :: String
defaultTestSuiteName = "testsuite.bin"

defaultTestRuleName :: String
defaultTestRuleName = "test"

defaultWriteupExtenstion :: String
defaultWriteupExtenstion = "tex"

defaultExplanationName :: String
defaultExplanationName = "explanation" <.> defaultWriteupExtenstion

publishImportsFileName :: String
publishImportsFileName = "explanations" <.> defaultWriteupExtenstion

publishLibraryImportsFileName :: String
publishLibraryImportsFileName = "libraries" <.> defaultWriteupExtenstion

mainWriteupFile :: String
mainWriteupFile = "main" <.> defaultWriteupExtenstion
