module Constants where

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

defaultExecutable :: String
defaultExecutable = "solution.bin"

defaultInputFileName :: String
defaultInputFileName = "input.txt"

defaultTestSuiteName :: String
defaultTestSuiteName = "testsuite.bin"

defaultTestRuleName :: String
defaultTestRuleName = "test"

defaultExplanationName :: String
defaultExplanationName = "explanation.tex"

publishImportsFileName :: String
publishImportsFileName = "explanations.tex"
