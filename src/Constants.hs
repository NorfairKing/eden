module Constants where

edenName :: String
edenName = "eden"

edenRootName :: String
edenRootName = '.' : edenName

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
