import Distribution.Simple
main = defaultMainWithHooks (simpleUserHooks {runTests = doRunTests})

doRunTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
doRunTests a b pd lb = system ( "runhaskell ./testsuite/tests/RunTests.hs") >> return()

