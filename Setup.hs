import Distribution.Simple
import System.Cmd(system)

main = defaultMainWithHooks (simpleUserHooks {runTests = doRunTests})

--doRunTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
doRunTests a b pd lb = system ( "runhaskell -i./src -i./testsuite/tests -i./dist/build/debts/debts-tmp/ ./testsuite/tests/RunTests.hs") >> return()

