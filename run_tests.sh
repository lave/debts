#!/bin/sh

runhaskell -w -i./src -i./testsuite/tests -i./dist/build/debts/debts-tmp/ ./testsuite/tests/RunTests.hs
