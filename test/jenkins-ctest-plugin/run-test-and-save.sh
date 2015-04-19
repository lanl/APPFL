#!/bin/sh
ctest -D ExperimentalTest --no-compress-output
cp Testing/`head -n 1 Testing/TAG`/Test.xml ./CTestResults.xml
