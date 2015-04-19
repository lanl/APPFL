# jenkins-ctest-plugin + [xUnit-plugin](http://wiki.jenkins-ci.org/display/JENKINS/xUnit+Plugin "xUnit Plugin") = test reports for [Jenkins-CI](http://jenkins-ci.org/ "Extensible continuous integration server")

## Installation

1. Make sure you've installed the xUnit Plugin
2. Get the `ctest-to-junit.xsl` file onto your Jenkins server(s) - I tend to use a Git Submodule to pull this repo into other projects

## Usage

1. When configuring your Job, enable the xUnit Plugin and select the *Custom Tool* option
2. Specify a pattern for ctest files to transform
3. Specify the path to the `ctest-to-junit.xsl` stylesheet
4. GO!

You might want to look at `run-test-and-save.sh` to see one way of prompting CTest to generate XML output and then moving it. You can execute that script as a build step from your build directory, and then your "pattern" will be `BUILDDIR/CTestResults.xml`.

## License

jenkins-ctest-plugin is covered under the MIT License. See LICENSE for more information.

Based on [WebUI-plugin](https://github.com/versionone/webui-plugin "WebUI Plugin")

