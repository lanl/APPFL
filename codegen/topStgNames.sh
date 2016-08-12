#!/usr/bin/env sh

# generate the list of top-level names in an STG file in Haskell [String] syntax

if [ -z "$1" ]
then
    print "Please provide /path/to/file.stg as an argument"
    exit 1
fi

awk 'BEGIN {FS = "[ =]"} /^[a-z_][a-zA-Z0-9_#]* *=/ {if ($1 != "data") {printf "\42%s\42, ", $1}}' "$1" |
    head -c-2 |
    fmt -w80 |
    xargs -d '\0' printf "[\n%s]\n"
