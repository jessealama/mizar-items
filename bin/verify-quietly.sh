#!/bin/bash

set -u; # enable some extra checks, such as using uninitialized variables

article=$1;
# accom -q -l -s $article > /dev/null 2> /dev/null;
if [[ $? -eq "0" ]]; then
    verifier -q -l -s $article > /dev/null 2> /dev/null;
    exit $?
else
    exit $?;
fi
