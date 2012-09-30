#!/bin/bash

set -u; # enable some extra checks, such as using uninitialized variables

article=$1;

clusters=$article.ecl;
theorems=$article.eth;
schemes=$article.esh;
definientia=$article.dfs;
patterns=$article.eno;
identifications=$article.eid;
constructors=$article.atr.pruned;

ourgrep='grep --only-matching';

if [[ -e $clusters ]]; then
    $ourgrep '<[RCF]Cluster .*' $clusters;
fi

if [[ -e $theorems ]]; then
    $ourgrep '<Theorem .*' $theorems;
fi

if [[ -e $schemes ]]; then
    $ourgrep '<Scheme .*' $schemes;
fi

if [[ -e $definientia ]]; then
    $ourgrep '<Definiens .*' $definientia;
fi

if [[ -e $patterns ]]; then
    $ourgrep '<Pattern .*' $patterns;
fi

if [[ -e $identifications ]]; then
    $ourgrep '<Identify .*' $identifications;
fi

if [[ -e $constructors ]]; then
    $ourgrep '<Constructor .*' $constructors;
fi

exit 0;
