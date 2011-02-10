#!/bin/bash

article=$1;

cclusters=$article-needed-CCluster;
fclusters=$article-needed-FCluster;
rclusters=$article-needed-RCluster;
theorems=$article-needed-Theorem;
schemes=$article-needed-Scheme;
definientia=$article-needed-Definiens;
patterns=$article-needed-Pattern;
identifications=$article-needed-Identify;
constructors=$article-needed-Constructor;

ourgrep='grep --only-matching';

if [[ -e $cclusters ]]; then
    $ourgrep '<CCluster .*' $cclusters;
fi;

if [[ -e $fclusters ]]; then
    $ourgrep '<FCluster .*' $cclusters;
fi;

if [[ -e $rclusters ]]; then
    $ourgrep '<RCluster .*' $cclusters;
fi;

if [[ -e $theorems ]]; then
    $ourgrep '<JustifiedTheorem .*' $theorems;
fi;

if [[ -e $schemes ]]; then
    $ourgrep '<Scheme .*' $schemes;
fi;

if [[ -e $definientia ]]; then
    $ourgrep '<Definiens .*' $definientia;
fi;

if [[ -e $patterns ]]; then
    $ourgrep '<Pattern .*' $patterns;
fi;

if [[ -e $identifications ]]; then
    $ourgrep '<Identify .*' $identifications;
fi; 

if [[ -e $constructors ]]; then
    $ourgrep '<Constructor .*' $constructors;
fi;

exit 0;
