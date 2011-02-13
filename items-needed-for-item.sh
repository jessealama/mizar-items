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

grep --silent '<JustifiedTheorem' $article.miz;
if [[ $? -eq "0" ]]; then
    if [[ -e $cclusters ]]; then
	$ourgrep '<CCluster .*' $cclusters;
    fi;
    
    if [[ -e $fclusters ]]; then
	$ourgrep '<FCluster .*' $fclusters;
    fi;
    
    if [[ -e $rclusters ]]; then
	$ourgrep '<RCluster .*' $rclusters;
    fi;
fi

exit 0;
