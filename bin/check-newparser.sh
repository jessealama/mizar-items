#!/bin/bash -

set -u; # enable some extra checks, such as using uninitialized variables

# Check whether newparser produces veriable articles
#
# No arguments are needed, but the script does use $MIZFILES.

pwd=`pwd`;

scratch_dir=/tmp;
timestamp=`date --date=now "+%Y-%m-%d-%H-%M-%S"`;

# sanity check
mizar_tools="accom newparser msplit mglue verifier";
for tool in $mizar_tools; do
    which $tool > /dev/null 2>&1;
    if [ ! $? = '0' ]; then
	echo "The needed mizar tool '$tool' is not in your PATH" 1>&2;
	exit 1;
    fi
done

workdir="$scratch_dir/$timestamp";
mkdir $workdir || (echo "Can't create directory '$base'" 1>&2; exit 1);
echo "Working in $workdir...";

for article in `cat $MIZFILES/mml.lar`; do
    article_dir="$workdir/$article";
    mml_article="$MIZFILES/mml/$article.miz";
    if [[ -e $mml_article && -f $mml_article && -r $mml_article ]]; then
	mkdir $article_dir \
	    || echo "Can't create directory '$article_dir'" 1>&2;
	cd $article_dir \
	    || echo "Can't change directory to '$article_dir'" 1>&2;
	cp $mml_article . \
	    || echo "Can't copy '$mml_article' to '$article_dir'" 1>&2;
	accom -q -s -l $article > /dev/null 2>&1;
	if [ $? = '0' ]; then
	    newparser -q -s -l $article > /dev/null 2>&1;
	    if [ $? = '0' ]; then
		msplit $article > /dev/null 2>&1;
		if [ $? = '0' ]; then
		    cp $article.wsm $article.tpr;
		    mglue $article > /dev/null 2>&1;
		    if [ $? = '0' ]; then
			verifier -q -s -l $article > /dev/null 2> /dev/null;
			if [ $? = '0' ]; then
			    echo "$article: ok";
			else
			    echo "$article: verifier error" 1>&2;
			fi
		    else
			echo "$article: mglue error" 1>&2;
		    fi
		else
		    echo "$article: msplit error" 1>&2;
		fi
	    else
		echo "$article: newparser error" 1>&2;
	    fi
	else
	    echo "$article: accom error" 1>&2;
	fi
    else
	echo "$article: doesn't exist under '$MIZFILES/mml' or is not a regular readable file" 1>&2;
    fi
done

cd $pwd;

exit 0;
