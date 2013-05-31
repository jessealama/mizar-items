xsltproc = (test -e $1 && test -e $2 && xsltproc --output $3 $2 $1) || (test -e $3 && rm -f $3; false)
