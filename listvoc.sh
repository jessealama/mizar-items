#!/bin/sh

article=$1;
listvoc $1 | ghead --lines=-2 | tac | ghead --lines=-4
exit $?
