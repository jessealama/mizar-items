#!/bin/sh

article=$1;
listvoc $1 | head --lines=-2 | tac | head --lines=-4
exit $?
