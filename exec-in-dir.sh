#!/bin/sh

dir=$1
shift;
cd $dir;
exec $@;
exit $?;
