#!/bin/bash

set -u; # enable some extra checks, such as using uninitialized variables

article=$1;
/usr/bin/time --format="%U" /mnt/sdb3/alama/mizar-items/verify-quietly.sh $article 2>&1;
#                                                                                           ^^^^
# /usr/bin/time reports its output on standard error.  If you drop the
# "2>&1", you will find that $verifier_time is equal to the empty
# string, and you'll *see* the output of the time command printed on
# the terminal where you're executing this script.
exit $?;
