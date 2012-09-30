#!/bin/sh

set -u; # enable some extra checks, such as using uninitialized variables

# Execute an arbitraty command in a specified directory, specified in
# the first argument.  Example:
#
#   exec-in-dir.sh /tmp verifier -q -s -l text/foo.miz
#
# This is a paradigmatic example.  I need to execute the transfer
# mizar tool, which exports the exportable content of a mizar article,
# into a specific directory, when working in an arbitrary directory.
# This program doesn't seem to have an option for specifying in what
# directory it will write its output; transfer (possibly) creates a
# directory 'prel' *in the current working directory* and then
# populates 'prel'.  It would be much nicer if transfer would accept
# such an option, so that it could be called when working in arbitrary
# directories.  I can't (or, more precisely, would rather not) change
# the working directory, so this wrapper script was created.

dir=$1
shift;
cd $dir;
exec $@;
exit $?;
