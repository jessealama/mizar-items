package Utils;

use base qw(Exporter);
use warnings;
use strict;

our @EXPORT_OK = qw(ensure_readable_file ensure_directory ensure_executable);

sub ensure_readable_file {
  my $file = shift;
  (-e $file && -r $file) ? return 1
                         : return 0;
}

sub ensure_executable {
    my $file = shift;
    if (ensure_readable_file ($file)) {
	if (-x $file) {
	    return 1;
	} else {
	    return 0;
	}
    } else {
	return 0;
    }
}

sub ensure_directory {
    my $dir = shift;
    (-d $dir) ? return 1 : return 0;
}

1;
