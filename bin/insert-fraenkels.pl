#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;
use Pod::Usage;

sub ensure_readable_file {
    my $path = shift;
    if (-d $path) {
	print {*STDERR} $path, ' is a directory, not a file.', "\n";
	exit 1;
    }
    if (! -e $path) {
	print {*STDERR} 'No such file \'', $path, '\'.', "\n";
	exit 1;
    }
    if (! -r $path) {
	print {*STDERR} $path, ' is unreadable.', "\n";
	exit 1;
    }
    return;
}

if (scalar @ARGV != 2) {
    pod2usage (-exitval => 2,
	       -message => 'Usage: insert-fraenkels TABLE FRAENKEL-TABLE');
}

my $mptp_table = $ARGV[0];
my $fraenkel_table = $ARGV[1];

ensure_readable_file ($mptp_table);
ensure_readable_file ($fraenkel_table);

# Load the Fraenkel table
my %fraenkel = ();
open (my $fraenkel_fh, '<', $fraenkel_table);
while (defined (my $line = <$fraenkel_fh>)) {
    chomp $line;
    (my $item, my @deps) = split (' ', $line);
    if (scalar @deps == 0) {
	warn 'no deps for ', $item;
	my %empy_table = ();
	$fraenkel{$item} = \%empy_table;
    } else {
	foreach my $dep (@deps) {
	    $fraenkel{$item}{$dep} = 0;
	}
    }
}
close $fraenkel_fh;

my %mptp_deps = ();
my %printed = ();
open (my $table_fh, '<', $mptp_table);
while (defined (my $line = <$table_fh>)) {
    chomp $line;
    (my $item, my @deps) = split (' ', $line);

    if (defined $fraenkel{$item}) {
	my %fraenkel_for_item = %{$fraenkel{$item}};
	my @fraenkel_deps = keys %fraenkel_for_item;
	if (scalar @fraenkel_deps == 0) {

	} else {
	    foreach my $fraenkel_dep (@fraenkel_deps) {
		if ($fraenkel_dep =~ /\A fraenkel_ /) {
		    if (! defined $printed{$fraenkel_dep}) {
			print $fraenkel_dep;
			$printed{$fraenkel_dep} = 0;
			if (defined $fraenkel{$fraenkel_dep}) {
			    my %fraenkel_deps_for_fraenkel = %{$fraenkel{$fraenkel_dep}};
			    my @fraenkel_deps_deps = keys %fraenkel_deps_for_fraenkel;
			    foreach my $dep (@fraenkel_deps_deps) {
				print ' ', $dep;
			    }
			}
			print "\n";
		    }
		}
	    }
	}
    }

    if (! defined $printed{$item}) {
	print $item;
	$printed{$item} = 0;
	foreach my $dep (@deps) {
	    print ' ', $dep;
	}
	if (defined $fraenkel{$item}) {
	    my %fraenkel_deps = %{$fraenkel{$item}};
	    my @more_deps = keys %fraenkel_deps;
	    foreach my $dep (@more_deps) {
		print ' ', $dep;
	    }
	}
	print "\n";
    }
}
