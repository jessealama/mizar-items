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
my %new_axioms = ();
open (my $fraenkel_fh, '<', $fraenkel_table);
while (defined (my $line = <$fraenkel_fh>)) {
    chomp $line;
    (my $item, my @deps) = split (' ', $line);
    if (scalar @deps == 0) {
	$new_axioms{$item} = 0;
    } else {
	foreach my $dep (@deps) {
	    $fraenkel{$item}{$dep} = 0;
	}
    }
}
close $fraenkel_fh;

my %printed = ();
open (my $table_fh, '<', $mptp_table);

my %mptp_dependencies = ();
my @items = ();
while (defined (my $line = <$table_fh>)) {
    chomp $line;
    (my $item, my @deps) = split (' ', $line);
    $mptp_dependencies{$item} = \@deps;
    push (@items, $item);
}

foreach my $item (@items) {
    if (defined $fraenkel{$item}) {
	my %fraenkel_for_item = %{$fraenkel{$item}};
	my @fraenkel_deps = keys %fraenkel_for_item;
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

    my @deps = @{$mptp_dependencies{$item}};

    # Check for undefined items in the Fraenkel table appearing as dependencies
    foreach my $dep (@deps) {
	if (defined $new_axioms{$dep}) {
	    if (! defined $mptp_dependencies{$dep}) {
		if (! defined $printed{$dep}) {
		    print $dep, "\n";
		    $printed{$dep} = 0;
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
