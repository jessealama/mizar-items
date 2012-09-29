#!/usr/bin/perl

use strict;
use warnings;

sub check_path {
    my $path = shift;
    if (! -e $path) {
	print {*STDERR} 'Error: ', $path, ' does not exist.';
	exit 1;
    }
    if (-d $path) {
	print {*STDERR} 'Error: ', $path, ' is a directory.';
	exit 1;
    }
    if (! -r $path) {
	print {*STDERR} 'Error: ', $path, ' is unreadable.';
	exit 1;
    }
    return;
}

sub load_table {
    my $table_path = shift;
    my %table = ();

    open (my $table_fh, '<', $table_path)
	or die 'Error: unable to open an input filehandle for ', $table_path, '.';
    while (defined (my $table_line = <$table_fh>)) {
	chomp $table_line;
	(my $item, my @deps) = split (' ', $table_line);
	$table{$item} = 0;
	foreach my $dep (@deps) {
	    $table{$dep} = 0;
	}
    }
    close $table_fh
	or die 'Error: unable to close the input filehandle for ', $table_path, '.';

    return \%table;
}

if (scalar @ARGV != 2) {
    print {*STDERR} 'Usage: compare-tables TABLE-1 TABLE-2';
    exit 1;
}

my $table_path_1 = $ARGV[0];
my $table_path_2 = $ARGV[1];

check_path ($table_path_1);
check_path ($table_path_2);

my %table_1 = %{load_table ($table_path_1)};
my %table_2 = %{load_table ($table_path_2)};

my @missing_from_table_1 = ();
my @missing_from_table_2 = ();

foreach my $item (keys %table_1) {
    if (! defined $table_2{$item}) {
	push (@missing_from_table_2, $item);
    }
}

foreach my $item (keys %table_2) {
    if (! defined $table_1{$item}) {
	push (@missing_from_table_1, $item);
    }
}

@missing_from_table_1 = sort @missing_from_table_1;
@missing_from_table_2 = sort @missing_from_table_2;

if (scalar @missing_from_table_1 == 0) {
    print 'Every item appearing in ', $table_path_2, ' appears in ', $table_path_1, '.', "\n";
} else {
    print 'Items missing from ', $table_path_1, ':', "\n";
    foreach my $item (@missing_from_table_1) {
	print $item, "\n";
    }
}

if (scalar @missing_from_table_2 == 0) {
    print 'Every item appearing in ', $table_path_1, ' appears in ', $table_path_2, '.', "\n";
} else {
    print 'Items missing from ', $table_path_2, ':', "\n";
    foreach my $item (@missing_from_table_2) {
	print $item, "\n";
    }
}

exit 0;
