#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;
use Getopt::Long;
use Pod::Usage;

my $opt_mptp_table;

GetOptions (
    'mptp-table=s' => \$opt_mptp_table,
) or pod2usage (2);

if (scalar @ARGV > 0) {
    pod2usage (2);
}

while (defined (my $line = <STDIN>)) {
    chomp $line;
    (my $item, my @deps) = split ' ', $line;
    if ($item =~ /\A [tl] (\d+) [_] ([a-z0-9_]+) \z/) {
	(my $number, my $article) = ($1, $2);
	if (! -d $article) {
	    mkdir $article;
	}
	my $problem_path = "${article}/${item}";
	open (my $problem_fh, '>', $problem_path);
	if (defined $mptp_table
	close $problem_fh;

    }
}
