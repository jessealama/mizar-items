#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;

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

sub lemma_less_than {
    my $lemma_1 = shift;
    my $lemma_2 = shift;

    if ($lemma_1 =~ / [:] lemma [:] (\d+) \z/) {
	my $lemma_number_1 = $1;
	if ($lemma_2 =~ /[:] lemma [:] (\d+) \z/) {
	    my $lemma_number_2 = $1;
	    if ($lemma_number_1 < $lemma_number_2) {
		return -1;
	    } elsif ($lemma_number_2 < $lemma_number_1) {
		return 1;
	    } else {
		return 0;
	    }
	} else {
	    die 'Cannot make sense of \'', $lemma_2, '\'.';
	}
    } else {
	die 'Cannot make sense of \'', $lemma_1, '\'.';
    }
}

if (scalar @ARGV != 2) {
    print {*STDERR} 'Usage: lemmas ARTICLE-MAP MPTP-LEMMAS';
    exit 1;
}

my $article_map_file = $ARGV[0];
my $mptp_lemmas_file = $ARGV[1];

ensure_readable_file ($article_map_file);
ensure_readable_file ($mptp_lemmas_file);

# Sanity check: the number of lemmas in the article map equals the
# number of lemmas in the MPTP lemma file

my @mizar_items_lemmas
    = `egrep '^[a-z0-9_]+[:]lemma[:][0-9]+[ ]' $article_map_file | cut -f 1 -d ' '`;
my @mptp_lemmas = `cat $mptp_lemmas_file`;
chomp @mizar_items_lemmas;
chomp @mptp_lemmas;

@mizar_items_lemmas = sort { lemma_less_than ($a, $b) } @mizar_items_lemmas;

my $num_mizar_items_lemmas = scalar @mizar_items_lemmas;
my $num_mptp_lemmas = scalar @mptp_lemmas;

if ($num_mizar_items_lemmas != $num_mptp_lemmas) {
    print {*STDERR} 'Error: ', $article_map_file, ' has ', $num_mizar_items_lemmas, ' lemmas, but ', $mptp_lemmas_file, ' has ', $num_mptp_lemmas, ' lemmas.';
    exit 1;
}

foreach my $i (1 .. $num_mizar_items_lemmas) {
    my $mizar_item_lemma = $mizar_items_lemmas[$i - 1];
    my $mptp_lemma = $mptp_lemmas[$i - 1];
    if ($mizar_item_lemma =~ /\A ([a-z0-9_]+) [:] lemma [:] \d+ \z/) {
	my $article = $1;
	if ($mptp_lemma =~ /\A fof [(] [l] (\d+) [_] ${article} [,] /) {
	    my $mptp_lemma_number = $1;
	    print $mizar_item_lemma, ' ', 'l', $mptp_lemma_number, '_', $article, "\n";
	} else {
	    print 'Cannot make sense of MPTP lemma \'', $mptp_lemma, '\'.';
	}
    } else {
	die 'Cannot make sense of \'', $mizar_item_lemma, '\'.';
    }
    if ($mptp_lemma =~ /^fof/) {

    }
}
