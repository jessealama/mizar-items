#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;

my %mptp_for_item = ();

my %mptp_lemmas = ();

sub load_lemmas {
    my $lemma_file = 'lemmas';


    if (! -e $lemma_file) {
	die 'The lemma file does not exist in the expected location \'', $lemma_file, '\'.';
    }

    if (-d $lemma_file) {
	die 'The lemma file at \'', $lemma_file, '\' is actually a directory.';
    }

    if (! -r $lemma_file) {
	die 'The lemma file at \'', $lemma_file, '\' is unreadable.';
    }

    open (my $lemma_fh, '<', $lemma_file)
	or die 'Unable to open an input filehandle for \'', $lemma_file, '\': ', $!;
    while (defined (my $lemma_line = <$lemma_fh>)) {
	chomp $lemma_line;
	if ($lemma_line =~ / \A ([^ ]+) [ ] ([^ ]+) \z/) {
	    (my $item, my $lemma) = ($1, $2);
	    $mptp_lemmas{$lemma} = $item;
	} else {
	    die 'Cannot parse lemma line \'', $lemma_line, '\'.';
	}
    }
    close $lemma_fh
	or die 'Error closing the input filehandle for \'', $lemma_file, '\: ', $!;

    return;

}

sub to_mptp {
    my $item = shift;
    my $already_resolved = $mptp_for_item{$item};

    if (defined $already_resolved) {
	return $already_resolved;
    }

    if ($item =~ / \A ([a-z_0-9]+) [:] ([a-z]+) [:] ([0-9]+) ([[] ([a-z]+) []])? \z /) {
	(my $article, my $kind, my $number, my $property) = ($1, $2, $3, $5);

	my $answer = undef;

	if (defined $property) {
	    if ($kind =~ /\A ([lmvrk])constructor \z/) {
		if ($property eq 'asymmetry') {
		    $answer = "antisymmetry_${1}${number}_${article}";
		} else {
		    $answer = "${property}_${1}${number}_${article}";
		}
	    } else {
		die 'Error: unhandled item ', $item;
	    }

	} else {

	    if ($kind eq 'theorem') {
		$answer = "t${number}_${article}";
	    } elsif ($kind eq 'lemma') {
		if (defined $mptp_lemmas{$item}) {
		    $answer = $mptp_lemmas{$item};
		} else {
		    die 'Unknown lemma ', $item;
		}
	    } elsif ($kind eq 'scheme') {
		$answer = "s${number}_${article}";
	    } elsif ($kind =~ /([kmu])constructor/) {
		$answer = "dt_${1}${number}_${article}";
	    } elsif ($kind =~ /[kmrv]definiens/) {
		$answer = "d${number}_${article}";
	    } elsif ($kind eq 'kidentification') {
		$answer = "ie${number}_${article}";
	    } elsif ($kind eq 'deftheorem') {
		$answer = "d${number}_${article}";
	    } elsif ($kind =~ /([cfr])cluster/) {
		$answer = "${1}c${number}_${article}";
	    } elsif ($kind eq 'reduction') {
		$answer = "rd${number}_${article}";
	    } else {
		die 'Error: unhandled item ', $item;
	    }

	}

	$mptp_for_item{$item} = $answer;

	return $answer;

    } else {
	die 'Error: cannot make sense of the item ', $item;
    }

}

my %handled_table = ();


sub handled {
    my $item = shift;

    my $known_answer = $handled_table{$item};
    if (defined $known_answer) {
	return $known_answer;
    }

    my $answer = undef;

    if ($item =~ / \A ([a-z_0-9]+) [:] ([a-z]+) [:] ([0-9]+) ([[] ([a-z]+) []] )? \z /) {
	(my $article, my $kind, my $number, my $property) = ($1, $2, $3, $5);
	if (defined $property) {
	    if ($property eq 'coherence') {
		$answer = 0;
	    } elsif ($property eq 'existence') {
		$answer = 0;
	    } elsif ($property eq 'uniqueness') {
		$answer = 0;
	    } elsif ($property eq 'compatibility') {
		$answer = 0;
	    } elsif ($property eq 'sethood') {
		$answer = 0;
	    } else {
		$answer = 1;
	    }
	} else {
	    if ($kind =~ /[glrv]constructor/) {
		$answer = 0;
	    } elsif ($kind =~ /[gjklmruv]pattern/) {
		$answer = 0;
	    } elsif ($kind eq 'lemma') {
		$answer = 0;
	    } else {
		$answer = 1;
	    }
	}
    } else {
	die 'Error: cannot make sense of the item ', $item;
    }

    $handled_table{$item} = $answer;

    return $answer;

}

# load_lemmas ();

foreach my $line (<STDIN>) {
    chomp $line;
    (my $item, my @deps) = split (' ', $line);
    if (handled ($item)) {
	print to_mptp ($item);
	foreach my $dep_item (@deps) {
	    if (handled ($dep_item)) {
		print ' ', to_mptp ($dep_item);
	    }
	}
	print "\n";
    }
}
