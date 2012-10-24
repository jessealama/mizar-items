#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;
use List::MoreUtils qw(firstidx);
use Getopt::Long;
use Pod::Usage;

my $opt_item_to_fragment_table = undef;
my $opt_mml_lar = undef;

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

GetOptions (
    'fragment-table=s' => \$opt_item_to_fragment_table,
    'lar=s' => \$opt_mml_lar,
) or pod2usage (2);

if (! defined $opt_item_to_fragment_table) {
    pod2usage (-exitval => 1,
	       -message => 'An item-to-fragment table must be supplied.');
}

if (! defined $opt_mml_lar) {
    pod2usage (-exitval => 1,
	       -message => 'Where is mml.lar?');
}

ensure_readable_file ($opt_item_to_fragment_table);
ensure_readable_file ($opt_mml_lar);

sub load_mml {
    my @articles = ();
    open (my $mml_lar_fh, '<', $opt_mml_lar)
	or die 'Unable to open an input filehandle for ', $opt_mml_lar, ': ', $!;
    while (defined (my $article = <$mml_lar_fh>)) {
	chomp $article;
	push (@articles, $article);
    }
    close $mml_lar_fh
	or die 'Unable to close the input filehandle for ', $opt_mml_lar, ': ', $!;
    return @articles;
}

my @mml = load_mml ();

warn 'MML loaded (', scalar @mml, ' articles)';

my %mml_order = ();

foreach my $i (1 .. scalar @mml) {
    my $article = $mml[$i - 1];
    $mml_order{$article} = $i;
}



sub load_item_to_fragment_table {

    my %table = ();

    open (my $table_fh, '<', $opt_item_to_fragment_table)
	or die 'Unable to open an input filehandle for ', $opt_item_to_fragment_table, ': ', $!;
    while (defined (my $line = <$table_fh>)) {
	chomp $line;
	(my $item, my $fragment) = split (' ', $line);
	$table{$item} = $fragment;
    }
    close $table_fh
	or die 'Unable to close the input filehandle for ', $opt_item_to_fragment_table, ': ', $!;

    return \%table;

}

my %item_to_fragment_table = %{load_item_to_fragment_table ()};

warn 'Item-to-fragment table loaded.';

my @condition_order = ('kp', 'ch');

my %less_than = ();
sub item_less_than {
    my $item_1 = shift;
    my $item_2 = shift;

    my $earlier = $less_than{$item_1}{$item_2};

    if (defined $earlier) {
	return $earlier;
    }

    my $answer = undef;

    if ($item_1 eq $item_2) {
	$answer = 0;
    } elsif ($item_1 =~ /\A rq [a-zA-Z]+ \z/) {
	$answer = 1;
    } elsif ($item_2 =~ /\A rq [a-zA-Z]+ \z/) {
	$answer = -1;
    } elsif ($item_1 =~ /\A ([a-z0-9_]+) [:] ([a-z]+) [:] ([0-9]+) ([[] ([a-z]+) []] )? \z/) {
	(my $item_1_article, my $item_1_kind, my $item_1_number)
	    = ($1, $2, $3);
	if ($item_2 =~ /\A ([a-z0-9_]+) [:] ([a-z]+) [:] ([0-9]+) ([[] ([a-z]+) []] )? \z/) {
	    (my $item_2_article, my $item_2_kind, my $item_2_number)
		= ($1, $2, $3);
	    my $article_1_position = $mml_order{$item_1_article};
	    my $article_2_position = $mml_order{$item_2_article};

	    if (! defined $article_1_position) {
		die 'The article (', $item_1_article, ') of ', $item_1, ' has no known position in mml.lar.';
	    }

	    if (! defined $article_2_position) {
		die 'The article (', $item_2_article, ') of ', $item_2, ' has no known position in mml.lar.';
	    }

	    my $item_1_stripped = "${item_1_article}:${item_1_kind}:${item_1_number}";
	    my $item_2_stripped = "${item_2_article}:${item_2_kind}:${item_2_number}";

	    if ($article_1_position < $article_2_position) {
		$answer = -1;
	    } elsif ($article_1_position > $article_2_position) {
		$answer = 1;
	    } else {
		my $fragment_1 = $item_to_fragment_table{$item_1};
		my $fragment_2 = $item_to_fragment_table{$item_2};

		if (defined $fragment_1) {
		    if (defined $fragment_2) {
			if ($fragment_1 =~ / [:] fragment [:] ([0-9]+) ([[] ([a-z]+) []]) ? \z/) {
			    (my $fragment_number_1, my $fragment_1_tag) = ($1, $3);
			    if ($fragment_2 =~ /[:] fragment [:] ([0-9]+) ([[] ([a-z]+) []]) ? \z/) {
				(my $fragment_number_2, my $fragment_2_tag) = ($1, $3);
				if ($fragment_number_1 < $fragment_number_2) {
				    $answer = -1;
				} elsif ($fragment_number_2 < $fragment_number_1) {
				    $answer = 1;
				} elsif ($item_1 =~ /[:] [0-9]+ \z/) {
				    if ($item_2 =~ /[:] [0-9]+ \z/) {
					if ($item_1 =~ /[:] deftheorem [:] /) {
					    $answer = 1;
					} elsif ($item_2 =~ /[:] deftheorem [:] /) {
					    $answer = -1;
					} else {
					    $answer = 0;
					}
				    } else {
					$answer = -1;
				    }
				} elsif ($item_2 =~ /[:] [0-9]+ \z/) {
				    $answer = 1;
				} else {
				    $answer = 0;
				}
			    }
			}
		    } else {
			if (defined $item_to_fragment_table{$item_2_stripped}) {
			    $answer = item_less_than ($item_1, $item_2_stripped);
			} else {
			    die 'Neither ', $item_2, ' nor its stripped form ', $item_2_stripped, ' could be found in the item-to-fragment table.';
			}
		    }
		} else {
		    if (defined $item_to_fragment_table{$item_1_stripped}) {
			$answer = item_less_than ($item_1_stripped, $item_2);
		    } else {
			die 'Neither ', $item_1, ' nor its stripped form ', $item_1_stripped, ' could be found in the item-to-fragment table.';
		    }
		}
	    }

	    if (! defined $answer) {
		die 'We computed no answer when comparing ', $item_1, ' with ', $item_2, '.';
	    }

	    $less_than{$item_1}{$item_2} = $answer;

	    if ($answer == -1) {
		$less_than{$item_2}{$item_1} = 1;
	    } elsif ($answer == 0) {
		$less_than{$item_2}{$item_1} = 0;
	    } elsif ($answer == 1) {
		$less_than{$item_2}{$item_1} = -1;
	    } else {
		die 'We computed an unrecognized answer, \'', $answer, '\' when computing whether', "\n", "\n", '  ', $item_1, "\n", "\n", 'is less than', "\n", "\n", '  ', $item_2, "\n";
	    }
	}
    }

    # warn 'answer: ', $answer;

    return $answer;

}

my %items = ();
my %dependencies = ();

while (defined (my $dependency_line = <STDIN>)) {
    (my $item, my @dep_items) = split (' ', $dependency_line);
    $items{$item} = 0;
    foreach my $dep_item (@dep_items) {
	$items{$dep_item} = 0;
	$dependencies{$item}{$dep_item} = 0;
    }
}

warn 'Dependency hashes installed.';

my @sorted_items = sort { item_less_than ($a, $b) } keys %items;

sub item_dependencies {
    my $item = shift;
    my @deps = ();
    foreach my $other_item (@sorted_items) {
	if (defined $dependencies{$item}{$other_item}) {
	    push (@deps, $other_item);
	}
    }
    return @deps;
}

warn 'Sorted.';

foreach my $item (@sorted_items) {
    my @deps = item_dependencies ($item);
    my @sorted_deps = sort { item_less_than ($a, $b) } @deps;
    print $item;
    foreach my $dep_item (@sorted_deps) {
	print ' ', $dep_item;
    }
    print "\n";
}

__END__
