#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename);
use XML::LibXML;
use Getopt::Long;
use Pod::Usage;
use Regexp::DefaultFlags;
use Carp qw(croak carp);
use Data::Dumper;

use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';

use Utils qw(ensure_directory strip_extension ensure_readable_file);
use Xsltproc qw(apply_stylesheet);
use LocalDatabase;

my $xml_parser = XML::LibXML->new(); # for our XML processing needs

my $help = 0;
my $man = 0;
my $verbose = 0;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose)
    or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) if (scalar @ARGV != 1);

my $article_dir = $ARGV[0];

if (! ensure_directory ($article_dir)) {
    croak ('Error: the supplied directory ', $article_dir, ' does not exist.');
}

my $local_db = LocalDatabase->new (location => $article_dir);

my $prel_subdir = $local_db->get_prel_subdir ();
my $text_subdir = $local_db->get_text_subdir ();

my $article_basename = basename ($article_dir);

my %conditions
    = ('ex' => 'existence',
       'un' => 'uniqueness',
       'ch' => 'coherence',
       'cr' => 'correctness',
       'ab' => 'abstractness',
       're' => 'reflexivity',
       'ir' => 'irreflexivity',
       'sy' => 'symmetry',
       'as' => 'asymmetry',
       'cn' => 'connectedness',
       'in' => 'involutiveness',
       'pr' => 'projectivity',
       'id' => 'idempotence',
       'cm' => 'commutativity',
       'cp' => 'compatibility',
       'se' => 'sethood',
       # 'vc' => 'vconstructor',
       # 'rc' => 'rconstructor',
       # 'kc' => 'kconstructor',
       'kf' => 'kdefiniens',
       'mf' => 'mdefiniens',
       'rf' => 'rdefiniens',
       'vf' => 'vdefiniens',
       'kp' => 'kpattern',
       'rp' => 'rpattern',
       'mp' => 'mpattern',
       'vp' => 'vpattern',
       'dt' => 'deftheorem');

my %code_of_property = ();
foreach my $condition (keys %conditions) {
    my $property = $conditions{$condition};
    $code_of_property{$property} = $condition;
}

my %fragments_to_constructors = ();

sub print_properties_of_constructors_of_kind {
    my $kind = shift;

    my $kind_uc = uc $kind;

    my @dcos = $local_db->constructors_in_prel_of_kind ($kind);
    my @dcos_no_extension = map { strip_extension ($_) } @dcos;
    my @sorted_dcos
	= sort { fragment_less_than ($a, $b) } @dcos_no_extension;

    foreach my $i (1 .. scalar @sorted_dcos) {
	my $dco = $sorted_dcos[$i - 1];
	my $fragment_number = fragment_number ($dco);
	my $fragment = "${article_basename}:fragment:${fragment_number}";
	my $dco_path = "${prel_subdir}/${dco}.dco";
	if (ensure_readable_file ($dco_path)) {
	    my $dco_doc = eval { $xml_parser->parse_file ($dco_path) };
	    if (defined $dco_doc) {
		my $xpath = '/Constructors/Constructor[@kind = "' . $kind_uc . '"]/Properties/*';
		my @property_nodes = $dco_doc->findnodes ($xpath);
		foreach my $property_node (@property_nodes) {
		    my $property = lc $property_node->nodeName ();

		    if ($property eq 'antisymmetry') {
			$property = 'asymmetry';
		    }

		    my $item = "${article_basename}:${kind}constructor:${i}[${property}]";

		    my $property_code = $code_of_property{$property};

		    if (defined $property_code) {
			my $pseudo_fragment = "${fragment}[$property_code]";
			print $item, ' => ', $pseudo_fragment, "\n";
		    } else {
			croak ('Error: what is the short form of \'', $property, '\'?');
		    }


		}
	    } else {
		croak ('Error: the .dco file at ', $dco_path, ' is not a well-formed XML file.');
	    }
	} else {
	    croak ('Error: the dco file ', $dco, ' does not exist at the expected location (', $dco_path, ').');
	}
    }

}

sub print_functor_properties {
    print_properties_of_constructors_of_kind ('k');
}

sub print_relation_properties {
    print_properties_of_constructors_of_kind ('r');
}

sub print_mode_properties {
    print_properties_of_constructors_of_kind ('m');
}

sub print_attribute_properties {
    print_properties_of_constructors_of_kind ('v');
}

sub print_constructor_properties {
    print_functor_properties ();
    print_relation_properties ();
    print_mode_properties ();
    print_attribute_properties ();
}

sub print_constructors_of_kind {

    my $kind = shift;

    my @dcos = $local_db->constructors_in_prel_of_kind ($kind);
    my @dcos_no_extension = map { strip_extension ($_) } @dcos;
    my @sorted_dcos
	= sort { fragment_less_than ($a, $b) } @dcos_no_extension;

    foreach my $i (1 .. scalar @sorted_dcos) {
	my $fragment_of_ccluster = $sorted_dcos[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_ccluster);
	my $item = "${article_basename}:${kind}constructor:${i}";
	my $fragment = "${article_basename}:fragment:${fragment_number}";
	print $item, ' => ', $fragment, "\n";

	# Record that this fragment number generates this constructor.
	# Later, when computing correctness conditions, we'll need
	# this information.

	$fragments_to_constructors{$fragment_number} = $item;

    }

}

sub print_constructors {
    foreach my $kind ('g', 'k', 'l', 'm', 'r', 'u', 'v') {
	print_constructors_of_kind ($kind);
    }
}

sub print_patterns_of_kind {

    my $kind = shift;

    my @dnos = $local_db->patterns_in_prel_of_kind ($kind);
    my @dnos_no_extension = map { strip_extension ($_) } @dnos;
    my @sorted_dnos
	= sort { fragment_less_than ($a, $b) } @dnos_no_extension;

    foreach my $i (1 .. scalar @sorted_dnos) {
	my $fragment = $sorted_dnos[$i - 1];
	my $fragment_number = fragment_number ($fragment);
	print $article_basename, ':', $kind . 'pattern', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_notations {
    foreach my $kind ('g', 'j', 'k', 'l', 'm', 'r', 'u', 'v') {
	print_patterns_of_kind ($kind);
    }
}

sub print_definientia_of_kind {

    my $kind = shift;

    my @definientia = $local_db->definientia_in_prel_of_kind ($kind);
    my @definientia_no_extension = map { strip_extension ($_) } @definientia;
    my @sorted_definientia
	= sort { fragment_less_than ($a, $b) } @definientia_no_extension;

    foreach my $i (1 .. scalar @sorted_definientia) {
	my $fragment_of_ccluster = $sorted_definientia[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_ccluster);
	print $article_basename, ':', $kind . 'definiens', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_definientia {
    foreach my $kind ('k', 'm', 'r', 'v') {
	print_definientia_of_kind ($kind);
    }
}

sub print_correctness_conditions {

    my @properties_and_conditions = `find ${text_subdir} -maxdepth 1 -mindepth 1 -type f -name "ckb[0-9]*[a-z][a-z].miz" -exec basename {} .miz ';'`;
chomp @properties_and_conditions;

    foreach my $p_or_c_file (@properties_and_conditions) {
	if ($p_or_c_file =~ /ckb ([0-9]+) ([a-z]{2})/x) {
	    (my $fragment_number, my $condition_code) = ($1, $2);
	    if ($condition_code eq $code_of_property{'coherence'}
		    || $condition_code eq $code_of_property{'existence'}
			|| $condition_code eq $code_of_property{'uniqueness'}) {
		my $resolved_condition = $conditions{$condition_code};
		if (defined $resolved_condition) {
		    my $constructor = $fragments_to_constructors{$fragment_number};
		    if (defined $constructor) {
			my $property_key = "${constructor}[${resolved_condition}]";
			print $property_key, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, '[', $condition_code, ']', "\n";
		    } else {
			croak ('Error: we were unable to find out which constructor is generated by fragment ', $fragment_number, '; the fragment-to-constructor table looks like:', "\n", Dumper (%fragments_to_constructors));
		    }
		} else {
		    carp ('Warning: unable to make sense of the condition code \'', $condition_code, '\'.', "\n");
		}
	    }
	}
    }
}

sub print_deftheorems {

    my @theorems = $local_db->deftheorems_in_prel ();
    my @theorems_no_extension = map { strip_extension ($_) } @theorems;
    my @sorted_theorems = sort { fragment_less_than ($a, $b) } @theorems_no_extension;

    foreach my $i (1 .. scalar @sorted_theorems) {
	my $fragment_of_theorem = $sorted_theorems[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_theorem);
	print $article_basename, ':', 'deftheorem', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub fragment_number {
    my $fragment = shift;
    if ($fragment =~ /\A ckb ([0-9]+)/) {
	return $1;
    } else {
	croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.');
    }
}

sub fragment_less_than {
    my $a_num = fragment_number ($a);
    my $b_num = fragment_number ($b);
    if ($a_num < $b_num) {
	return -1;
    } elsif ($a_num == $b_num) {
	return 0;
    } else {
	return 1;
    }
}

sub print_schemes {

    my @schemes = $local_db->schemes_in_prel ();
    my @schemes_no_extension = map { strip_extension ($_) } @schemes;
    my @sorted_schemes = sort { fragment_less_than ($a, $b) } @schemes_no_extension;

    foreach my $i (1 .. scalar @sorted_schemes) {
	my $fragment_of_scheme = $sorted_schemes[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_scheme);
	print $article_basename, ':', 'scheme', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_reductions {

    my @reductions = $local_db->reductions_in_prel ();
    my @reductions_no_extension = map { strip_extension ($_) } @reductions;
    my @sorted_reductions
	= sort { fragment_less_than ($a, $b) } @reductions_no_extension;

    foreach my $i (1 .. scalar @sorted_reductions) {
	my $fragment_of_reduction = $sorted_reductions[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_reduction);
	print $article_basename, ':', 'reduction', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_conditional_clusters {

    my @cclusters = $local_db->conditional_clusters_in_prel ();
    my @cclusters_no_extension = map { strip_extension ($_) } @cclusters;
    my @sorted_cclusters
	= sort { fragment_less_than ($a, $b) } @cclusters_no_extension;

    foreach my $i (1 .. scalar @sorted_cclusters) {
	my $fragment_of_ccluster = $sorted_cclusters[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_ccluster);
	print $article_basename, ':', 'ccluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_functorial_clusters {

    my @fclusters = $local_db->functorial_clusters_in_prel ();
    my @fclusters_no_extension = map { strip_extension ($_) } @fclusters;
    my @sorted_fclusters
	= sort { fragment_less_than ($a, $b) } @fclusters_no_extension;

    foreach my $i (1 .. scalar @sorted_fclusters) {
	my $fragment_of_fcluster = $sorted_fclusters[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_fcluster);
	print $article_basename, ':', 'fcluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_existential_clusters {

    my @rclusters = $local_db->existential_clusters_in_prel ();
    my @rclusters_no_extension = map { strip_extension ($_) } @rclusters;
    my @sorted_rclusters
	= sort { fragment_less_than ($a, $b) } @rclusters_no_extension;

    foreach my $i (1 .. scalar @sorted_rclusters) {
	my $fragment_of_rcluster = $sorted_rclusters[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_rcluster);
	print $article_basename, ':', 'rcluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_clusters_of_kind {

    my $kind = shift;

    my @clusters = $local_db->clusters_in_prel_of_kind ($kind);
    my @clusters_no_extension = map { strip_extension ($_) } @clusters;
    my @sorted_clusters
	= sort { fragment_less_than ($a, $b) } @clusters_no_extension;

    foreach my $i (1 .. scalar @sorted_clusters) {
	my $fragment_of_cluster = $sorted_clusters[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_cluster);
	print $article_basename, ':', $kind, 'cluster', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

sub print_clusters {
    foreach my $kind ('c', 'f', 'r') {
	print_clusters_of_kind ($kind);
    }
}

sub print_identifications {

    my @dids = `find $prel_subdir -name "*.did" -exec basename {} .did ';' | sed -e 's/ckb//' | sort --numeric-sort`;
	chomp @dids;

    my %identifications = ();

    foreach my $i (1 .. scalar @dids) {
	my $did = $dids[$i - 1];
	my $did_path = "$prel_subdir/ckb${did}.did";
	my $identify_line = `grep '<Identify ' $did_path`;
	chomp $identify_line;
	$identify_line =~ m/ constrkind=\"(.)\"/;
	my $constrkind = $1;
	if (! defined $constrkind) {
	    print STDERR ('Error: we failed to extract a kind attribute from did Identify XML element', "\n", "\n", '  ', $identify_line, "\n");
	}
	my $constrkind_lc = lc $constrkind;
	my $num;
	if (defined $identifications{$constrkind}) {
	    $num = $identifications{$constrkind};
	    $identifications{$constrkind} = $num + 1;
	} else {
	    $num = 1;
	    $identifications{$constrkind} = 2;
	}
	print $article_basename, ':', $constrkind_lc, 'identification', ':', $num, ' => ', $article_basename, ':', 'fragment', ':', $did, "\n";
    }

}

sub print_theorems {

    my @theorems = $local_db->theorems_in_prel ();
    my @theorems_no_extension = map { strip_extension ($_) } @theorems;
    my @sorted_theorems = sort { fragment_less_than ($a, $b) } @theorems_no_extension;

    foreach my $i (1 .. scalar @sorted_theorems) {
	my $fragment_of_theorem = $sorted_theorems[$i - 1];
	my $fragment_number = fragment_number ($fragment_of_theorem);
	print $article_basename, ':', 'theorem', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
    }

}

# sub print_lemmas {

#     # Exported lemmas that were originally unexported
#     my @lemmas = $wsx_doc->findnodes ('Fragments/Text-Proper/Item[@kind = "Theorem-Item" and @promoted-lemma = "yes"]');

#     # DEBUG
#     # print 'Found ', scalar @lemmas, ' lemmas', "\n";

#     foreach my $lemma (@lemmas) {
# 	if ($lemma->exists ('@lemma-number')) {
# 	    my $lemma_number = $lemma->findvalue ('@lemma-number');
# 	    my $fragment_number = $lemma->findvalue ('count (preceding::Text-Proper) + 1');
# 	    print $article_basename, ':', 'lemma', ':', $lemma_number, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
# 	} else {
# 	    die 'Error: we did not find the lemma-number attribute for a promoted lemma!';
# 	}
#     }

#     # Unexportable lemmas that were originally unexported

#     my @unexportable_lemmas = $wsx_doc->findnodes ('Fragments/Text-Proper/Item[@kind = "Regular-Statement" and @exportable = "no" and not(following-sibling::*)]');

#     # DEBUG
#     # print 'Found ', scalar @unexportable_lemmas, ' unexportable lemmas', "\n";

#     foreach my $lemma (@unexportable_lemmas) {
# 	if ($lemma->exists ('@lemma-number')) {
# 	    my $lemma_number = $lemma->findvalue ('@lemma-number');
# 	    my $fragment_number = $lemma->findvalue ('count (preceding::Text-Proper) + 1');
# 	    print $article_basename, ':', 'lemma', ':', $lemma_number, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, "\n";
# 	} else {
# 	    die 'Error: we did not find the lemma-number attribute for an unpromoted lemma!', "\n";
# 	}
#     }

# }

print_correctness_conditions ();
print_constructor_properties ();
print_constructors ();
print_notations ();
print_definientia ();
print_deftheorems ();
print_schemes ();
print_reductions ();
print_clusters ();
print_identifications ();
print_theorems ();

#print_lemmas ();

__END__

=head1 MAP-CKBS

map-ckbs.pl - Resolve names of article-internal items ("CKBs")

=head1 SYNOPSIS

map-ckbs.pl [options] directory

Interpret the supplied directory as the result of itemizing a Mizar
article.  Print a mapping that shows how each of the items in the
itemized article is resolved into its absolute name.

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing as we're doing it.

=back

=head1 DESCRIPTION

B<map-ckbs.pl> will consult the given article directory to determine
how any "article-internal" items are resolved to their absolute names.

=head1 REQUIRED ARGUMENTS

It is necessary to supply a directory as the one and only argument of
this program.  The directory is supposed to be the result of itemizing
a Mizar article.  It should have the structure of a multi-article
Mizar development: there should be subdirectories 'prel', 'dict', and
'text'.

=head1 SEE ALSO

=over 8

=item F<itemized-article-dependencies.pl>

=item L<http://mizar.org/>

=back

=cut
