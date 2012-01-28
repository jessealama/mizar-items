#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename);
use XML::LibXML;
use Getopt::Long;
use Pod::Usage;
use Carp qw(croak carp);

use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';

use Utils qw(ensure_directory);
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
my %handled_constructor_properties = (); # to prevent printing duplicates

sub print_constructor_properties {

    my @dcos = `find $prel_subdir -name "*.dco" -exec basename {} .dco ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @dcos;

    my %constructors = ();

    foreach my $i (1 .. scalar @dcos) {
	my $dco = $dcos[$i - 1];
	my $dco_path = "$prel_subdir/ckb${dco}.dco";
	my $dco_doc = undef;
	if (! defined eval { $dco_doc = $xml_parser->parse_file ($dco_path) } ) {
	    print STDERR 'Error: the .dco file at ', $dco_path, ' is not well-formed XML.';
	}
	my @constructors = $dco_doc->findnodes ('Constructors/Constructor');
	if (scalar @constructors == 0) {
	    print STDERR ('Error: we found no Constructor nodes in ', $dco_path, '.');
	}
	foreach my $constructor (@constructors) {
	    if (! $constructor->exists ('@kind')) {
		print STDERR ('Error: we failed to extract a kind attribute from a Constructor XML element in ', $dco_path, '.', "\n");
		next;
	    }
	    my $kind = $constructor->findvalue ('@kind');
	    my $kind_lc = lc $kind;
	    my $num;
	    if (defined $constructors{$kind}) {
		$num = $constructors{$kind};
		$constructors{$kind} = $num + 1;
	    } else {
		$num = 1;
		$constructors{$kind} = 2;
	    }
	    my $constructor_key = "${article_basename}:${kind_lc}constructor:${num}";
	    my $fragment_number = $dco;

	    $fragments_to_constructors{$fragment_number} = $constructor_key;

	    my @properties = $constructor->findnodes ('Properties/*');
	    foreach my $property (@properties) {
		my $property_name = $property->nodeName ();
		my $property_name_lc = lc $property_name;

		# I wish this would be fixed
		if ($property_name_lc eq 'antisymmetry') {
		    $property_name_lc = 'asymmetry';
		}

		my $property_code = $code_of_property{$property_name_lc};

		my $property_key = "${article_basename}:${kind_lc}constructor:${num}[${property_name_lc}]";

		print $property_key, ' => ', $article_basename, ':', 'fragment', ':', $dco, '[', $property_code, ']', "\n";
		$handled_constructor_properties{$property_key} = 0;
	    }
	}
    }

}

sub print_constructors {
    my @dcos = `find $prel_subdir -name "*.dco" -exec basename {} .dco ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @dcos;

    my %constructors = ();
    my %handled_constructor_properties = (); # to prevent printing duplicates
    my %fragments_to_constructors = ();

    foreach my $i (1 .. scalar @dcos) {
	my $dco = $dcos[$i - 1];
	my $dco_path = "$prel_subdir/ckb${dco}.dco";
	my $dco_doc = undef;
	if (! eval { $dco_doc = $xml_parser->parse_file ($dco_path) } ) {
	    print STDERR 'Error: the .dco file at ', $dco_path, ' is not well-formed XML.';
	}
	my @constructors = $dco_doc->findnodes ('Constructors/Constructor');
	if (scalar @constructors == 0) {
	    print STDERR ('Error: we found no Constructor nodes in ', $dco_path, '.');
	}
	foreach my $constructor (@constructors) {
	    if (! $constructor->exists ('@kind')) {
		print STDERR ('Error: we failed to extract a kind attribute from a Constructor XML element in ', $dco_path, '.', "\n");
		next;
	    }
	    my $kind = $constructor->findvalue ('@kind');
	    my $kind_lc = lc $kind;
	    my $num;
	    if (defined $constructors{$kind}) {
		$num = $constructors{$kind};
		$constructors{$kind} = $num + 1;
	    } else {
		$num = 1;
		$constructors{$kind} = 2;
	    }
	    my $constructor_key = "${article_basename}:${kind_lc}constructor:${num}";
	    my $fragment_number = $dco;
	    print $article_basename, ':', $kind_lc, 'constructor', ':', $num, ' => ', $article_basename, ':', 'fragment', ':', $dco, '[', $kind_lc, 'c', ']', "\n";

	    $fragments_to_constructors{$fragment_number} = $constructor_key;

	    my @properties = $constructor->findnodes ('Properties/*');
	    foreach my $property (@properties) {
		my $property_name = $property->nodeName ();
		my $property_name_lc = lc $property_name;

		# I wish this would be fixed
		if ($property_name_lc eq 'antisymmetry') {
		    $property_name_lc = 'asymmetry';
		}

		my $property_code = $code_of_property{$property_name_lc};

		my $property_key = "${article_basename}:${kind_lc}constructor:${num}[${property_name_lc}]";

		print $property_key, ' => ', $article_basename, ':', 'fragment', ':', $dco, '[', $property_code, ']', "\n";
		$handled_constructor_properties{$property_key} = 0;
	    }
	}
    }
}

sub print_notations {

    my @dnos = `find $prel_subdir -name "*.dno" -exec basename {} .dno ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @dnos;

    my %patterns = ();
    my %fragments_to_patterns = ();

    foreach my $i (1 .. scalar @dnos) {
	my $dno = $dnos[$i - 1];
	my $dno_path = "$prel_subdir/ckb${dno}.dno";
	my $dno_doc = $xml_parser->parse_file ($dno_path);
	my @patterns = $dno_doc->findnodes ('Notations/Pattern');
	foreach my $pattern (@patterns) {
	    if ($pattern->exists ('@constrkind') &&
		    $pattern->exists ('@constrnr') &&
			$pattern->exists ('@aid')) {
		my $constrkind = $pattern->findvalue ('@constrkind');
		my $constrkind_lc = lc $constrkind;
		my $constrnr = $pattern->findvalue ('@constrnr');
		my $aid = $pattern->findvalue ('@aid');
		my $aid_lc = lc $aid;

		my $num = undef;
		if (defined $patterns{$constrkind}) {
		    $num = $patterns{$constrkind};
		    $patterns{$constrkind} = $num + 1;
		} else {
		    $num = 1;
		    $patterns{$constrkind} = 2;
		}

		my $pattern_key = "${article_basename}:${constrkind_lc}pattern:${num}";
		my $fragment_number = $dno;
		$fragments_to_patterns{$fragment_number} = $pattern_key;

		print $article_basename, ':', $constrkind_lc, 'pattern', ':', $num, ' => ', $article_basename, ':', 'fragment', ':', $dno, '[', $constrkind_lc, 'p', ']', "\n";

	    } else {
		croak ('Error: Unable to make sense of a Pattern node in an eno file that does not have a constrkind, constrnr, and aid attribute.');
	    }

	}

    }

}

sub print_definientia {

    my @defs = `find $prel_subdir -name "*.def" -exec basename {} .def ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @defs;

    my %definiens = ();
    my %fragments_to_definientia = ();

    foreach my $i (1 .. scalar @defs) {
	my $def = $defs[$i - 1];
	my $def_path = "$prel_subdir/ckb${def}.def";
	my $def_doc = undef;

	if (! eval { $def_doc = $xml_parser->parse_file ($def_path) } ) {
	    print STDERR 'Error: the .def file at ', $def_path, ' is not well-formed XML.';
	}
	my @definientia = $def_doc->findnodes ('Definientia/Definiens');
	if (scalar @definientia == 0) {
	    print STDERR ('Error: we found no Definiens nodes in ', $def_path, '.');
	}
	if (scalar @definientia > 1) {
	    print STDERR ('Error: we found multiple Definiens nodes in ', $def_path, '.');
	}
	my $definiens = $definientia[0];
	if (! $definiens->exists ('@constrkind')) {
	    print STDERR ('Error: we failed to extract a constrkind attribute from a Definiens XML element in ', $def_path, '.', "\n");
	    next;
	}
	my $constrkind = $definiens->findvalue ('@constrkind');
	my $constrkind_lc = lc $constrkind;
	my $num;
	    if (defined $definiens{$constrkind}) {
		$num = $definiens{$constrkind};
		$definiens{$constrkind} = $num + 1;
	    } else {
		$num = 1;
		$definiens{$constrkind} = 2;
	    }

	print $article_basename, ':', $constrkind_lc, 'definiens', ':', $num, ' => ', $article_basename, ':', 'fragment', ':', $def, '[', $constrkind_lc, 'f', ']', "\n";

	my $definiens_key = "${article_basename}:${constrkind_lc}definiens:${num}";
	$fragments_to_definientia{$def} = $definiens_key;
    }

}

    # Constructor properties and definition correctness conditions

    my @properties_and_conditions = `find ${text_subdir} -maxdepth 1 -mindepth 1 -type f -name "ckb[0-9]*[a-z][a-z].miz" -exec basename {} .miz ';'`;
chomp @properties_and_conditions;

foreach my $p_or_c_file (@properties_and_conditions) {
    if ($p_or_c_file =~ / \A ckb ([1-9][0-9]*) ([a-z]{2}) \z/x) {
	(my $fragment_number, my $condition_code) = ($1, $2);
	if ($condition_code !~ /\A . c \z/x
		&& $condition_code !~ /\A . p \z/x
		    && $condition_code !~ /\A . f \z/x
			&& $condition_code ne 'ab') {
	    my $resolved_condition = $conditions{$condition_code};
	    if (defined $resolved_condition) {
		if (defined $fragments_to_constructors{$fragment_number}) {
		    my $constructor = $fragments_to_constructors{$fragment_number};
		    my $property_key = "${constructor}[${resolved_condition}]";
		    if (! defined $handled_constructor_properties{$property_key}) {
			print $property_key, ' => ', $article_basename, ':', 'fragment', ':', $fragment_number, '[', $condition_code, ']', "\n";
		    }
		}
	    } else {
		carp ('Warning: unable to make sense of the condition code \'', $condition_code, '\'.', "\n");
	    }
	}
    }
}

sub print_deftheorems {

    my @thes = `grep --with-filename '<Theorem kind="D"' $prel_subdir/*.the | cut -f 1 -d ':' | parallel basename {} .the | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @thes;

    my %deftheorems = ();

    foreach my $i (1 .. scalar @thes) {
	my $the = $thes[$i - 1];
	print $article_basename, ':', 'deftheorem' , ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $the, '[dt]', "\n";
    }
}

sub print_schemes {

    # Schemes

    my @schs = `find $prel_subdir -name "*.sch" -exec basename {} .sch ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @schs;

    my %schemes = ();

    foreach my $i (1 .. scalar @schs) {
	my $sch = $schs[$i - 1];
	my $sch_path = "$prel_subdir/ckb${sch}.sch";
	print $article_basename, ':', 'scheme', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $sch, "\n";
    }

}

sub print_reductions {

    # Reductions

    my @drds = `find $prel_subdir -name "*.drd" -exec basename {} .drd ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @drds;

    my %reductions = ();

    foreach my $i (1 .. scalar @drds) {
	my $drd = $drds[$i - 1];
	my $drd_path = "$prel_subdir/ckb${drd}.drd";
	print $article_basename, ':', 'reduction', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $drd, "\n";
    }

}

sub print_clusters {

    # Clusters

    my @dcls = `find $prel_subdir -name "*.dcl" -exec basename {} .dcl ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @dcls;

    my %clusters = ();

    foreach my $i (1 .. scalar @dcls) {
	my $dcl = $dcls[$i - 1];
	my $dcl_path = "$prel_subdir/ckb${dcl}.dcl";
	my $cluster_line = `grep '<[CFR]Cluster ' $dcl_path`;
	chomp $cluster_line;
	$cluster_line =~ m/([CFR])Cluster /;
	my $kind = $1;
	if (! defined $kind) {
	    print STDERR ('Error: we failed to extract the cluster kind from the XML element', "\n", "\n", '  ', $cluster_line, "\n");
	}
	my $kind_lc = lc $kind;
	my $num;
	if (defined $clusters{$kind}) {
	    $num = $clusters{$kind};
	    $clusters{$kind} = $num + 1;
	} else {
	    $num = 1;
	    $clusters{$kind} = 2;
	}
	print $article_basename, ':', $kind_lc, 'cluster', ':', $num, ' => ', $article_basename, ':', 'fragment', ':', $dcl, "\n";
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
    my @thes = `find $prel_subdir -name "*.sch" -exec basename {} .the ';' | sed -e 's/ckb//' | sort --numeric-sort`;
    chomp @thes;

    foreach my $i (1 .. scalar @thes) {
	my $the = $thes[$i - 1];
	my $the_path = "$prel_subdir/ckb${the}.the";
	print $article_basename, ':', 'theorem', ':', $i, ' => ', $article_basename, ':', 'fragment', ':', $the, "\n";
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
