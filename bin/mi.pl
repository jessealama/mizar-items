#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename);
use File::Path qw(rmtree);
use Getopt::Long;
use Pod::Usage;
use File::Temp qw(tempfile);
use Carp qw(croak carp confess);
use XML::LibXML;
use List::MoreUtils qw(none first_index);
use Regexp::DefaultFlags;
use IPC::Run qw(run start);
use charnames qw( :full ); # for referring to characters in regular expressions
use Readonly;
use FindBin qw($RealBin);
use lib "$RealBin/../src/perl/";
use Data::Dumper;
use List::MoreUtils qw(any);

use Utils qw(parse_xml_file);

Readonly my $LF => "\N{LF}";
Readonly my $PREFIX_LC => lc 'ckb';
Readonly my $PREFIX_UC => uc $PREFIX_LC;

my $article_dir = $ARGV[0];

ensure_local_db_structure ($article_dir);

my $prel_dir = "${article_dir}/prel";
my $text_dir = "${article_dir}/text";

# Set up an XML parser that we might use
my $xml_parser = XML::LibXML->new ();

my $script_home = $RealBin;
my $verbose = 0;
my $man = 0;
my $help = 0;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose)
    or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

sub ensure_sensible_commandline {
    if (scalar @ARGV != 1) {
        pod2usage (1);
    }
    my $article_dir = $ARGV[0];
    if (! -d $article_dir) {
        confess 'Error: ', $article_dir, ' is not a directory.';
    }
    return;
}

sub ensure_sensible_environment {
    my $mizfiles = $ENV{'MIZFILES'};
    if (! defined $mizfiles) {
        confess 'Error: MIZFILES is not set.';
    }
    return 1;
}

ensure_sensible_commandline ();
ensure_sensible_environment ();

sub ensure_local_db_structure
{
    my $dir = shift;
    my $prel = "${dir}/prel";
    my $text = "${dir}/text";
    if (! -d $prel) {
        confess 'Error: the prel subdirectory is missing from ', $dir, '.';
    }
    if (! -d $text) {
        confess 'Error: the text subdirectory is missing from ', $dir, '.';
    }
    return;
}

sub fragment_number {
    my $fragment = shift;
    if ($fragment =~ / ${PREFIX_LC} (\d+) \z /) {
	return $1;
    } elsif ($fragment =~ / ${PREFIX_LC} (\d+) [:] /) {
        return $1;
    } elsif ($fragment =~ / ${PREFIX_LC} (\d+) [.] /) {
        return $1;
    } elsif ($fragment =~ / : fragment : (\d+) \z /) {
	return $1;
    } else {
	croak ('Error: unable to make sense of the fragment \'', $fragment, '\'.');
    }
}

sub fragment_less_than {
    my $a = shift;
    my $b = shift;
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

sub constructors_in_file {
    my $file = shift;
    my $doc = parse_xml_file ($file);
    my $root = $doc->documentElement ();
    my @constructors = constructors_under_node ($root);
    return @constructors;
}

sub get_attribute {
    my $node = shift;
    if (! defined $node) {
        confess 'First argument (an XML node) missing.';
    }
    my $attr = shift;
    if (! defined $attr) {
        confess 'Second argument (an attribute name) missing.';
    }
    if ($node->hasAttribute ($attr)) {
        return $node->getAttribute ($attr);
    } else {
        my $name = $node->nodeName ();
        confess '', $name, ' node lacks a(n) ', $attr, ' attribute.';
    }
}

sub get_aid_attribute {
    my $node = shift;
    return get_attribute ($node, 'aid');
}

sub get_nr_attribute {
    my $node = shift;
    return get_attribute ($node, 'nr');
}

sub get_kind_attribute {
    my $node = shift;
    return get_attribute ($node, 'kind');
}

sub get_vid_attribute {
    my $node = shift;
    return get_attribute ($node, 'vid');
}

sub article_dependencies {
    my $article_path = shift;

    my $article_basename = basename ($article_path, '.miz');
    my $article_xml_path = "${text_dir}/${article_basename}.xml";

    my %deps = ();
    my @article_files = glob "${text_dir}/${article_basename}.*";

    foreach my $file (@article_files) {
        if ($file =~ / [.] miz \z /) {
            next;
        } elsif ($file =~ / [.] aco \z /) {
            next;
        } elsif ($file =~ / [.] ano \z /) {
            next;
        } elsif ($file =~ / [.] atr \z /) {
            next;
        } elsif ($file =~ / [.] atr [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] cho \z /) {
            next;
        } elsif ($file =~ / [.] dcl \z /) {
            next;
        } elsif ($file =~ / [.] ecl \z /) {
            my $dco_doc = parse_xml_file ($file);
            my $dco_root = $dco_doc->documentElement ();
            my @clusters = $dco_root->findnodes ('descendant::CCluster | descendant::FCluster | descendant::RCluster');
            foreach my $cluster (@clusters) {
                my $cluster_kind = undef;
                if ($cluster->exists ('self::CCluster')) {
                    $cluster_kind = 'c';
                } elsif ($cluster->exists ('self::FCluster')) {
                    $cluster_kind = 'f';
                } elsif ($cluster->exists ('self::RCluster')) {
                    $cluster_kind = 'r';
                } else {
                    confess 'What kind of cluster is contained in ', $file, '?';
                }
                my $aid = get_attribute ($cluster, 'aid');
                my $nr = get_attribute ($cluster, 'nr');
                my $aid_lc = lc $aid;
                my $item = "${aid_lc}:${cluster_kind}cluster:${nr}";
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] ecl1 \z / ) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] ecl [.] orig \z / ) {
            next;
        } elsif ($file =~ / [.] dco \z /) {
            next;
        } elsif ($file =~ / [.] dct \z /) {
            next;
        } elsif ($file =~ / [.] dcx \z /) {
            next;
        } elsif ($file =~ / [.] def \z /) {
            next;
        } elsif ($file =~ / [.] dfe \z /) {
            next;
        } elsif ($file =~ / [.] dfe1 \z /) {
            my $dfs_doc = parse_xml_file ($file);
            my $dfs_root = $dfs_doc->documentElement ();
            my @definientia = $dfs_root->findnodes ('descendant::Definiens');
            foreach my $definiens (@definientia) {
                my $definiens_kind = get_attribute ($definiens, 'constrkind');
                my $aid = get_attribute ($definiens, 'aid');
                my $constraid = get_attribute ($definiens, 'constraid');
                my $nr = get_attribute ($definiens, 'absconstrnr');
                if ($aid ne $constraid) {
                    $nr = get_attribute ($definiens, 'defnr');
                }
                my $aid_lc = lc $aid;
                my $kind_lc = lc $definiens_kind;
                my $item = "${aid_lc}:${kind_lc}definiens:${nr}";
                $deps{$item} = 0;
            }
            # Record constructor dependencies
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] dfr \z /) {
            next;
        } elsif ($file =~ / [.] dfs \z /) {
            next;
        } elsif ($file =~ / [.] dfs [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] dfs1 \z /) {
            my $dfs_doc = parse_xml_file ($file);
            my $dfs_root = $dfs_doc->documentElement ();
            my @definientia = $dfs_root->findnodes ('descendant::Definiens');
            foreach my $definiens (@definientia) {
                my $definiens_kind = $definiens->getAttribute ('constrkind');
                my $aid = $definiens->getAttribute ('aid');
                my $constraid = $definiens->getAttribute ('constraid');
                my $nr = $definiens->getAttribute ('absconstrnr');
                my $defnr = $definiens->getAttribute ('defnr');
                if (! defined $definiens_kind) {
                    confess 'Unable to make sense of a definiens in ', $file;
                }
                if (! defined $aid) {
                    confess 'Definiens node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Definiens node in ', $file, ' lacks an nr attribute.';
                }
                if (! defined $defnr) {
                    confess 'Definiens node in ', $file, ' lacks a defnr attribute.';
                }
                if (! defined $constraid) {
                    confess 'Definiens node in ', $file, ' lacks a constraid attribute.';
                }
                my $aid_lc = lc $aid;
                my $kind_lc = lc $definiens_kind;
                if ($constraid eq $aid) {
                    my $item = "${aid_lc}:${kind_lc}definiens:${nr}";
                    $deps{$item} = 0;
                } else {
                    my $mizfiles = $ENV{'MIZFILES'};
                    if (! defined $mizfiles) {
                        confess 'MIZFILES environment variable not defined.';
                    }
                    my $miztmp_dir = "${mizfiles}/miztmp";
                    if (! -d $miztmp_dir) {
                        confess 'miztmp dir missing under ', $mizfiles;
                    }
                    my $item_xml = "${miztmp_dir}/${aid_lc}.xml1";
                    if (! -e $item_xml) {
                        confess 'Absolutized XML for ', $aid_lc, ' missing under ', $miztmp_dir;
                    }
                    my $item_xml_doc = parse_xml_file ($item_xml);
                    my $item_xml_root = $item_xml_doc->documentElement ();
                    my $definiens_xpath = "descendant::Definiens[\@constrkind = \"${definiens_kind}\" and \@defnr = \"${defnr}\"]";
                    (my $definiens_node) = $item_xml_root->findnodes ($definiens_xpath);
                    if (! defined $definiens_node) {
                        confess 'No Definiens node found in ', $item_xml, ' matching', $LF, $LF, '  ', $definiens_xpath;
                    }
                    my $count_xpath = "count (preceding::Definiens[\@constrkind = \"${definiens_kind}\"]) + 1";
                    my $count = $definiens_node->findvalue ($count_xpath);
                    my $item = "${aid_lc}:${kind_lc}definiens:${count}";
                    $deps{$item} = 0;
                }
            }
            # Record constructor dependencies
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] dfx \z /) {
            next;
        } elsif ($file =~ / [.] dfx1 \z /) {
            my $dfs_doc = parse_xml_file ($file);
            my $dfs_root = $dfs_doc->documentElement ();
            my @definientia = $dfs_root->findnodes ('descendant::Definiens');
            foreach my $definiens (@definientia) {
                my $definiens_kind = $definiens->getAttribute ('constrkind');
                my $aid = $definiens->getAttribute ('aid');
                my $constraid = $definiens->getAttribute ('constraid');
                my $nr = $definiens->getAttribute ('absconstrnr');
                my $defnr = $definiens->getAttribute ('defnr');
                if (! defined $definiens_kind) {
                    confess 'Unable to make sense of a definiens in ', $file;
                }
                if (! defined $aid) {
                    confess 'Definiens node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $constraid) {
                    confess 'Definiens node in ', $file, ' lacks a constraid attribute.';
                }
                if (! defined $nr) {
                    confess 'Definiens node in ', $file, ' lacks an nr attribute.';
                }
                if (! defined $defnr) {
                    confess 'Definiens node in ', $file, ' lacks a defnr attribute.';
                }
                my $aid_lc = lc $aid;
                my $kind_lc = lc $definiens_kind;
                if ($aid eq $constraid) {
                    my $item = "${aid_lc}:${kind_lc}definiens:${nr}";
                    $deps{$item} = 0;
                } else {
                    my $mizfiles = $ENV{'MIZFILES'};
                    if (! defined $mizfiles) {
                        confess 'MIZFILES environment variable not defined.';
                    }
                    my $miztmp_dir = "${mizfiles}/miztmp";
                    if (! -d $miztmp_dir) {
                        confess 'miztmp dir missing under ', $mizfiles;
                    }
                    my $item_xml = "${miztmp_dir}/${aid_lc}.xml1";
                    if (! -e $item_xml) {
                        confess 'Absolutized XML for ', $aid_lc, ' missing under ', $miztmp_dir;
                    }
                    my $item_xml_doc = parse_xml_file ($item_xml);
                    my $item_xml_root = $item_xml_doc->documentElement ();
                    my $definiens_xpath = "descendant::Definiens[\@constrkind = \"${definiens_kind}\" and \@defnr = \"${defnr}\"]";
                    (my $definiens_node) = $item_xml_root->findnodes ($definiens_xpath);
                    if (! defined $definiens_node) {
                        confess 'No Definiens node found in ', $item_xml, ' matching', $LF, $LF, '  ', $definiens_xpath;
                    }
                    my $count_xpath = "count (preceding::Definiens[\@constrkind = \"${definiens_kind}\"]) + 1";
                    my $count = $definiens_node->findvalue ($count_xpath);
                    my $item = "${aid_lc}:${kind_lc}definiens:${count}";
                    $deps{$item} = 0;
                }
            }
            # Record constructor dependencies
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] dno \z /) {
            next;
        } elsif ($file =~ / [.] dpr \z /) {
            next;
        } elsif ($file =~ / [.] drd \z /) {
            next;
        } elsif ($file =~ / [.] eid \z /) {
            my $did_doc = parse_xml_file ($file);
            my $did_root = $did_doc->documentElement ();
            my @identifications = $did_root->findnodes ('descendant::Identify');
            foreach my $identification (@identifications) {
                my $identification_kind = $identification->getAttribute ('constrkind');
                my $aid = $identification->getAttribute ('aid');
                my $nr = $identification->getAttribute ('nr');
                if (! defined $identification_kind) {
                    confess 'Unable to make sense of an identification in ', $file;
                }
                if (! defined $aid) {
                    confess 'Identification node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Identification node in ', $file, ' lacks an nr attribute.';
                }
                my $aid_lc = lc $aid;
                my $kind_lc = lc $identification_kind;
                my $item = "${aid_lc}:${kind_lc}identification:${nr}";
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] eid1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] eid [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] eno \z /) {
            my $eno_doc = parse_xml_file ($file);
            my $eno_root = $eno_doc->documentElement ();
            my @patterns = $eno_root->findnodes ('descendant::Pattern');
            foreach my $pattern (@patterns) {
                my $pattern_kind = $pattern->getAttribute ('kind');
                my $aid = $pattern->getAttribute ('aid');
                my $nr = $pattern->getAttribute ('nr');
                if (! defined $pattern_kind) {
                    confess 'Unable to make sense of a pattern in ', $file;
                }
                if (! defined $aid) {
                    confess 'Pattern node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Pattern node in ', $file, ' lacks an nr attribute.';
                }
                my $aid_lc = lc $aid;
                my $kind_lc = lc $pattern_kind;
                my $item = "${aid_lc}:${kind_lc}pattern:${nr}";
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] eno1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] eno [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] epr \z / ) {
            my $epr_doc = parse_xml_file ($file);
            my $epr_root = $epr_doc->documentElement ();
            my @properties = $epr_root->findnodes ('descendant::Property');
            foreach my $property (@properties) {
                my $aid = $property->getAttribute ('aid');
                my $nr = $property->getAttribute ('nr');
                my $x = $property->getAttribute ('x');
                if (! defined $aid) {
                    confess 'Property node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Property node in ', $file, ' lacks an nr attribute.';
                }
                if (! defined $x) {
                    confess 'Property node in ', $file, ' lacks an x attribute.';
                }
                # special case: only sethood (x = 12) is handled
                if ($x != 12) {
                    confess 'Error: we assume that the value of the x attribute in a Property node is always \'12\'.';
                }
                my $aid_lc = lc $aid;
                my $item = "${aid_lc}:sethood:${nr}";
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] epr1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] epr [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] erd \z /) {
            my $erd_doc = parse_xml_file ($file);
            my $erd_root = $erd_doc->documentElement ();
            my @reductions = $erd_root->findnodes ('descendant::Reduction');
            foreach my $reduction (@reductions) {
                my $aid = $reduction->getAttribute ('aid');
                my $nr = $reduction->getAttribute ('nr');
                if (! defined $aid) {
                    confess 'Reduction node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Reduction node in ', $file, ' lacks an nr attribute.';
                }
                my $aid_lc = lc $aid;
                my $item = "${aid_lc}:reduction:${nr}";
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] erd1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] erd [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] ere \z /) {
            my @ere_lines = `cat $file`;
            chomp @ere_lines;
            foreach my $i (0 .. scalar @ere_lines - 1) {
                my $ere_line = $ere_lines[$i];
                if ($ere_line !~ /\A 0 [\N{SPACE}]* \z / ) {
                    my $requirement_item = "requirement:${i}";
                    $deps{$requirement_item} = 0;
                }
            }
        } elsif ($file =~ / [.] err \z /) {
            next;
        } elsif ($file =~ / [.] esh \z /) {
            my $esh_doc = parse_xml_file ($file);
            my $esh_root = $esh_doc->documentElement ();
            my @schemes = $esh_root->findnodes ('descendant::Scheme');
            foreach my $scheme (@schemes) {
                my $aid = $scheme->getAttribute ('aid');
                my $nr = $scheme->getAttribute ('nr');
                if (! defined $aid) {
                    confess 'Scheme node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Scheme node in ', $file, ' lacks an nr attribute.';
                }
                my $aid_lc = lc $aid;
                my $item = "${aid_lc}:scheme:${nr}";
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] esh [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] esh1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] eth \z /) {
            my $eth_doc = parse_xml_file ($file);
            my $eth_root = $eth_doc->documentElement ();
            my @theorems = $eth_root->findnodes ('descendant::Theorem');
            foreach my $theorem (@theorems) {
                my $kind = $theorem->getAttribute ('kind');
                my $aid = $theorem->getAttribute ('aid');
                my $nr = $theorem->getAttribute ('nr');
                if (! defined $kind) {
                    confess 'Theorem node in ', $file, ' lacks a kind attribute.';
                }
                if (! defined $aid) {
                    confess 'Theorem node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Theorem node in ', $file, ' lacks an nr attribute.';
                }
                my $aid_lc = lc $aid;
                my $item;
                if ($kind eq 'T') {
                    $item = "${aid_lc}:theorem:${nr}";
                } elsif ($kind eq 'D') {
                    $item = "${aid_lc}:deftheorem:${nr}";
                } else {
                    confess 'Error: we expect Theorem elements to carry a kind attribute whose value is either \'T\' or \'D\', not \'', $kind, '\'.';
                }
                $deps{$item} = 0;
            }
        } elsif ($file =~ / [.] eth1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [.] eth [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] evd \z /) {
            next;
        } elsif ($file =~ / [.] evl \z /) {
            next;
        } elsif ($file =~ / [.] fil \z /) {
            next;
        } elsif ($file =~ / [.] frm \z /) {
            next;
        } elsif ($file =~ / [.] frx \z /) {
            next;
        } elsif ($file =~ / [.] idx \z /) {
            next;
        } elsif ($file =~ / [.] log \z /) {
            next;
        } elsif ($file =~ / [.] msx \z /) {
            next;
        } elsif ($file =~ / [.] nol \z /) {
            next;
        } elsif ($file =~ / [.] par \z /) {
            next;
        } elsif ($file =~ / [.] prf \z /) {
            next;
        } elsif ($file =~ / [.] ref \z /) {
            next;
        } elsif ($file =~ / [.] sch \z /) {
            next;
        } elsif ($file =~ / [.] sgl \z /) {
            next;
        } elsif ($file =~ / [.] the \z /) {
            next;
        } elsif ($file =~ / [.] tpr \z /) {
            next;
        } elsif ($file =~ / [.] vcl \z /) {
            next;
        } elsif ($file =~ / [.] wsx \z /) {
            next;
        } elsif ($file =~ / [.] xml \z /) {
            next;
        } elsif ($file =~ / [.] xml [.] trimmed \z /) {
            next;
        } elsif ($file =~ / [.] xml [.] orig \z /) {
            next;
        } elsif ($file =~ / [.] xml [.] orig [.] trimmed \z /) {
            next;
        } elsif ($file =~ / [.] xml [.] reference \z /) {
            next;
        } elsif ($file =~ / [.] xml [.] stripped \z /) {
            next;
        } elsif ($file =~ / [.] xml [.] temp [.] stripped \z /) {
            next;
        } elsif ($file =~ / [.] xml1 \z /) {
            my @constructors = constructors_in_file ($file);
            foreach my $constructor (@constructors) {
                $deps{$constructor} = 0;
            }
        } elsif ($file =~ / [~] \z /) { # emacs backup file
            next;
        } else {
            confess 'Error: how to extract dependencies from', $LF, $LF, '  ', $file, '?', $LF;
        }

    }

    my @deps = keys %deps;
    return @deps;
}

sub constructors_under_node {
    my $node = shift;

    my %constructors = ();

    my @all_modes = $node->findnodes ('descendant::Typ');
    my @all_predicates = $node->findnodes ('descendant::Pred[not(@kind = "P")]');
    my @all_functions = $node->findnodes ('descendant::Func[not(@kind = "F")]');
    my @all_attributes = $node->findnodes ('descendant::Attr');
    my @all_fields = $node->findnodes ('descendant::Field');
    my @all_numerals = $node->findnodes ('descendant::Num');

    my %modes = ();
    my %predicates = ();
    my %functions = ();
    my %attributes = ();
    my %fields = ();
    my %numerals = ();

    foreach my $mode (@all_modes) {
        my $kind = $mode->getAttribute ('kind');
        my $aid = $mode->getAttribute ('aid');
        my $nr = $mode->getAttribute ('absnr');

        if (! defined $kind) {
            croak 'Error: mode node lacks a kind attribute.';
        }

        if (! defined $aid) {
            croak 'Error: mode node lacks an aid attribute!';
        }

        if (! defined $nr) {
            croak 'Error: mode node lacks an absnr attribute!';
        }

        $kind = lc $kind;
        $aid = lc $aid;
        $modes{"${aid}:${kind}constructor:${nr}"} = 0;
    }

    foreach my $predicate (@all_predicates) {
        my $kind = $predicate->getAttribute ('kind');
        my $aid = $predicate->getAttribute ('aid');
        my $nr = $predicate->getAttribute ('absnr');

        if (! defined $kind) {
            croak 'Error: predicate node lacks a kind attribute.';
        }

        if (! defined $aid) {
            croak 'Error: predicate node lacks an aid attribute!';
        }

        if (! defined $nr) {
            croak 'Error: predicate node lacks an absnr attribute!';
        }

        $kind = lc $kind;
        $aid = lc $aid;
        $predicates{"${aid}:${kind}constructor:${nr}"} = 0;
    }

    foreach my $function (@all_functions) {
        my $kind = $function->getAttribute ('kind');
        my $aid = $function->getAttribute ('aid');
        my $nr = $function->getAttribute ('absnr');

        if (! defined $kind) {
            croak 'Error: function node lacks a kind attribute!';
        }

        if (! defined $aid) {
            croak 'Error: function node lacks an aid attribute!';
        }

        if (! defined $nr) {
            croak 'Error: function node lacks an absnr attribute!';
        }

        $kind = lc $kind;
        $aid = lc $aid;
        $functions{"${aid}:${kind}constructor:${nr}"} = 0;
    }

    foreach my $attribute (@all_attributes) {
        my $aid = $attribute->getAttribute ('aid');
        my $nr = $attribute->getAttribute ('nr');

        if (! defined $aid) {
            croak 'Error: attribute node lacks an aid attribute!';
        }

        if (! defined $nr) {
            croak 'Error: attribute node lacks an nr attribute!';
        }

        $aid = lc $aid;
        $attributes{"${aid}:vconstructor:${nr}"} = 0;
    }

    foreach my $field (@all_fields) {
        my $aid = $field->getAttribute ('aid');
        my $nr = $field->getAttribute ('nr');

        if (! defined $aid) {
            croak 'Error: field node lacks an aid field!';
        }

        if (! defined $nr) {
            croak 'Error: field node lacks an nr field!';
        }

        $aid = lc $aid;
        $fields{"${aid}:lconstructor:${nr}"} = 0;
    }

    foreach my $numeral (@all_numerals) {
        my $nr = $numeral->getAttribute ('nr');

        if (! defined $nr) {
            croak 'Error: numeral node lacks an nr attribute!';
        }

        $modes{"numeral:${nr}"} = 0;
    }

    my @modes= keys %modes;
    my @predicates = keys %predicates;
    my @functions = keys %functions;
    my @attributes = keys %attributes;
    my @fields = keys %fields;
    my @numerals = keys %numerals;

    my @everything = (@modes, @predicates, @functions, @attributes, @fields, @numerals);

    return @everything;

}

my @miz = glob "${article_dir}/*.miz";

if (scalar @miz == 0) {
    confess 'Error: no .miz found in ', $article_dir, '.';
}

if (scalar @miz > 1) {
    confess 'Error: multiple .miz files found under ', $article_dir, '.';
}

my $article = $miz[0];
my $article_name = basename ($article, '.miz');

my @prel_files = glob "${prel_dir}/*" ;
@prel_files = sort { fragment_less_than ($a, $b) } @prel_files;
my %dependencies = ();

# state of the enumeration
my %enumeration = (
    'ccluster' => 1,
    'fcluster' => 1,
    'rcluster' => 1,
    'deftheorem' => 1,
    'theorem' => 1,
    'scheme' => 1,
    'lemma' => 1,
    'kconstructor' => 1,
    'mconstructor' => 1,
    'rconstructor' => 1,
    'vconstructor' => 1,
    'kdefiniens' => 1,
    'mdefiniens' => 1,
    'rdefiniens' => 1,
    'vdefiniens' => 1,
    'kpattern' => 1,
    'mpattern' => 1,
    'rpattern' => 1,
    'vpattern' => 1,
    'reduction' => 1,
    'sethood' => 1,
);

my %local_item_table = ();

sub resolve_local_item {
    my $item = shift;
    if (defined $local_item_table{$item}) {
        return $local_item_table{$item};
    }
    if ($item =~ / \A ${PREFIX_LC} (\d+) [:] /) {
        my $fragment_number = $1;
        if ($item =~ / \A (.+) [[] ([a-z]+) []] \z/) {
            (my $underlying_item, my $property) = ($1, $2);
            my $resolved = $local_item_table{$underlying_item};
            if (defined $resolved) {
                my $full_item = "${resolved}[${property}]";
                return $full_item;
            } else {
                confess 'Error: could not resolve \'', $underlying_item, '\'.', $LF, 'The local item table looks like:', $LF, Dumper (%local_item_table);
            }
        } elsif ($item =~ / [:] theorem [:] [1] \z/) {
            # is this actually a deftheorem or a lemma in disguise?
            my $deftheorem_item = "${PREFIX_LC}${fragment_number}:deftheorem:1";
            my $lemma_item = "${PREFIX_LC}${fragment_number}:lemma:1";
            my $resolved_deftheorem = $local_item_table{$deftheorem_item};
            my $resolved_lemma = $local_item_table{$lemma_item};
            if (defined $resolved_deftheorem) {
                return $resolved_deftheorem;
            } elsif (defined $resolved_lemma) {
                return $resolved_lemma;
            } else {
                confess 'Error: could not resolve the theorem item \'', $item, '\'.', $LF, 'The local item table looks like:', $LF, Dumper (%local_item_table);
            }
        } else {
            confess 'Error: could not resolve \'', $item, '\'.', $LF, 'The local item table looks like:', $LF, Dumper (%local_item_table);
        }
    } else {
        return $item;
    }
}

my $num_fragments = 0;
foreach my $prel_file (@prel_files) {
    my $fragment_number = fragment_number ($prel_file);
    if ($fragment_number > $num_fragments) {
        $num_fragments = $fragment_number;
    }
}

foreach my $prel_file (@prel_files) {

    my $fragment_number = fragment_number ($prel_file);

    my $fragment = "${PREFIX_LC}${fragment_number}";
    my $fragment_miz = "${text_dir}/${fragment}.miz";
    my $fragment_msx = "${text_dir}/${fragment}.msx";
    my $fragment_xml = "${text_dir}/${fragment}.xml1";
    my $prel_doc = parse_xml_file ($prel_file);
    my $prel_root = $prel_doc->documentElement ();
    my $fragment_msx_doc = parse_xml_file ($fragment_msx);
    my $fragment_msx_root = $fragment_msx_doc->documentElement ();
    my $fragment_xml_doc = parse_xml_file ($fragment_xml);
    my $fragment_xml_root = $fragment_xml_doc->documentElement ();

    my $item;
    my @deps;

    if ($prel_file =~ / [.] dcl \z/) {
        my @ccluster_nodes = $fragment_xml_root->findnodes ('descendant::CCluster');
        my @fcluster_nodes = $fragment_xml_root->findnodes ('descendant::FCluster');
        my @rcluster_nodes = $fragment_xml_root->findnodes ('descendant::RCluster');
        foreach my $cluster_node (@ccluster_nodes, @fcluster_nodes, @rcluster_nodes) {
            my $kind;
            if ($cluster_node->exists ('self::CCluster')) {
                $kind = 'c';
            } elsif ($cluster_node->exists ('self::FCluster')) {
                $kind = 'f';
            } elsif ($cluster_node->exists ('self::RCluster')) {
                $kind = 'r';
            } else {
                confess 'Error: unhandled cluster node in', $LF, $LF, '  ', $fragment_xml, $LF;
            }
            my $key = "${kind}cluster";
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            $item = "${article_name}:${kind}cluster:${index}";
            my $local_item = "${PREFIX_LC}${fragment_number}:${kind}cluster:1";
            $local_item_table{$local_item} = $item;
            @deps = article_dependencies ($fragment_miz);
            $enumeration{$key} = $index + 1;
            $dependencies{$item} = \@deps;
        }
    } elsif ($prel_file =~ / [.] the \z/) {
        if ($fragment_msx_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C induced"]')) {
            # appears to be an induced theorem; we'll take care of this later
            next;
        } elsif ($fragment_msx_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C deftheorem"]')) {
            next;
        } elsif ($fragment_msx_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C abstractness"]')) {
            next;
        } elsif ($prel_doc->exists ('count (descendant::Theorem) > 1')) {
            confess 'Too many theorems in ', $prel_doc, '; we assume there is exactly 1.';
        } elsif ($prel_doc->exists ('descendant::Theorem[@kind = "T"]')) {
            my $key = undef;
            if ($fragment_msx_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C promoted lemma"]')) {
                $key = 'lemma';
            } else {
                $key = 'theorem';
            }
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            $item = "${article_name}:${key}:${index}";
            my $local_item = "${PREFIX_LC}${fragment_number}:${key}:1";
            $local_item_table{$local_item} = $item;
            $enumeration{$key} = $index + 1;
            @deps = article_dependencies ($fragment_miz);
            $dependencies{$item} = \@deps;
        } elsif ($prel_doc->exists ('descendant::Theorem[@kind = "D"]')) {
            my $key = 'deftheorem';
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            $item = "${article_name}:${key}:${index}";
            # Go hunting for a deftheorem item following the definition
            my $next_fragment_number = $fragment_number + 1;
            if ($next_fragment_number > $num_fragments) {
                confess 'Error: hunting for a deftheorem, we tried to look into fragment ', $next_fragment_number, ' but there are only ', $num_fragments, ' total fragments.';
            }
            my $next_fragment_msx = "${text_dir}/${PREFIX_LC}${next_fragment_number}.msx";
            my $next_fragment_miz = "${text_dir}/${PREFIX_LC}${next_fragment_number}.miz";
            my $next_fragment_doc = parse_xml_file ($next_fragment_msx);
            my $next_fragment_root = $next_fragment_doc->documentElement ();
            if ($next_fragment_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C deftheorem"]')) {
                my $local_item = "${PREFIX_LC}${fragment_number}:${key}:1";
                $local_item_table{$local_item} = $item;
                $enumeration{$key} = $index + 1;
                @deps = article_dependencies ($next_fragment_miz);
                $dependencies{$item} = \@deps;
            }
        } else {
            confess 'Error: unable to make sense of ', $prel_file, '.';
        }
    } elsif ($prel_file =~ / [.] sch \z/) {
        my $key = 'scheme';
        my $index = $enumeration{$key};
        if (! defined $index) {
            confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
        }
        $item = "${article_name}:${key}:${index}";
        $enumeration{$key} = $index + 1;
        @deps = article_dependencies ($fragment_miz);
        $dependencies{$item} = \@deps;
        my $local_item = "${PREFIX_LC}${fragment_number}:${key}:1";
        $local_item_table{$local_item} = $item;
    } elsif ($prel_file =~ / [.] dco \z/) {
        my @constructor_nodes = $fragment_xml_root->findnodes ('descendant::Constructor');
        my %local_enumeration = (
            'kconstructor' => 1,
            'rconstructor' => 1,
            'vconstructor' => 1,
        );
        foreach my $constructor_node (@constructor_nodes) {
            my $kind = $constructor_node->getAttribute ('kind');
            if (! defined $kind) {
                confess 'Error: constructor node in', $LF, $LF, '  ', $prel_file, $LF, $LF, 'lacks a kind attribute.';
            }
            $kind = lc $kind;
            my $key = "${kind}constructor";
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            my $local_index = $local_enumeration{$key};
            if (! defined $local_index) {
                confess 'Error: somehow the local constructor enumeration for', $LF, $LF, '  ', $prel_file, $LF, $LF, 'for constructors of kind ', $kind, ' is missing.';
            }
            $item = "${article_name}:${kind}constructor:${index}";
            my $local_item = "${PREFIX_LC}${fragment_number}:${kind}constructor:${local_index}";
            $enumeration{$key} = $index + 1;
            $local_enumeration{$key} = $local_index + 1;
            $local_item_table{$local_item} = $item;
            @deps = constructors_under_node ($constructor_node);
            $dependencies{$item} = \@deps;

            # Constructor properties
            if (($constructor_node->exists ('Properties')) || ($kind eq 'k')) {
                my @properties = $constructor_node->findnodes ('Properties/*');
                @properties = map { $_->nodeName () } @properties;
                if ($kind eq 'k') {
                    push (@properties, 'Existence', 'Uniqueness');
                }
                if ($fragment_msx_root->exists ('descendant::Item[@kind = "Correctness-Condition" and @condition = "coherence"]')) {
                    push (@properties, 'Coherence');
                }
                foreach my $property (@properties) {
                    if ($property eq 'Antisymmetry') {
                        $property = 'Asymmetry';
                    }
                    my $target_fragment_index = undef;
                    my $i = $fragment_number - 1;
                    while (($i > 0)
                               && (! defined $target_fragment_index)) {
                        my $earlier_fragment_msx = "${text_dir}/${PREFIX_LC}${i}.msx";
                        my $earlier_fragment_doc = parse_xml_file ($earlier_fragment_msx);
                        my $earlier_fragment_root = $earlier_fragment_doc->documentElement ();
                        my $lemma_name = "${property}Lemma";
                        my $pragma_xpath = 'Item[@kind = "Pragma" and @spelling = "$C induced"]';
                        my $label_xpath = 'Item[@kind = "Theorem-Item"]/Proposition/Label[@spelling = "' . $lemma_name . '"]';
                        if ($earlier_fragment_root->exists ($pragma_xpath)) {
                            if ($earlier_fragment_root->exists ($label_xpath)) {
                                $target_fragment_index = $i;
                            }
                        }
                        $i--;
                    }
                    my $property_lc = lc $property;
                    my $condition_or_property = "${article_name}:${kind}constructor:${index}[${property_lc}]";
                    if (defined $target_fragment_index) {
                        my $property_fragment = "${PREFIX_LC}${target_fragment_index}";
                        my $property_fragment_miz = "${text_dir}/${property_fragment}.miz";
                        my @condition_or_property_deps = article_dependencies ($property_fragment_miz);
                        $dependencies{$condition_or_property}
                            = \@condition_or_property_deps;
                        my $local_name = "${PREFIX_LC}${target_fragment_index}:theorem:1";
                        $local_item_table{$local_name} = $condition_or_property;
                        # carp 'registering ', $condition_or_property, ' under ', $local_name;
                    } else {
                        # warn 'could not find property ', $property, ' for ', $kind, 'constructor ', $index;
                    }
                }
            }
        }
    } elsif ($prel_file =~ / [.] def \z/) {
        my @definiens_nodes
            = $fragment_xml_root->findnodes ('descendant::Definiens');
        foreach my $definiens_node (@definiens_nodes) {
            my $kind = $definiens_node->getAttribute ('constrkind');
            if (! defined $kind) {
                confess 'Error: definiens node in', $LF, $LF, '  ', $prel_file, $LF, $LF, 'lacks a kind attribute.';
            }
            $kind = lc $kind;
            my $key = "${kind}definiens";
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            my $item = "${article_name}:${key}:${index}";
            # Check that the following item is the expected deftheorem
            my $next_fragment_number = $fragment_number + 1;
            if ($next_fragment_number > $num_fragments) {
                confess 'The definiens contained in fragment ', $fragment_number, ' ought to be followed by an induced deftheorem fragment, but there only ', $num_fragments, ' total fragments.';
            }
            my $next_fragment_miz = "${text_dir}/${PREFIX_LC}${next_fragment_number}.miz";
            my $next_fragment_msx = "${text_dir}/${PREFIX_LC}${next_fragment_number}.msx";
            my $next_fragment_doc = parse_xml_file ($next_fragment_msx);
            my $next_fragment_root = $next_fragment_doc->documentElement ();
            if (! $next_fragment_root->exists ('Item[@kind = "Pragma" and @spelling = "$C deftheorem"]')) {
                confess 'Fragment ', $next_fragment_number, ' ought to contain an induced deftheorem (because the previous fragment generates a definiens); but it does not.';
            }
            # my $local_item = "${PREFIX_LC}${fragment_number}:${key}:1";
            my $local_theorem_item = "${PREFIX_LC}${next_fragment_number}:${key}:1";
            my $local_definiens_item = "${PREFIX_LC}${fragment_number}:${key}:1";
            $local_item_table{$local_theorem_item} = $item;
            $local_item_table{$local_definiens_item} = $item;
            $enumeration{$key} = $index + 1;
            @deps = article_dependencies ($next_fragment_miz);
            $dependencies{$item} = \@deps;
        }
    } elsif ($prel_file =~ / [.] dfr \z/) {
        # ignore
    } elsif ($prel_file =~ / [.] dno \z/) {

        my @pattern_nodes = $fragment_xml_root->findnodes ('descendant::Pattern');
        my %local_enumeration = (
            'kpattern' => 1,
            'mpattern' => 1,
            'rpattern' => 1,
            'vpattern' => 1,
        );
        foreach my $pattern_node (@pattern_nodes) {
            my $kind = $pattern_node->getAttribute ('kind');
            if (! defined $kind) {
                confess 'Error: pattern node in', $LF, $LF, '  ', $prel_file, $LF, $LF, 'lacks a kind attribute.';
            }
            $kind = lc $kind;
            my $key = "${kind}pattern";
            my $index = $enumeration{$key};
            my $local_index = $local_enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            if (! defined $local_index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the local enumeration table for', $LF, $LF, $prel_file, $LF;
            }
            $item = "${article_name}:${kind}pattern:${index}";
            my $local_item = "${PREFIX_LC}${fragment_number}:${kind}pattern:${local_index}";
            $local_item_table{$local_item} = $item;
            $enumeration{$key} = $index + 1;
            $local_enumeration{$key} = $local_index + 1;
            @deps = constructors_under_node ($pattern_node);
            $dependencies{$item} = \@deps;
        }
    } elsif ($prel_file =~ / [.] dpr \z /) {
        my @property_nodes
            = $fragment_xml_root->findnodes ('descendant::Property');
        foreach my $property_node (@property_nodes) {
            my $x = $property_node->getAttribute ('x');
            if ($x ne '12') {
                confess 'Error: we assume that Property nodes always have the value \'12\' for their x attribute.';
            }
            my $key = 'sethood';
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            $item = "${article_name}:sethood:${index}";
            my $local_item = "${PREFIX_LC}${fragment_number}:${key}:1";
            $local_item_table{$local_item} = $item;
            $enumeration{$key} = $index + 1;
            @deps = article_dependencies ($fragment_miz);
            $dependencies{$item} = \@deps;
        }
    } elsif ($prel_file =~ / [.] drd \z /) {
        my @reduction_nodes
            = $fragment_xml_root->findnodes ('descendant::Reduction');
        foreach my $reduction_node (@reduction_nodes) {
            my $key = 'reduction';
            my $index = $enumeration{$key};
            if (! defined $index) {
                confess 'Error: somehow the key', $LF, $LF, '  ', $key, $LF, $LF, 'is missing from the enmeration table, which looks like:', $LF, Dumper (%enumeration);
            }
            $item = "${article_name}:reduction:${index}";
            my $local_item = "${PREFIX_LC}${fragment_number}:reduction:1";
            $local_item_table{$local_item} = $item;
            $enumeration{$key} = $index + 1;
            @deps = article_dependencies ($fragment_miz);
            $dependencies{$item} = \@deps;
        }
    } else {
        confess 'Error: unable to make sense of the prel file', $LF, $LF, '  ', $prel_file, $LF;
    }

}

# warn 'OK, the table now looks like this:', $LF, Dumper (%local_item_table);

# Now take care of all induced theorems

foreach my $prel_file (@prel_files) {
    if ($prel_file =~ / [.] the \z /) {
        my $fragment_number = fragment_number ($prel_file);
        my $fragment = "${PREFIX_LC}${fragment_number}";
        my $fragment_miz = "${text_dir}/${fragment}.miz";
        my $fragment_msx = "${text_dir}/${fragment}.msx";
        my $fragment_xml = "${text_dir}/${fragment}.xml1";
        my $prel_doc = parse_xml_file ($prel_file);
        my $prel_root = $prel_doc->documentElement ();
        my $fragment_msx_doc = parse_xml_file ($fragment_msx);
        my $fragment_msx_root = $fragment_msx_doc->documentElement ();
        my $fragment_xml_doc = parse_xml_file ($fragment_xml);
        my $fragment_xml_root = $fragment_xml_doc->documentElement ();
        if ($fragment_msx_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C induced"]')) {
            (my $theorem_node) = $fragment_msx_root->findnodes ('descendant::Item[@kind = "Theorem-Item"]');
            if (! defined $theorem_node) {
                confess 'We are inspecting an induced theorem in', $LF, $LF, '  ', $fragment_msx, $LF, $LF, 'but we failed to find a theorem item, as expected.';
            }
            (my $proposition_node) = $theorem_node->findnodes ('Proposition');
            if (! defined $proposition_node) {
                confess 'We are inspecting an induced theorem in', $LF, $LF, '  ', $fragment_msx, $LF, $LF, 'but the theorem somehow fails to have a Proposition child.';
            }
            (my $label_node) = $proposition_node->findnodes ('Label');
            if (! defined $label_node) {
                confess 'We are inspecting the Proposition node for an induced theorem in', $LF, $LF, $fragment_msx, $LF, $LF, 'but the node somehow lacks a Label child.';
            }
            my $label = $label_node->getAttribute ('spelling');
            if (! defined $label) {
                confess 'Missing label for a theorem!';
            }
            my $property = undef;
            if ($label =~ / \A (.+)Lemma \z/) {
                $property = $1;
            } else {
                confess 'To what property does \'', $label, '\' correspond? We are working on ', $fragment_msx;
            }
            $property = lc $property;
            # Now find the item that uses this one
            my $this_fragment = "${PREFIX_UC}${fragment_number}";
            # warn 'working on property ', $property, ' of fragment ', $this_fragment;
            my $target_fragment_index = undef;
            my $i = $fragment_number + 1;
            while ((! defined $target_fragment_index) && ($i < $num_fragments)) {
                my $target_fragment_xml = "${text_dir}/${PREFIX_LC}${i}.msx";
                my $target_fragment_doc = parse_xml_file ($target_fragment_xml);
                my $target_fragment_root = $target_fragment_doc->documentElement ();
                my $ref_xpath = 'descendant::Theorem-Reference[@spelling = "' . $this_fragment . '"]';
                if ($target_fragment_root->exists ($ref_xpath)) {
                    $target_fragment_index = $i;
                }
                $i++;
            }
            if (defined $target_fragment_index) {
                # warn 'target fragment index is: ', $target_fragment_index;
                if ($property eq 'compatibility') {
                    my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                    my $other_local_item = "${PREFIX_LC}${target_fragment_index}:deftheorem:1";
                    my $resolved_item = $local_item_table{$other_local_item};
                    if (! defined $resolved_item) {
                        confess 'Error: dealing with compatibility (fragment number ', $fragment_number, '), we find that the local item table does not have a resolved item for ', $other_local_item, '.  The local item table looks like:', $LF, Dumper (%local_item_table);
                    }
                    my $item = "${resolved_item}[${property}]";
                    $local_item_table{$local_item} = $item;
                    my @deps = article_dependencies ($fragment_miz);
                    $dependencies{$item} = \@deps;
                    next;
                }
                if ($property eq 'coherence') {
                    # Try looking for a function for which this is the
                    # coherence/correctness condition
                    # What is the next functor constructor?
                    my $target_fragment_index = undef;
                    my $j = $fragment_number + 1;
                    while ((! defined $target_fragment_index) && ($j < $num_fragments)) {
                        my $target_fragment_xml = "${text_dir}/${PREFIX_LC}${j}.xml1";
                        my $target_fragment_doc = parse_xml_file ($target_fragment_xml);
                        my $target_fragment_root = $target_fragment_doc->documentElement ();
                        my $def_xpath = 'descendant::Definition[@kind = "K" and Coherence]';
                        my $correctness_xpath = 'descendant::Definition[@kind = "K" and Correctness[Coherence]]';
                        if ($target_fragment_root->exists ($def_xpath)) {
                            $target_fragment_index = $j;
                        }
                        if ($target_fragment_root->exists ($correctness_xpath)) {
                            $target_fragment_index = $j;
                        }
                        $j++;
                    }
                    if (! defined $target_fragment_index) {
                        confess 'Error: unable to find a functor definition with a coherence correctness condition after fragment ', $fragment_number, ', so we are unable to deal with the existence/uniqueness condition it contains.';
                    }
                    my $constructor_item = "${PREFIX_LC}${target_fragment_index}:kconstructor:1";
                    my $resolved_item = $local_item_table{$constructor_item};
                    if (! defined $resolved_item) {
                        confess 'Error: unable to find', $LF, $LF, '  ', $constructor_item, $LF, $LF, 'in the local item table, which is:', $LF, $LF, Dumper (%local_item_table);
                    }
                    my $item = "${resolved_item}[${property}]";
                    my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                    $local_item_table{$local_item} = $item;
                    # warn 'HEY again: registering ', $local_item, ' under ', $item;
                    my @deps = article_dependencies ($fragment_miz);
                    $dependencies{$item} = \@deps;
                    next;
                }
                my @candidate_items = ();
                foreach my $local_item (keys %local_item_table) {
                    if ($local_item =~ / \A ${PREFIX_LC}${target_fragment_index} [:] /) {
                        my $resolved_item = $local_item_table{$local_item};
                        if ($resolved_item =~ / [:] . constructor [:] /) {
                            push (@candidate_items, $resolved_item);
                        }
                    }
                }
                if (scalar @candidate_items == 0) {
                    carp 'no candidate items! We are dealing with property ', $property, ' for ', $prel_file;
                } elsif (scalar @candidate_items > 1) {
                    confess 'Error: we found multiple items coming from fragment ', $target_fragment_index, ':', $LF, $LF, Dumper (@candidate_items), $LF, 'Which one should we choose to deal with property ', $property, '?';
                } else {
                    my $other_item = $candidate_items[0];
                    # warn 'the item coming from fragment ', $target_fragment_index, ' is ', $other_item;
                    if ($other_item =~ / [[] [a-z]+ []] \z/) {
                        my $item = $other_item;
                        my $local_item = "${PREFIX_LC}${target_fragment_index}:theorem:1";
                        $local_item_table{$local_item} = $item;
                        # warn 'Part 2: registering ', $item, ' under ', $local_item;
                    } else {
                        my $item = "${other_item}[${property}]";
                        my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                        $local_item_table{$local_item} = $item;
                        # warn 'Part 2: registering ', $item, ' under ', $local_item;
                    }
                }
            } elsif (($property eq 'existence') || ($property eq 'uniqueness')) {
                # What is the next functor constructor?
                my $target_fragment_index = undef;
                my $j = $fragment_number + 1;
                while ((! defined $target_fragment_index) && ($j < $num_fragments)) {
                    my $target_fragment_xml = "${text_dir}/${PREFIX_LC}${j}.xml1";
                    my $target_fragment_doc = parse_xml_file ($target_fragment_xml);
                    my $target_fragment_root = $target_fragment_doc->documentElement ();
                    my $def_xpath = 'descendant::Definition[@kind = "K" and Coherence]';
                    my $correctness_xpath = 'descendant::Definition[@kind = "K" and Correctness[Coherence]]';
                    if ($target_fragment_root->exists ($def_xpath)) {
                        $target_fragment_index = $j;
                    }
                    if ($target_fragment_root->exists ($correctness_xpath)) {
                        $target_fragment_index = $j;
                    }
                    $j++;
                }
                if (! defined $target_fragment_index) {
                    confess 'Error: unable to find a functor definition with a coherence correctness condition after fragment ', $fragment_number, ', so we are unable to deal with the existence/uniqueness condition it contains.';
                }
                my $constructor_item = "${PREFIX_LC}${target_fragment_index}:kconstructor:1";
                my $resolved_item = $local_item_table{$constructor_item};
                if (! defined $resolved_item) {
                    confess 'Error: unable to find', $LF, $LF, '  ', $constructor_item, $LF, $LF, 'in the local item table, which is:', $LF, $LF, Dumper (%local_item_table);
                }
                my $item = "${resolved_item}[${property}]";
                my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                $local_item_table{$local_item} = $item;
                my @deps = article_dependencies ($fragment_miz);
                $dependencies{$item} = \@deps;
            } elsif ($property eq 'coherence') {
                # Try looking for a function for which this is the
                # coherence/correctness condition
                # What is the next functor constructor?
                my $target_fragment_index = undef;
                my $j = $fragment_number + 1;
                while ((! defined $target_fragment_index) && ($j < $num_fragments)) {
                    my $target_fragment_xml = "${text_dir}/${PREFIX_LC}${j}.xml1";
                    my $target_fragment_doc = parse_xml_file ($target_fragment_xml);
                    my $target_fragment_root = $target_fragment_doc->documentElement ();
                    my $def_xpath = 'descendant::Definition[@kind = "K" and Coherence]';
                    my $correctness_xpath = 'descendant::Definition[@kind = "K" and Correctness[Coherence]]';
                    if ($target_fragment_root->exists ($def_xpath)) {
                        $target_fragment_index = $j;
                    }
                    if ($target_fragment_root->exists ($correctness_xpath)) {
                        $target_fragment_index = $j;
                    }
                    $j++;
                }
                if (! defined $target_fragment_index) {
                    confess 'Error: unable to find a functor definition with a coherence correctness condition after fragment ', $fragment_number, ', so we are unable to deal with the existence/uniqueness condition it contains.';
                }
                my $constructor_item = "${PREFIX_LC}${target_fragment_index}:kconstructor:1";
                my $resolved_item = $local_item_table{$constructor_item};
                if (! defined $resolved_item) {
                    confess 'Error: unable to find', $LF, $LF, '  ', $constructor_item, $LF, $LF, 'in the local item table, which is:', $LF, $LF, Dumper (%local_item_table);
                }
                my $item = "${resolved_item}[${property}]";
                my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                $local_item_table{$local_item} = $item;
                my @deps = article_dependencies ($fragment_miz);
                $dependencies{$item} = \@deps;
            }
        }
    }
}

# warn 'After dealing with constructor properties, the table now looks like this:', $LF, Dumper (%local_item_table);

# deftheorems
my $deftheorem_index = 1;
foreach my $prel_file (@prel_files) {
    if ($prel_file =~ / [.] the \z /) {
        my $fragment_number = fragment_number ($prel_file);
        my $fragment = "${PREFIX_LC}${fragment_number}";
        my $fragment_miz = "${text_dir}/${fragment}.miz";
        my $fragment_msx = "${text_dir}/${fragment}.msx";
        my $fragment_xml = "${text_dir}/${fragment}.xml1";
        my $prel_doc = parse_xml_file ($prel_file);
        my $prel_root = $prel_doc->documentElement ();
        my $fragment_msx_doc = parse_xml_file ($fragment_msx);
        my $fragment_msx_root = $fragment_msx_doc->documentElement ();
        my $fragment_xml_doc = parse_xml_file ($fragment_xml);
        my $fragment_xml_root = $fragment_xml_doc->documentElement ();
        if ($fragment_msx_root->exists ('descendant::Item[@kind = "Pragma" and @spelling = "$C deftheorem"]')) {
            # The previous fragment had better be a definition
            my $previous_fragment_number = $fragment_number - 1;
            if ($previous_fragment_number < 1) {
                confess 'Not enough fragments.';
            }
            my $previous_the = "${prel_dir}/${PREFIX_LC}${previous_fragment_number}.the";
            if (! -e $previous_the) {
                confess 'Fragment ', $fragment_number, ' is an induced deftheorem, but the previous fragment does not generate a .the file.';
            }
            my $previous_the_doc = parse_xml_file ($previous_the);
            my $previous_the_root = $previous_the_doc->documentElement ();
            if ($previous_the_root->exists ('descendant::Theorem[@kind = "D"]')) {
                my $deftheorem_item = "${article_name}:deftheorem:${deftheorem_index}";
                my $local_deftheorem_item = "${PREFIX_LC}${fragment_number}:deftheorem:1";
                my $local_theorem_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                if (defined $local_item_table{$local_deftheorem_item}) {
                    my $already = $local_item_table{$local_deftheorem_item};
                    confess $local_deftheorem_item, ' is already in the local item table as ', $already;
                }
                $local_item_table{$local_deftheorem_item} = $deftheorem_item;
                # $local_item_table{$local_theorem_item} = $deftheorem_item;
                my @deps = article_dependencies ($fragment_miz);
                $dependencies{$deftheorem_item} = \@deps;
                $deftheorem_index++;
            } else {
                confess 'Fragment ', $fragment_number, ' is an induced deftheorem, but the previous .the file (', $previous_the, ') does not generate a definitional theorem.';
            }
        }
    }
}

# Constructor properties
foreach my $item (keys %dependencies) {
    my $source = source_of_item ($item);
    my $fragment_number = fragment_number ($source);
    my $fragment_atr = "${text_dir}/${PREFIX_LC}${fragment_number}.atr";
    my $fragment_atr_doc = parse_xml_file ($fragment_atr);
    my $fragment_atr_root = $fragment_atr_doc->documentElement ();
    my @deps = @{$dependencies{$item}};
    # carp 'Deps of ', $item, ':', $LF, Dumper (@deps);
    foreach my $dep (@deps) {
        if ($dep =~ / \A ([a-z0-9_]+) [:] ([a-z]) constructor [:] (\d+) \z /) {
            (my $article, my $kind, my $nr) = ($1, $2, $3);

            my $article_uc = uc $article;
            my $kind_uc = uc $kind;
            my $xpath = 'descendant::Constructor[@aid = "' . $article_uc . '" and @kind = "' . $kind_uc . '" and @nr = "' . $nr . '"]';
            (my $constructor_node) = $fragment_atr_doc->findnodes ($xpath);
            if (! defined $constructor_node) {
                confess 'We failed to find a constructor node in ', $fragment_atr, ' matching', $LF, $LF, '  ', $xpath, $LF, $LF, 'Yet somehow we believe that ', $item, ' (source: ', $source, ') depends on ', $dep, '.';
            }
            my @property_nodes = $constructor_node->findnodes ('Properties/*');
            foreach my $property_node (@property_nodes) {
                my $property = $property_node->nodeName ();
                if ($property eq 'Antisymmetry') {
                    $property = 'Asymmetry';
                }
                my $property_lc = lc $property;
                my $item = "${PREFIX_LC}${fragment_number}";
                my $constructor_item = "${article}:${kind}constructor:${nr}";
                my $property_item = "${constructor_item}[${property_lc}]";
                push (@deps, $property_item);
            }
            $dependencies{$item} = \@deps;
        }
    }
}



# Resolve all dependencies
my %resolved_dependencies = ();

sub register_dependencies {
    my $item = shift;
    my @deps = @{$dependencies{$item}};
    my @resolved_deps = map { resolve_local_item ($_) } @deps;
    my %resolved = map { $_ => 1 } @resolved_deps;
    my @resolved_no_deps = keys %resolved;
    $resolved_dependencies{$item} = \@resolved_no_deps;
    return;
}

foreach my $item (keys %dependencies) {
    register_dependencies ($item);
}

sub source_of_item {
    my $item = shift;
    my @candidates = ();
    foreach my $local_item (keys %local_item_table) {
        my $resolved_item = $local_item_table{$local_item};
        if ($resolved_item eq $item) {
            push (@candidates, $local_item);
        }
    }
    if (scalar @candidates == 0) {
        confess 'Unable to determine where ', $item, ' comes from.', $LF, 'The local item table looks like:', $LF, Dumper (%local_item_table);
    } elsif (scalar @candidates == 1) {
        return $candidates[0];
    } else {
        # Pick the later one
        my $max = undef;
        my $source = undef;
        foreach my $candidate (@candidates) {
            my $fragment_number = fragment_number ($candidate);
            if (defined $max) {
                if ($fragment_number > $max) {
                    $max = $fragment_number;
                    $source = $candidate;
                } elsif ($fragment_number == $max) {
                    confess 'To resolve ', $item, ', how should we deal with these candidates:', $LF, Dumper (@candidates);
                }
            } else {
                $max = $fragment_number;
                $source = $candidate;
            }
        }
        return $source;
    }
}

# # Sanity checks
# # Warn about items that depend on nothing
# foreach my $item (keys %resolved_dependencies) {
#     my @deps = @{$resolved_dependencies{$item}};
#     my $num_deps = scalar @deps;
#     if ($num_deps == 0) {
#         my $source_item = source_of_item ($item);
#         carp $item, ' (coming from ', $source_item, ') depends on nothing.';
#     }

# }

# # Every local item appearing as needed is "defined"
# my %needed = ();
# foreach my $item (keys %resolved_dependencies) {
#     my @deps = @{$resolved_dependencies{$item}};
#     foreach my $dep (@deps) {
#         if ($dep =~ / \A ${article_name} [:] /) {
#             $needed{$dep} = 0;
#         }
#     }
# }
# foreach my $item (keys %needed) {
#     if (! defined $resolved_dependencies{$item}) {
#         carp $item, ' appears as a dependency, but it is not "defined" in the current table.';
#     }
# }

# # Every fragment yields an item that appears in the local item table
# my @local_items = keys %local_item_table;
# my @miz_files = glob "${text_dir}/${PREFIX_LC}*.miz";
# foreach my $miz_file (@miz_files) {
#     my $fragment_number = fragment_number ($miz_file);
#     if (none { $_ =~ / \A ${PREFIX_LC}${fragment_number} [:] / } @local_items) {
#         carp 'Fragment ', $fragment_number, ' produced nothing that was registered in the local item table.';
#     }
# }

# # Warn about unused items
# foreach my $item (keys %resolved_dependencies) {
#     if (! (defined $needed{$item})) {
#         my $source = source_of_item ($item);
#         carp $item, ' (coming from ', $source, ') is never used.';
#     }
# }

my $fragment_item_path = "${article_dir}/${article_name}.map";

# Save the fragment-item table
open (my $fragment_fh, '>', $fragment_item_path)
    or confess 'Cannot open an output filehandle for ', $fragment_item_path;
foreach my $local_item (keys %local_item_table) {
    my $item = $local_item_table{$local_item};
    say {$fragment_fh} "${local_item} ${item}";
}
close $fragment_fh
    or confess 'Cannot close the output filehandle for ', $fragment_item_path;

my $deps_path = "${article_dir}/${article_name}.deps";

open (my $deps_fh, '>', $deps_path)
    or confess 'Cannot open an output filehandle for ', $deps_path;
foreach my $item (keys %resolved_dependencies) {
    my @deps = @{$resolved_dependencies{$item}};
    print {$deps_fh} $item;
    foreach my $dep (@deps) {
        print {$deps_fh} ' ', $dep;
    }
    print {$deps_fh} $LF;
}
close $deps_fh
    or confess 'Cannot close output filehandle for ', $deps_path;

sub is_scheme_item {
    my $item = shift;
    return ($item =~ / [:] scheme [:] \d+ \z /);
}

sub is_requirement_item {
    my $item = shift;
    if ($item =~ /\A requirement [:] \d+ \z/) {
        return 1;
    } else {
        return 0;
    }
}

sub is_definiens_item {
    my $item = shift;
    return ($item =~ / [:] . definiens [:] /);
}

sub is_theorem_item {
    my $item = shift;
    return ($item =~ / [:] theorem [:] /)
}

sub is_lemma_item {
    my $item = shift;
    return ($item =~ / [:] lemma [:] /)
}

sub is_cluster_item {
    my $item = shift;
    return ($item =~ / [:] . cluster [:] /);
}

sub is_deftheorem_item {
    my $item = shift;
    return ($item =~ / [:] deftheorem [:] /)
}

sub is_constructor_property_item {
    my $item = shift;
    return ($item =~ / [:] . constructor [:] \d+ [[] [a-z]+ []] \z/);
}

sub has_semantic_content {
    my $item = shift;
    if (is_scheme_item ($item)) {
        return 1;
    } elsif (is_requirement_item ($item)) {
        return 1;
    } elsif (is_deftheorem_item ($item)) {
        return 1;
    } elsif (is_theorem_item ($item)) {
        return 1;
    } elsif (is_lemma_item ($item)) {
        return 1;
    } elsif (is_cluster_item ($item)) {
        return 1;
    } elsif (is_deftheorem_item ($item)) {
        return 1;
    } elsif (is_definiens_item ($item)) {
        return 1;
    } elsif (is_constructor_property_item ($item)) {
        return 1;
    } else {
        return 0;
    }
}

sub render_semantic_content {
    my $node = shift;
    my @arguments = @_;
    my $name = $node->nodeName ();
    my $aid = $node->getAttribute ('aid');
    my $kind = $node->getAttribute ('kind');
    my $nr = $node->getAttribute ('nr');
    my $kind_lc = defined $kind ? lc $kind : undef;
    my $aid_lc = defined $aid ? lc $aid : undef;
    if ($name eq 'Pred' || $name eq 'Func') {
        if (! defined $aid) {
            confess 'aid attribute missing from a Pred/Func';
        }
        if (! defined $kind) {
            confess 'kind attribute missing from a Pred/Func';
        }
        my $absnr = $node->getAttribute ('absnr');
        if (! defined $absnr) {
            confess 'absnr attribute missing from a Pred/Func.';
        }
        if ($aid_lc =~ /\A ${PREFIX_LC} \d+ \z/) {
            my $local_item = "${aid_lc}:${kind_lc}constructor:${absnr}";
            my $item = $local_item_table{$local_item};
            if (! defined $item) {
                confess 'Could not find ', $local_item, ' in the local item table.';
            }
            $absnr = nr_of_item ($item);
            $aid_lc = $article_name;
        }
        my @children = $node->findnodes ('*');
        my $answer = "${kind_lc}${absnr}_${aid_lc}";
        if (scalar @children == 0) {
            return $answer;
        } elsif ($aid_lc eq 'hidden' && $kind_lc eq 'r' && $absnr eq '1') {
            (my $lhs) = $node->findnodes ('*[1]');
            (my $rhs) = $node->findnodes ('*[2]');
            my $rendered_lhs = render_semantic_content ($lhs, @arguments);
            my $rendered_rhs = render_semantic_content ($rhs, @arguments);
            return "(${rendered_lhs} = ${rendered_rhs})";
        } else {
            my $num_children = scalar @children;
            $answer .= '(';
            foreach my $i (1 .. $num_children) {
                my $child = $children[$i - 1];
                my $rendered_child = render_semantic_content ($child, @arguments);
                $answer .= $rendered_child;
                if ($i < $num_children) {
                    $answer .= ', ';
                }
            }
            $answer .= ')';
            return $answer;
        }
    } elsif ($name eq 'Choice') {
        # quite incomplete
        return render_choice_term ($node);
    } elsif ($name eq 'LocusVar') {
        return "X${nr}";
    } elsif ($name eq 'Verum') {
        return '$true';
    } elsif ($name eq 'PrivPred') {
        my $nr = $node->getAttribute ('nr');
        if (! defined $nr) {
            confess 'PrivPred node lacks an nr attribute.';
        }
        my @values = $node->findnodes ('*[preceding-sibling::*[1][self::Var]]');
        if (scalar @values != 1) {
            confess 'How to deal with a PrivPred that has more than 1 value?';
        }
        my $value = $values[0];
        return render_semantic_content ($value);
    } elsif ($name eq 'Is') {
        my @children = $node->findnodes ('*');
        if (scalar @children != 2) {
            confess 'Is node does not have exactly 2 children.';
        }
        my $term = $children[0];
        my $type = $children[1];
        my $term_rendered = render_semantic_content ($term, @arguments);
        return render_guard ($term_rendered, $type);
    } elsif ($name eq 'For') {
        (my $typ) = $node->findnodes ('Typ');
        if (! defined $typ) {
            confess 'Typ node not found under a universal quantifier.';
        }
        my $vid = $node->getAttribute ('vid');
        my $var_name = undef;
        if (defined $vid) {
            $var_name = "X${vid}";
        } else {
            my $vid = $node->findvalue ('count (ancestor::For[not(@vid)]) + 1');
            $var_name = "Y${vid}";
        }
        (my $matrix) = $node->findnodes ('*[position() = last()]');
        my $rendered_matrix = render_semantic_content ($matrix, @arguments);
        my $guard = render_guard ($var_name, $typ);
        return "(! [${var_name}] : (${guard} => ${rendered_matrix}))";
    } elsif ($name eq 'Typ') {
        confess 'We do not render bare Typ nodes!';
    } elsif ($name eq 'Var') {
        my @quantifiers = $node->findnodes ('ancestor::For');
        my $num_quantifiers = scalar @quantifiers;
        if ($num_quantifiers < $nr) {
            confess 'To render a Var with nr = ', $nr, ' we need at least that many quantifiers; but we found only ', $num_quantifiers;
        }
        my $quantifier = $quantifiers[$nr - 1];
        my $var_name = undef;
        if ($quantifier->hasAttribute ('vid')) {
            my $vid = $quantifier->getAttribute ('vid');
            $var_name = "X${vid}";
        } else {
            my $vid = $quantifier->findvalue ('count (ancestor::For[not(@vid)]) + 1');
            $var_name = "Y${vid}";
        }
        if (scalar @arguments < $nr) {
            return $var_name;
        } else {
            # We are substituting
            # carp 'we are substituting';
            my $val = $arguments[$nr - 1];
            return render_semantic_content ($val);
        }
    } elsif ($name eq 'And') {
        my @children = $node->findnodes ('*');
        if (scalar @children == 1) {
            my $child = $children[0];
            return render_semantic_content ($child, @arguments);
        } elsif (scalar @children == 2) {
            my $lhs = $children[0];
            my $rhs = $children[1];
            my $lhs_rendered = render_semantic_content ($lhs, @arguments);
            my $rhs_rendered = render_semantic_content ($rhs, @arguments);
            if ($rhs_rendered eq '0') {
                confess 'wtf? rhs node as string:', $LF, $rhs->toString;
            }
            # is this an equivalence?
            if ($lhs->exists ('self::Not') && $rhs->exists ('self::Not')) {
                (my $lhs_unnegated) = $lhs->findnodes ('*[1]');
                (my $rhs_unnegated) = $rhs->findnodes ('*[1]');
                if ($lhs_unnegated->exists ('self::And') && $rhs_unnegated->exists ('self::And')) {
                    (my $lhs_unnegated_lhs) = $lhs_unnegated->findnodes ('*[1]');
                    (my $lhs_unnegated_rhs) = $lhs_unnegated->findnodes ('*[2]');
                    (my $rhs_unnegated_lhs) = $rhs_unnegated->findnodes ('*[1]');
                    (my $rhs_unnegated_rhs) = $rhs_unnegated->findnodes ('*[2]');
                    if ($lhs_unnegated_rhs->exists ('self::Not') && $rhs_unnegated_rhs->exists ('self::Not')) {
                        (my $lhs_unnegated_rhs_unnegated) = $lhs_unnegated_rhs->findnodes ('*[1]');
                        (my $rhs_unnegated_rhs_unnegated) = $rhs_unnegated_rhs->findnodes ('*[1]');
                        my $p1 = render_semantic_content ($lhs_unnegated_lhs, @arguments);
                        my $q1 = render_semantic_content ($lhs_unnegated_rhs_unnegated, @arguments);
                        my $q2 = render_semantic_content ($rhs_unnegated_lhs, @arguments);
                        my $p2 = render_semantic_content ($rhs_unnegated_rhs_unnegated, @arguments);
                        if ($p1 eq $p2 && $q1 eq $q2) {
                            return "(${p1} <=> ${q1})";
                        } else {
                            return "(${lhs_rendered} & ${rhs_rendered})";
                        }
                    } else {
                        return "(${lhs_rendered} & ${rhs_rendered})";
                    }
                } else {
                    return "(${lhs_rendered} & ${rhs_rendered})";
                }
            } else {
                return "(${lhs_rendered} & ${rhs_rendered})";
            }
        } else {
            my $answer = '(';
            my $num_children = scalar @children;
            foreach my $i (1 .. $num_children) {
                my $child = $children[$i - 1];
                my $child_rendered = render_semantic_content ($child, @arguments);
                $answer .= $child_rendered;
                if ($i < $num_children) {
                    $answer .= ' & ';
                }
            }
            $answer .= ')';
            return $answer;
        }
    } elsif ($name eq 'Not') {
        (my $arg) = $node->findnodes ('*[1]');
        if (! defined $arg) {
            confess 'Not node lacks a child!';
        }
        my $rendered_arg = render_semantic_content ($arg, @arguments);
        if ($node->exists ('For/Not')) {
            (my $for) = $node->findnodes ('For');
            (my $typ) = $for->findnodes ('Typ');
            if (! defined $typ) {
                confess 'Typ node not found under a universal quantifier.';
            }
            my $var_name = undef;
            if ($for->hasAttribute ('vid')) {
                my $vid = $for->getAttribute ('vid');
                $var_name = "X${vid}";
            } else {
                my $vid = $for->findvalue ('count (ancestor::For[not(@vid)]) + 1');
                $var_name = "Y${vid}";
            }
            my $guard = render_guard ($var_name, $typ);
            (my $matrix) = $for->findnodes ('Not/*[1]');
            my $rendered_matrix = render_semantic_content ($matrix, @arguments);
            return "(? [${var_name}] : (${guard} & ${rendered_matrix}))";
        } elsif ($node->exists ('For')) {
            (my $for) = $node->findnodes ('For');
            (my $typ) = $for->findnodes ('Typ');
            if (! defined $typ) {
                confess 'Typ node not found under a universal quantifier.';
            }
            my $vid = $for->getAttribute ('vid');
            my $var_name = undef;
            if (defined $vid) {
                $var_name = "X${vid}";
            } else {
                my $vid = $for->findvalue ('count (ancestor::For[not(@vid)]) + 1');
                $var_name = "Y${vid}";
            }
            my $guard = render_guard ($var_name, $typ);
            (my $matrix) = $for->findnodes ('*[position() = last()]');
            my $rendered_matrix = render_semantic_content ($matrix, @arguments);
            return "(? [${var_name}] : (${guard} & (~ ${rendered_matrix})))";
        } elsif ($node->exists ('And[count(*) = 2]/Not')) {
            (my $conjunction) = $node->findnodes ('And');
            (my $lhs) = $conjunction->findnodes ('*[1]');
            (my $rhs) = $conjunction->findnodes ('*[2]');
            if ($lhs->exists ('self::Not') && $rhs->exists ('self::Not')) {
                (my $lhs_unnegated) = $lhs->findnodes ('*[1]');
                (my $rhs_unnegated) = $rhs->findnodes ('*[1]');
                my $lhs_unnegated_rendered = render_semantic_content ($lhs_unnegated, @arguments);
                my $rhs_unnegated_rendered = render_semantic_content ($rhs_unnegated, @arguments);
                return "(${lhs_unnegated_rendered} | ${rhs_unnegated_rendered})";
            } elsif ($lhs->exists ('self::Not')) {
                (my $lhs_unnegated) = $lhs->findnodes ('*[1]');
                my $lhs_unnegated_rendered = render_semantic_content ($lhs_unnegated, @arguments);
                my $rhs_rendered = render_semantic_content ($rhs, @arguments);
                return "(${rhs_rendered} => ${lhs_unnegated_rendered})";
            } elsif ($rhs->exists ('self::Not')) {
                (my $rhs_unnegated) = $rhs->findnodes ('*[1]');
                my $rhs_unnegated_rendered = render_semantic_content ($rhs_unnegated, @arguments);
                my $lhs_rendered = render_semantic_content ($lhs, @arguments);
                return "(${lhs_rendered} => ${rhs_unnegated_rendered})";
            } else {
                my $answer = '(~ ' . $rendered_arg . ')';
                return $answer;
            }
        } else {
            my $answer = '(~ ' . $rendered_arg . ')';
            return $answer;
        }
    } elsif ($name eq 'Const') {
        my $vid = get_attribute ($node, 'vid');
        return "X${vid}";
    } else {
        confess 'Unable to generate a rendering for nodes of kind \'', $name, '\'.';
    }
}

sub constants_under_node {
    my $node = shift;
    my @all_constants = $node->findnodes ('descendant::Const');
    my %constants = ();
    foreach my $const (@all_constants) {
        my $nr = get_nr_attribute ($const);
        $constants{$nr} = $const;
    }
    my @constants_no_dups = values %constants;
    return @constants_no_dups;
}

sub type_for_constant {
    my $constant = shift;
    my $vid = get_vid_attribute ($constant);
    my $binder_xpath = "preceding::*[\@vid = \"${vid}\" and (self::For or self::Typ)][1]";
    (my $binder) = $constant->findnodes ($binder_xpath);
    if (defined $binder) {
        my $binder_name = $binder->nodeName ();
        if ($binder_name eq 'For') {
            (my $typ) = $binder->findnodes ('Typ');
            if (! defined $typ) {
                confess 'For node lacks a Typ child.';
            }
            return $typ;
        } elsif ($binder_name eq 'Typ') {
            return $binder;
        } else {
            confess 'What kind of binder is', $LF, $binder->toString (), $LF, '?';
        }
    } else {
        confess 'Could find no binder for a constant with vid = ', $vid, ' using the XPath expression', $LF, $LF, '  ', $binder_xpath;
    }
}

sub render_proposition {
    my $proposition_node = shift;
    (my $content_node) = $proposition_node->findnodes ('*[position() = last()]');
    my $content = render_semantic_content ($content_node);
    my @constants = constants_under_node ($proposition_node);
    foreach my $constant (@constants) {
        my $typ = type_for_constant ($constant);
        my $var_name = undef;
        if ($typ->hasAttribute ('vid')) {
            my $vid = $typ->getAttribute ('vid');
            $var_name = "X${vid}";
        } else {
            my $vid = $typ->findvalue ('count (ancestor::For[not(@vid)]) + 1');
            $var_name = "Y${vid}";
        }
        my $guard = render_guard ($var_name, $typ);
        $content = "(! [${var_name}] : (${guard} => ${content}))";
    }
    return $content;
}

sub render_justified_theorem {
    my $justified_theorem_node = shift;
    (my $proposition_node) = $justified_theorem_node->findnodes ('Proposition[1]');
    if (! defined $proposition_node) {
        confess 'Proposition node not found under a justified theorem node';
    }
    return render_proposition ($proposition_node);
}

sub render_deftheorem {
    my $deftheorem_node = shift;
    (my $proposition_node) = $deftheorem_node->findnodes ('Proposition[1]');
    if (! defined $proposition_node) {
        confess 'Proposition node not found under a definitional theorem node';
    }
    return render_proposition ($proposition_node);
}

sub render_defmeaning {
    my $defmeaning_node = shift;
    (my $proposition_node) = $defmeaning_node->findnodes ('*[1]');
    return render_proposition ($proposition_node);
}

sub article_of_item {
    my $item = shift;
    if ($item =~ /\A ([a-z0-9_]+) [:] /) {
        return $1;
    } else {
        confess 'Cannot extract article for \'', $item, '\'.';
    }
}

sub kind_of_item {
    my $item = shift;
    if ($item =~ /\A [a-z0-9_]+ [:] ([^:]+) [:] /) {
        return $1;
    } else {
        confess 'Cannot extract item kind for \'', $item, '\'.';
    }
}

sub property_for_constructor {
    my $constructor_item = shift;
    if ($constructor_item =~ / [:] . constructor [:] \d+ [[] ([a-z]+) []] \z/) {
        return $1;
    } else {
        confess 'Cannot extract constructor property for \'', $constructor_item, '\'.';
    }
}

sub constructor_of_constructor_property {
    my $item = shift;
    if ($item =~ / ([a-z0-9_]+) [:] (.) constructor [:] (\d+) [[] [a-z]+ []] \z/) {
        return "${1}:${2}constructor:${3}";
    } else {
        confess 'Cannot extract constructor from \'', $item, '\'.';
    }
}

sub constructor_kind {
    my $constructor_item = shift;
    if ($constructor_item =~ / [:] (.) constructor [:] /) {
        return $1;
    } else {
        confess 'Cannot extract constructor kind from \'', $constructor_item, '\'.';
    }
}

sub cluster_kind {
    my $cluster_item = shift;
    if ($cluster_item =~ / [:] (.) cluster [:] /) {
        return $1;
    } else {
        confess 'Cannot extract cluster kind from \'', $cluster_item, '\'.';
    }
}

sub definiens_kind {
    my $definiens_item = shift;
    if ($definiens_item =~ / [:] (.) definiens [:] /) {
        return $1;
    } else {
        confess 'Cannot extract definiens kind from \'', $definiens_item, '\'.';
    }
}

sub nr_of_item {
    my $item = shift;
    if ($item =~ / [:] (\d+) \z/) {
        return $1;
    } elsif ($item =~ / \A requirement [:] (\d+) \z/ ) {
        return $1;
    } elsif (is_constructor_property_item ($item)) {
        my $constructor = constructor_of_constructor_property ($item);
        return nr_of_item ($constructor);
    } else {
        confess 'Cannot extract nr from \'', $item, '\'.';
    }
}

sub deftheorem_from_definiens {
    my $item = shift;
    if ($item =~ / \A ${article_name} [:] . definiens [:] /) {
        # local item
        my $source = source_of_item ($item);
        my $fragment_number = fragment_number ($source);
        my $target_deftheorem = undef;
        foreach my $x (keys %local_item_table) {
            my $y = $local_item_table{$x};
            if ($x =~ /\A ${PREFIX_LC}${fragment_number} [:] /) {
                if ($y =~ / [:] deftheorem [:] (\d+) \z/) {
                    $target_deftheorem = $y;
                    last;
                }
            }
        }
        if (! defined $target_deftheorem) {
            confess 'To what deftheorem does ', $item, ' correspond?';
        }
        return $target_deftheorem;
    } else {
        # non-local definiens
        my $mizfiles = $ENV{'MIZFILES'};
        if (! defined $mizfiles) {
            confess 'MIZFILES environment variable not defined.';
        }
        my $miztmp_dir = "${mizfiles}/miztmp";
        if (! -d $miztmp_dir) {
            confess 'miztmp dir missing under ', $mizfiles;
        }
        my $article = article_of_item ($item);
        my $item_xml = "${miztmp_dir}/${article}.xml1";
        if (! -e $item_xml) {
            confess 'Absolutized XML for ', $article, ' missing under ', $miztmp_dir;
        }
        my $item_xml_doc = parse_xml_file ($item_xml);
        my $item_xml_root = $item_xml_doc->documentElement ();
        my $kind = definiens_kind ($item);
        my $kind_uc = uc $kind;
        my $nr = nr_of_item ($item);
        my $definiens_xpath = "descendant::Definiens[\@constrkind = \"${kind_uc}\" and count (preceding::Definiens[\@constrkind = \"${kind_uc}\"]) + 1 = ${nr}]";
        (my $definiens_node) = $item_xml_root->findnodes ($definiens_xpath);
        if (! defined $definiens_node) {
            confess 'No suitable Definiens node found in ', $item_xml, ' using the XPath', $LF, $LF, '  ', $definiens_xpath;
        }
        my $nr_from_definiens = $definiens_node->getAttribute ('nr');
        if (! defined $nr_from_definiens) {
            confess 'nr missing from a Definiens node.';
        }
        return "${article}:deftheorem:${nr_from_definiens}";
    }
}

sub is_compatibility_item {
    my $item = shift;
    return ($item =~ / [[] compatibility []] \z/);
}

sub deftheorem_of_compatibility_item {
    my $compatibility_item = shift;
    if ($compatibility_item =~ / \A ([a-z0-9_]+) [:] deftheorem [:] (\d+) [[] compatibility []] \z/) {
        (my $article, my $nr) = ($1, $2);
        return "${article}:deftheorem:${nr}";
    } else {
        confess 'Cannot make sense of compatibility item \'', $compatibility_item, '\'.';
    }
}

sub tptp_name_for_item {
    my $item = shift;
    if (is_constructor_property_item ($item)) {
        my $constructor = constructor_of_constructor_property ($item);
        my $property = property_for_constructor ($item);
        my $article = article_of_item ($constructor);
        my $kind = constructor_kind ($constructor);
        my $nr = nr_of_item ($constructor);
        return "${property}_${kind}${nr}_${article}";
    } elsif (is_compatibility_item ($item)) {
        my $dt = deftheorem_of_compatibility_item ($item);
        my $nr = nr_of_item ($dt);
        my $aid = article_of_item ($dt);
        return "compatibility_d${nr}_${aid}";
    } elsif (is_requirement_item ($item)) {
        my $req_nr = nr_of_item ($item);
        return "requirement_${req_nr}";
    } else {
        my $article = article_of_item ($item);
        my $kind = kind_of_item ($item);
        my $nr = nr_of_item ($item);
        if ($kind eq 'theorem') {
            return "t${nr}_${article}";
        } elsif ($kind eq 'lemma') {
            return "l${nr}_${article}";
        } elsif ($kind eq 'deftheorem') {
            return "d${nr}_${article}";
        } elsif ($kind =~ / \A (.) cluster \z /) {
            return "${1}c${nr}_${article}";
        } elsif ($kind =~ /\A . definiens \z/) {
            my $target_deftheorem = deftheorem_from_definiens ($item);
            return tptp_name_for_item ($target_deftheorem);
        } else {
            confess 'How to make a TPTP name for \'', $item, '\'?';
        }
    }
}

sub render_choice_term {
    my $node = shift;
    (my $typ) = $node->findnodes ('Typ');
    if (! defined $typ) {
        confess 'Choice node lacks a Typ child.';
    }
    my $typ_aid = get_aid_attribute ($typ);
    my $typ_nr = get_nr_attribute ($typ);
    my $typ_kind = get_kind_attribute ($typ);
    my $typ_aid_lc = lc $typ_aid;
    my $typ_kind_lc = lc $typ_kind;
    my $typ_rendered = "${typ_kind_lc}${typ_nr}_${typ_aid_lc}";
    return "epsilon_${typ_rendered}";
}

sub render_choices {
    my $node = shift;
    my $choice_xpath = 'descendant::Choice';
    my @choices = ();
    if ($node->exists ($choice_xpath)) {
        my @choice_nodes = $node->findnodes ($choice_xpath);
        foreach my $choice_node (@choice_nodes) {
            (my $typ) = $choice_node->findnodes ('Typ');
            if (! defined $typ) {
                confess 'Choice lacks a Typ child.';
            }
            (my $cluster) = $typ->findnodes ('Cluster');
            if (! defined $cluster) {
                confess 'Typ node lacks a Cluster child.';
            }
            my $choice_term = render_choice_term ($choice_node);
            # the choice term belongs to the type and has the adjectives
            my @statements = ();
            my $guard = render_guard ($choice_term, $typ);
            push (@statements, $guard);
            my $answer = '(';
            my $num_statements = scalar @statements;
            foreach my $i (1 .. $num_statements) {
                my $statement = $statements[$i - 1];
                $answer .= $statement;
                if ($i < $num_statements) {
                    $answer .= ' & ';
                }
            }
            $answer .= ')';
            push (@choices, "fof(dt_${choice_term},axiom,(${answer})).");
        }
    } else {
        return;
    }
    return @choices;
}

Readonly my %CONSTRUCTOR_PROPERTY_MAKERS =>
    (
        'commutativity' => \&render_commutativity,
        'idempotence' => \&render_idempotence,
        'symmetry' => \&render_symmetry,
        'asymmetry' => \&render_asymmetry,
        'reflexivity' => \&render_reflexivity,
        'irreflexivity' => \&render_irreflexivity,
        'existence' => \&render_existence,
        'uniqueness' => \&render_uniqueness,
        'coherence' => \&render_coherence,
    );

sub render_tptp_constructor {
    my $constructor = shift;
    my $kind = $constructor->getAttribute ('kind');
    my $aid = $constructor->getAttribute ('aid');
    my $nr = $constructor->getAttribute ('nr');
    if (! defined $kind) {
        confess 'Constructor node lacks a kind attribute.';
    }
    if (! defined $aid) {
        confess 'Constructor node lacks an aid attribute.';
    }
    if (! defined $nr) {
        confess 'Constructor node lacks a nr attribute.';
    }
    my $aid_lc = lc $aid;
    my $kind_lc = lc $kind;
    return "${kind_lc}${nr}_${aid_lc}";
}

sub render_guard {
    my $variable = shift;
    my $type = shift;
    my $type_aid = get_aid_attribute ($type);
    my $type_nr = get_nr_attribute ($type);
    my $type_kind = get_kind_attribute ($type);
    my $type_kind_lc = lc $type_kind;
    my $type_aid_lc = lc $type_aid;
    my $type_rendered = "${type_kind_lc}${type_nr}_${type_aid_lc}";
    my $guard = "(${type_rendered}(${variable})";
    (my $cluster_node) = $type->findnodes ('Cluster'); # the lower cluster
    my @adjectives = $cluster_node->findnodes ('Adjective');
    my $num_adjectives = scalar @adjectives;
    foreach my $i (1 .. $num_adjectives) {
        my $adjective = $adjectives[$i - 1];
        my $adj_aid = get_aid_attribute ($adjective);
        my $adj_nr = get_nr_attribute ($adjective);
        my $adj_aid_lc = lc $adj_aid;
        my $bare_adj = "v${adj_nr}_${adj_aid_lc}";
        if (($adjective->hasAttribute ('value')) && ($adjective->getAttribute ('value') eq 'false')) {
            $guard .= " & (~ ${bare_adj}(${variable}))";
        } else {
            $guard .= " & ${bare_adj}(${variable})";
        }
    }
    $guard .= ')';
    return $guard;
}

sub render_idempotence {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    if (scalar @arg_types != 2) {
        confess 'How to render idempotence for a constructor that does not accept exactly 2 arguments?';
    }
    my $var_1 = 'X1';
    my $typ_1 = $arg_types[0];
    my $guard = render_guard ($var_1, $typ_1);
    return "(! [${var_1}] : (${guard} => (${constructor_name}(${var_1},${var_1}) = ${var_1})))";
}

sub render_commutativity {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    if (scalar @arg_types != 2) {
        confess 'How to render commutativity for a constructor that does not accept exactly 2 arguments?';
    }
    my $typ_1 = $arg_types[0];
    my $typ_2 = $arg_types[1];
    my $var_1 = 'X1';
    my $var_2 = 'X2';
    my $guard_1 = render_guard ($var_1, $typ_1);
    my $guard_2 = render_guard ($var_2, $typ_2);
    return "(! [${var_1},${var_2}] : ((${guard_1} & ${guard_2}) => (${constructor_name}(${var_1},${var_2}) = ${constructor_name}(${var_2},${var_1}))))";
}

sub render_existence {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    (my $result_type) = $constructor->findnodes ('*[position() = last()]');
    return '$true';
}

sub render_uniqueness {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    (my $result_type) = $constructor->findnodes ('*[position() = last()]');
    return '$true';
}

sub render_coherence {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    return '$true';
}

sub render_symmetry {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    if (scalar @arg_types != 2) {
        confess 'How to render symmetry for a constructor that does not accept exactly 2 arguments?';
    }
    my $typ_1 = $arg_types[0];
    my $typ_2 = $arg_types[1];
    my $var_1 = 'X1';
    my $var_2 = 'X2';
    my $guard_1 = render_guard ($var_1, $typ_1);
    my $guard_2 = render_guard ($var_2, $typ_2);
    return "(! [${var_1},${var_2}] : ((${guard_1} & ${guard_2}) => (${constructor_name}(${var_1},${var_2}) <=> ${constructor_name}(${var_2},${var_1}))))";
}

sub render_asymmetry {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    if (scalar @arg_types != 2) {
        confess 'How to render asymmetry for a constructor that does not accept exactly 2 arguments?';
    }
    my $typ_1 = $arg_types[0];
    my $typ_2 = $arg_types[1];
    my $var_1 = 'X1';
    my $var_2 = 'X2';
    my $guard_1 = render_guard ($var_1, $typ_1);
    my $guard_2 = render_guard ($var_2, $typ_2);
    return "(! [${var_1},${var_2}] : ((${guard_1} & ${guard_2}) => (${constructor_name}(${var_1},${var_2}) => (~ ${constructor_name}(${var_2},${var_1})))))";
}

sub render_reflexivity {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    if (scalar @arg_types != 2) {
        confess 'How to render reflexivity for a constructor that does not accept exactly 2 arguments?';
    }
    my $typ_1 = $arg_types[0];
    my $var_1 = 'X1';
    my $guard_1 = render_guard ($var_1, $typ_1);
    return "(! [${var_1}] : (${guard_1} => (${constructor_name}(${var_1},${var_1}))))";
}

sub render_irreflexivity {
    my $constructor = shift;
    my $constructor_name = render_tptp_constructor ($constructor);
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    if (scalar @arg_types != 2) {
        confess 'How to render reflexivity for a constructor that does not accept exactly 2 arguments?';
    }
    my $typ_1 = $arg_types[0];
    my $var_1 = 'X1';
    my $guard = render_guard ($var_1, $typ_1);
    return "(! [${var_1}] : (${guard} => (~ ${constructor_name}(${var_1},${var_1}))))";
}

sub formulate_property_for_constructor {
    my $constructor = shift;
    my $property = shift;
    my $ref = $CONSTRUCTOR_PROPERTY_MAKERS{$property};
    if (! defined $ref) {
        confess 'How to render the constructor property \'', $property, '\'?';
    }
    my $content = $ref->($constructor);
    return $content;
}

Readonly my %REQUIREMENT_CONTENTS =>
    (
        1 => ['(! [X] : (m1_hidden(X)))',
              '(! [X] : (m2_hidden(X)))'],
        2 => ['(! [X] : (m1_hidden(X) => m2_hidden(X)))'],
        3 => ['(! [X,Y] : ((m2_hidden(X) & m2_hidden(Y) & (! [Z] : (r2_hidden(Z,X) <=> r2_hidden(Z,Y)))) => X = Y))'],
        4 => ['(! [X,Y] : (r2_hidden(X,Y) => (~ r2_hidden(Y,X))))',
              '(! [X] : (~r2_hidden(X,X)))'],
        5 => ['v1_xboole_0(k1_xboole_0)',
              '(! [X] : (v1_xboole_0(X) => X = k1_xboole_0))',
              '(! [X] : (v1_xboole_0(X) <=> (! [Y] : (~ r2_hidden(Y,X)))))'],
        6 => ['(! [X] : (~ r2_hidden(X,k1_xboole_0)))',
              '(! [X] : ((! [Y] : (~r2_hidden(Y,X))) => (X = k1_xboole_0)))'],
        17 => ['(! [X] : (k2_xboole_0(X,k1_xboole_0) = X))',
               '(! [X] : (k2_xboole_0(k1_xboole_0,X) = X))',
               '(! [X] : (k3_xboole_0(X,k1_xboole_0) = X))',
               '(! [X] : (k4_xboole_0(X,k1_xboole_0) = X))',
               '(! [X] : (k4_xboole_0(k1_xboole_0,X) = X))',
               '(! [X] : (k5_xboole_0(X,X) = k1_xboole_0))'],
        18 => ['(! [X] : (k3_xboole_0(X,k1_xboole_0) = k1_xboole_0))'],
        19 => ['(! [X] : (k4_xboole_0(X,k1_xboole_0) = X))',
               '(! [X] : (k4_xboole_0(k1_xboole_0,X) = k1_xboole_0))'],
    );

sub render_requirement {
    my $nr = shift;
    my @results = ();
    if (defined $REQUIREMENT_CONTENTS{$nr}) {
        my @contents = @{$REQUIREMENT_CONTENTS{$nr}};
        push (@results, @contents);
    } else {
        confess 'What is requirement ', $nr, '?';
    }
    my @named_results = ();
    foreach my $i (1 .. @results) {
        my $formula = $results[$i - 1];
        push (@named_results, "fof(requirement_${nr}_${i},axiom,${formula}).");
    }
    return @named_results;
}

sub render_non_local_item {
    my $item = shift;
    my $article = article_of_item ($item);
    my $nr = nr_of_item ($item);
    my $tptp_name = tptp_name_for_item ($item);
    my @results = ();
    if ($item eq 'hidden:rconstructor:1[symmetry]') {
        push (@results, "fof(${tptp_name},axiom,(! [X,Y] : (X = Y => Y = X))).");
    } elsif ($item eq 'hidden:rconstructor:1[reflexivity]') {
        push (@results, "fof(${tptp_name},axiom,(! [X] : (X = X))).");
    } elsif (is_requirement_item ($item)) {
        my @requirements = render_requirement ($nr);
        push (@results, @requirements);
    } else {
        my $mizfiles = $ENV{'MIZFILES'};
        if (! defined $mizfiles) {
            confess 'MIZFILES environment variable not defined.';
        }
        my $miztmp_dir = "${mizfiles}/miztmp";
        if (! -d $miztmp_dir) {
            confess 'miztmp dir missing under ', $mizfiles;
        }
        my $item_xml = "${miztmp_dir}/${article}.xml1";
        if (! -e $item_xml) {
            confess 'Absolutized XML for ', $article, ' missing under ', $miztmp_dir;
        }
        my $item_xml_doc = parse_xml_file ($item_xml);
        my $item_xml_root = $item_xml_doc->documentElement ();
        if (is_theorem_item ($item)) {
            my $xpath = "descendant::JustifiedTheorem[${nr}]";
            (my $theorem_node) = $item_xml_root->findnodes ($xpath);
            if (! defined $theorem_node) {
                confess 'Could not find theorem ', $nr, ' in ', $item_xml;
            }
            my $content = render_justified_theorem ($theorem_node);
            push (@results, "fof(${tptp_name},theorem,${content}).");
            (my $proposition_node) = $theorem_node->findnodes ('Proposition[1]');
            if (! defined $proposition_node) {
                confess 'No Proposition node found under the Coherence node in ', $item_xml;
            }
            my @choices = render_choices ($proposition_node);
            push (@results, @choices);
        } elsif (is_lemma_item ($item)) {
            my $xpath = "Proposition[count (preceding-sibling::Proposition) + 1 = ${nr}]";
            (my $proposition_node) = $item_xml_root->findnodes ($xpath);
            if (! defined $proposition_node) {
                confess 'Could not find lemma ', $nr, ' in ', $item_xml;
            }
            my $content = render_proposition ($proposition_node);
            push (@results, "fof(${tptp_name},lemma,${content}).");
            my @choices = render_choices ($proposition_node);
            push (@results, @choices);
        } elsif (is_deftheorem_item ($item)) {
            my $xpath = "descendant::DefTheorem[${nr}]";
            (my $deftheorem_node) = $item_xml_root->findnodes ($xpath);
            if (! defined $deftheorem_node) {
                confess 'Could not find theorem ', $nr, ' in ', $item_xml;
            }
            my $content = render_deftheorem ($deftheorem_node);
            push (@results, "fof(${tptp_name},definition,${content}).");
            (my $proposition_node) = $deftheorem_node->findnodes ('Proposition[1]');
            if (! defined $proposition_node) {
                confess 'No Proposition node found under the Coherence node in ', $item_xml;
            }
            my @choices = render_choices ($proposition_node);
            push (@results, @choices);
        } elsif (is_definiens_item ($item)) {
            my $kind = definiens_kind ($item);
            my $kind_uc = uc $kind;
            my $nr = nr_of_item ($item);
            my $definiens_xpath = "descendant::Definiens[\@constrkind = \"${kind_uc}\" and count (preceding::Definiens[\@constrkind = \"${kind_uc}\"]) + 1 = ${nr}]";
            (my $definiens_node) = $item_xml_root->findnodes ($definiens_xpath);
            if (! defined $definiens_node) {
                confess 'No suitable Definiens node found in ', $item_xml, ' using the XPath', $LF, $LF, '  ', $definiens_xpath;
            }
            my $constrnr = $definiens_node->getAttribute ('constrnr');
            my $deftheorem_xpath = "following-sibling::DefTheorem[\@constrkind = \"${kind_uc}\" and \@constrnr = \"${constrnr}\"]";
            (my $deftheorem_node) = $definiens_node->findnodes ($deftheorem_xpath);
            if (! defined $deftheorem_node) {
                confess 'No DefTheorem node found following the Definiens node for ', $item_xml;
            }
            my $content = render_deftheorem ($deftheorem_node);
            push (@results, "fof(${tptp_name},definition,${content}).");
            (my $proposition_node) = $deftheorem_node->findnodes ('Proposition[1]');
            if (! defined $proposition_node) {
                confess 'No Proposition node found under the Coherence node in ', $item_xml;
            }
            my @choices = render_choices ($proposition_node);
            push (@results, @choices);
        } elsif (is_cluster_item ($item)) {
            my $kind = cluster_kind ($item);
            if ($kind eq 'f') {
                my $cluster_xpath = "descendant::FCluster[${nr}]";
                (my $cluster_node) = $item_xml_root->findnodes ($cluster_xpath);
                if (! defined $cluster_node) {
                    confess 'FCluster not found in ', $item_xml, $LF, $LF, '  ', $cluster_xpath
                }
                (my $registration_node) = $cluster_node->findnodes ('ancestor::Registration');
                my @arg_types = $registration_node->findnodes ('preceding-sibling::Let/*');
                warn '', scalar @arg_types, ' arg types for an fcluster';
                (my $coherence_node) = $cluster_node->findnodes ('following-sibling::*[1][self::Coherence]');
                if (! defined $coherence_node) {
                    confess 'Coherence node missing for a cluster at ', $item_xml;
                }
                (my $proposition_node) = $coherence_node->findnodes ('Proposition[1]');
                if (! defined $proposition_node) {
                    confess 'No Proposition node found under the Coherence node in ', $item_xml;
                }
                my $rendered_proposition = render_proposition ($proposition_node, @arg_types);
                push (@results, "fof(${kind}c${nr}_${article},theorem,${rendered_proposition}).");
                my @choices = render_choices ($proposition_node);
                push (@results, @choices);
            } elsif ($kind eq 'r') {
                my $cluster_xpath = "descendant::RCluster[\@nr = \"${nr}\"]";
                (my $cluster_node) = $item_xml_root->findnodes ($cluster_xpath);
                if (! defined $cluster_node) {
                    confess 'RCluster not found in ', $item_xml;
                }
                (my $existence_node) = $cluster_node->findnodes ('following-sibling::*[1][self::Existence]');
                if (! defined $existence_node) {
                    confess 'Existence node missing for a cluster at ', $item_xml;
                }
                (my $proposition_node) = $existence_node->findnodes ('Proposition[1]');
                if (! defined $proposition_node) {
                    confess 'No Proposition node found under the Existence node in ', $item_xml;
                }
                my $rendered_proposition = render_proposition ($proposition_node);
                push (@results, "fof(${kind}c${nr}_${article},theorem,${rendered_proposition}).");
                my @choices = render_choices ($proposition_node);
                push (@results, @choices);
            } else {
                confess 'How to handle clusters of kind \'', $kind, '\'?';
            }

        } elsif (is_constructor_property_item ($item)) {
            # are these always found in the same article?
            my $constructor = constructor_of_constructor_property ($item);
            my $constructor_nr = nr_of_item ($constructor);
            my $constructor_aid = article_of_item ($constructor);
            my $kind = constructor_kind ($constructor);
            my $kind_uc = uc $kind;
            my $constructor_xpath = "descendant::Constructor[\@kind = \"${kind_uc}\" and \@nr = \"${nr}\"]";
            (my $constructor_node) = $item_xml_root->findnodes ($constructor_xpath);
            if (! defined $constructor_node) {
                confess 'We failed to find a constructor node in ', $item_xml, ' matching the XPath expression', $LF, $LF, '  ', $constructor_xpath;
            }
            my $property = property_for_constructor ($item);
            my $content = formulate_property_for_constructor ($constructor_node, $property);
            push (@results, "fof(${tptp_name},theorem,${content}).");

            # We may need to add the definition of the constructor
            my $deftheorem_xpath = "descendant::DefTheorem[\@constrkind = \"${kind_uc}\"][${constructor_nr}]";
            (my $deftheorem_node) = $item_xml_root->findnodes ($deftheorem_xpath);
            if (! defined $deftheorem_node) {
                confess 'No DefTheorem node for constructor ', $kind_uc, $constructor_nr, ' in ', $item_xml;
            }
            my $deftheorem_content = render_deftheorem ($deftheorem_node);
            my $deftheorem = "fof(d${constructor_nr}_${constructor_aid},definition,${deftheorem_content}).";
            push (@results, $deftheorem);

            # it seems we don't need to worry about choice nodes in this case
            # my @choices = render_choices ($proposition_node);
            # push (@results, @choices);
        } else {
            confess 'How to extract ', $item, '?';
        }
    }
    return @results;
}

sub render_scheme_instance {
    my $proposition = shift;
    return render_proposition ($proposition);
}

sub extract_schemes {
    my $node = shift;
    my @formulas = ();
    my @from_nodes = $node->findnodes ('descendant::From');
    my @proposition_nodes = $node->findnodes ('descendant::Proposition[following-sibling::*[1][self::From]]');
    my %schemes_by_article = ();
    my $num_scheme_instances = scalar @proposition_nodes;
    if (scalar @from_nodes != scalar @proposition_nodes) {
        confess 'There are ', scalar @from_nodes, ' From node(s) but ', scalar @proposition_nodes, ' scheme-justified Proposition nodes.';
    }
    foreach my $i (1 .. scalar @proposition_nodes) {
        my $proposition_node = $proposition_nodes[$i - 1];
        my $from_node = $from_nodes[$i - 1];
        my $aid = $from_node->getAttribute ('aid');
        my $aid_lc = lc $aid;
        if ($aid_lc =~ /\A ${PREFIX_LC} \d+ \z/) {
            $aid_lc = $article_name;
        }
        my $nr = $from_node->getAttribute ('absnr');
        if (! defined $schemes_by_article{$aid}) {
            $schemes_by_article{$aid} = 1;
        }
        my $instance_nr = $schemes_by_article{$aid};
        $schemes_by_article{$aid} = $instance_nr + 1;
        my $content = render_scheme_instance ($proposition_node);
        my $formula = "fof(${aid_lc}_s${nr}_${instance_nr},theorem,${content}).";
        push (@formulas, $formula);
    }
    return @formulas;
}

sub conjecturify_formula_in_problem {
    my $formula_name = shift;
    my @formulas = @_;
    my @new_problem = ();
    my $encountered = 0;
    foreach my $formula (@formulas) {
        if ($formula =~ / \A fof [(] ${formula_name} [,] [a-z]+ [,] (.+) [)] [.] \z/) {
            my $content = $1;
            my $new_formula = "fof(${formula_name},conjecture,${content}).";
            push (@new_problem, $new_formula);
            $encountered = 1;
        } else {
            push (@new_problem, $formula);
        }
    }
    if (! $encountered) {
        confess 'We were asked to turn', $LF, $LF, '  ', $formula_name, $LF, $LF, 'into a conjecture, but that formula does not occur in the given list of formulas:', $LF, $LF, Dumper (@formulas);
    }
    return @new_problem;
}

sub problem_for_item {
    my $item = shift;
    my $source = source_of_item ($item);
    my $fragment_number = fragment_number ($source);
    my $fragment_xml = "${text_dir}/${PREFIX_LC}${fragment_number}.xml1";
    my $fragment_doc = parse_xml_file ($fragment_xml);
    my $fragment_root = $fragment_doc->documentElement ();
    my $item_label = tptp_name_for_item ($item);
    my @problem = render_non_local_item ($item);
    @problem = conjecturify_formula_in_problem ($item_label, @problem);
    my @deps = @{$resolved_dependencies{$item}};
    my @scheme_deps = ();
    foreach my $dep (@deps) {
        if (is_scheme_item ($dep)) {
            push (@scheme_deps, $dep);
        } elsif (has_semantic_content ($dep)) {
            my @rendered = render_non_local_item ($dep);
            push (@problem, @rendered);
        }
    }
    my @formulas_from_schemes = extract_schemes ($fragment_root);
    push (@problem, @formulas_from_schemes);
    # remove any potential duplicates
    my %formulas = ();
    foreach my $formula (@problem) {
        if ($formula =~ /\A fof [(] ([^,]+) [,] /) {
            my $name = $1;
            $formulas{$name} = $formula;
        } else {
            confess 'Cannot make sense of TPTP formula \'', $formula, '\'.';
        }
    }
    return values %formulas;
}

sub is_problematic_item {
    my $item = shift;
    if (has_semantic_content ($item)) {
        if (is_scheme_item ($item)) {
            return 0;
        } elsif (is_deftheorem_item ($item)) {
            return 0;
        } elsif (is_definiens_item ($item)) {
            return 0;
        } else {
            return 1;
        }
    } else {
        return 0;
    }
}

# Make the problems.  Trash it if it already exists.
my $problem_dir = "${article_dir}/problems";
if (-d $problem_dir) {
    rmtree ($problem_dir)
        or confess 'Cannot delete ', $problem_dir;
}
mkdir $problem_dir
    or confess 'Cannot make the directory ', $problem_dir, ' .';

foreach my $item (keys %resolved_dependencies) {
    if (is_problematic_item ($item)) {
        my @problem = problem_for_item ($item);
        my $item_label = tptp_name_for_item ($item);
        my $problem_file = "${problem_dir}/${item_label}";
        open (my $problem_fh, '>', $problem_file)
            or confess 'Unable to open output filehandle at ', $problem_file;
        foreach my $formula (@problem) {
            say {$problem_fh} $formula;
        }
        close $problem_fh
            or confess 'Unable to close output filehandle for ', $problem_file;
    }
}

exit 0;

__END__

=head1 ITEMIZED-ARTICLE-DEPENDENCIES

itemized-article-dependencies.pl - Print the dependencies of an itemized Mizar article

=head1 SYNOPSIS

itemized-article-dependencies.pl [options] directory

Interpret the supplied directory as the result of itemizing a Mizar
article, and print the dependencies of each of the microarticles under
the supplied directory.

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing as we're doing it.

=item B<--script-home=DIR>

The directory in which we will look for the needed auxiliary scripts.

=item B<--stylesheet-home=DIR>

The directory in which we will look for any needed auxiliary
stylesheets.

=back

=head1 DESCRIPTION

B<itemized-article-dependencies.pl> will consult the given article as
well as its environment to determine the article's dependencies, which
it prints (one per line) to standard output.

=head1 REQUIRED ARGUMENTS

It is necessary to supply a directory as the one and only argument of
this program.  The directory is supposed to be the result of itemizing
a Mizar article.  It should have the structure of a multi-article
Mizar development: there should be subdirectories 'prel', 'dict', and
'text'.

=head1 SEE ALSO

=over 8

=item F<dependencies.pl>

=item L<http://mizar.org/>

=back

=cut
