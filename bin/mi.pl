#!/usr/bin/perl

use warnings;
use strict;
use File::Basename qw(basename);
use Getopt::Long;
use Pod::Usage;
use File::Temp qw(tempfile);
use Carp qw(croak carp confess);
use XML::LibXML;
use List::MoreUtils qw(none);
use Regexp::DefaultFlags;
use IPC::Run qw(run start);
use charnames qw( :full ); # for referring to characters in regular expressions
use Readonly;
use FindBin qw($RealBin);
use lib "$RealBin/../src/perl/";
use Data::Dumper;

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
                }
                if (! defined $cluster_kind) {
                    confess 'Unable to make sense of a cluster in ', $file;
                }
                my $aid = $cluster->getAttribute ('aid');
                my $nr = $cluster->getAttribute ('nr');
                if (! defined $aid) {
                    confess 'Cluster node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Cluster node in ', $file, ' lacks an nr attribute.';
                }
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
                my $definiens_kind = $definiens->getAttribute ('constrkind');
                my $aid = $definiens->getAttribute ('aid');
                my $nr = $definiens->getAttribute ('absconstrnr');
                if (! defined $definiens_kind) {
                    confess 'Unable to make sense of a definiens in ', $file;
                }
                if (! defined $aid) {
                    confess 'Definiens node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Definiens node in ', $file, ' lacks an nr attribute.';
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
                my $nr = $definiens->getAttribute ('absconstrnr');
                if (! defined $definiens_kind) {
                    confess 'Unable to make sense of a definiens in ', $file;
                }
                if (! defined $aid) {
                    confess 'Definiens node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Definiens node in ', $file, ' lacks an nr attribute.';
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
        } elsif ($file =~ / [.] dfx \z /) {
            next;
        } elsif ($file =~ / [.] dfx1 \z /) {
            my $dfs_doc = parse_xml_file ($file);
            my $dfs_root = $dfs_doc->documentElement ();
            my @definientia = $dfs_root->findnodes ('descendant::Definiens');
            foreach my $definiens (@definientia) {
                my $definiens_kind = $definiens->getAttribute ('constrkind');
                my $aid = $definiens->getAttribute ('aid');
                my $nr = $definiens->getAttribute ('absconstrnr');
                if (! defined $definiens_kind) {
                    confess 'Unable to make sense of a definiens in ', $file;
                }
                if (! defined $aid) {
                    confess 'Definiens node in ', $file, ' lacks an aid attribute.';
                }
                if (! defined $nr) {
                    confess 'Definiens node in ', $file, ' lacks an nr attribute.';
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
            foreach my $i (1 .. scalar @ere_lines) {
                my $ere_line = $ere_lines[$i - 1];
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
        } else {
            my $resolved = $local_item_table{$item};
            if (defined $resolved) {
                return $resolved;
            } elsif ($item =~ / [:] theorem [:] [1] \z/) {
                # is this actually a deftheorem in disguise?
                my $deftheorem_item = "${PREFIX_LC}${fragment_number}:deftheorem:1";
                my $resolved_deftheorem = $local_item_table{$deftheorem_item};
                if (defined $resolved_deftheorem) {
                    return $resolved_deftheorem;
                } else {
                    confess 'Error: could not resolve the theorem item \'', $item, '\'.', $LF, 'The local item table looks like:', $LF, Dumper (%local_item_table);
                }
            } else {
                confess 'Error: could not resolve \'', $item, '\'.', $LF, 'The local item table looks like:', $LF, Dumper (%local_item_table);
            }
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
            my $key = 'theorem';
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
                        carp 'registering ', $condition_or_property, ' under ', $local_name;
                    } else {
                        warn 'could not find property ', $property, ' for ', $kind, 'constructor ', $index;
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

warn 'OK, the table now looks like this:', $LF, Dumper (%local_item_table);

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
            }
            if (! defined $property) {
                confess 'To what property does \'', $label, '\' correspond?';
            }
            $property = lc $property;
            # Now find the item that uses this one
            my $this_fragment = "${PREFIX_UC}${fragment_number}";
            warn 'working on property ', $property, ' of fragment ', $this_fragment;
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
                warn 'target fragment index is: ', $target_fragment_index;
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
                    warn 'HEY';
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
                    warn 'next target fragment index: ', $target_fragment_index;
                    my $constructor_item = "${PREFIX_LC}${target_fragment_index}:kconstructor:1";
                    my $resolved_item = $local_item_table{$constructor_item};
                    if (! defined $resolved_item) {
                        confess 'Error: unable to find', $LF, $LF, '  ', $constructor_item, $LF, $LF, 'in the local item table, which is:', $LF, $LF, Dumper (%local_item_table);
                    }
                    my $item = "${resolved_item}[${property}]";
                    my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                    $local_item_table{$local_item} = $item;
                    warn 'HEY again: registering ', $local_item, ' under ', $item;
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
                    warn 'the item coming from fragment ', $target_fragment_index, ' is ', $other_item;
                    if ($other_item =~ / [[] [a-z]+ []] \z/) {
                        my $item = $other_item;
                        my $local_item = "${PREFIX_LC}${target_fragment_index}:theorem:1";
                        $local_item_table{$local_item} = $item;
                        warn 'Part 2: registering ', $item, ' under ', $local_item;
                    } else {
                        my $item = "${other_item}[${property}]";
                        my $local_item = "${PREFIX_LC}${fragment_number}:theorem:1";
                        $local_item_table{$local_item} = $item;
                        warn 'Part 2: registering ', $item, ' under ', $local_item;
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

warn 'After dealing with constructor properties, the table now looks like this:', $LF, Dumper (%local_item_table);

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

# Sanity checks
# Warn about items that depend on nothing
foreach my $item (keys %resolved_dependencies) {
    my @deps = @{$resolved_dependencies{$item}};
    my $num_deps = scalar @deps;
    if ($num_deps == 0) {
        my $source_item = source_of_item ($item);
        carp $item, ' (coming from ', $source_item, ') depends on nothing.';
    }

}

# Every local item appearing as needed is "defined"
my %needed = ();
foreach my $item (keys %resolved_dependencies) {
    my @deps = @{$resolved_dependencies{$item}};
    foreach my $dep (@deps) {
        if ($dep =~ / \A ${article_name} [:] /) {
            $needed{$dep} = 0;
        }
    }
}
foreach my $item (keys %needed) {
    if (! defined $resolved_dependencies{$item}) {
        carp $item, ' appears as a dependency, but it is not "defined" in the current table.';
    }
}

# Every fragment yields an item that appears in the local item table
my @local_items = keys %local_item_table;
my @miz_files = glob "${text_dir}/${PREFIX_LC}*.miz";
foreach my $miz_file (@miz_files) {
    my $fragment_number = fragment_number ($miz_file);
    if (none { $_ =~ / \A ${PREFIX_LC}${fragment_number} [:] / } @local_items) {
        carp 'Fragment ', $fragment_number, ' produced nothing that was registered in the local item table.';
    }
}

# Warn about unused items
foreach my $item (keys %resolved_dependencies) {
    if (! (defined $needed{$item})) {
        my $source = source_of_item ($item);
        carp $item, ' (coming from ', $source, ') is never used.';
    }
}

# Dump the table
print Dumper (%local_item_table);
foreach my $item (keys %resolved_dependencies) {
    my @deps = @{$resolved_dependencies{$item}};
    print $item;
    foreach my $dep (@deps) {
        print ' ', $dep;
    }
    print "\N{LF}";
}

sub has_semantic_content {
    my $item = shift;
    return 0;
}

sub render_tptp {
    my $node = shift;
    return 'false';
}

# Make the problems
my $problem_dir = "${article_dir}/problems";
if (! -d $problem_dir) {
    mkdir $problem_dir
        or confess 'Cannot make the directory ', $problem_dir, ' .';
}
foreach my $item (keys %resolved_dependencies) {
    if ($item =~ /\A ${article_name} [:] theorem [:] (\d+) \z /) {
        my $theorem_number = $1;
        my $source = source_of_item ($item);
        my $fragment_number = fragment_number ($source);
        my $fragment_xml = "${text_dir}/${PREFIX_LC}${fragment_number}.xml1";
        my $fragment_doc = parse_xml_file ($fragment_xml);
        my $fragment_root = $fragment_doc->documentElement ();
        (my $theorem_node) = $fragment_root->findnodes ('descendant::JustifiedTheorem');
        if (defined $theorem_node) {
            (my $proposition_node) = $theorem_node->findnodes ('Proposition');
            if (! defined $proposition_node) {
                confess 'Error: where is the Proposition node in ', $fragment_xml, '?';
            }
            (my $theorem_content) = $proposition_node->findnodes ('*[1]');
            if (! defined $theorem_content) {
                confess 'Error: what is the content of the theorem in ', $fragment_xml, '?';
            }
            my $theorem_rendering = render_tptp ($theorem_content);
            my @problem = ("fof(${article_name}_t${theorem_number},conjecture,${theorem_rendering}).");
            my @deps = @{$resolved_dependencies{$item}};
            foreach my $dep (@deps) {
                if (has_semantic_content ($dep)) {
                    if ($dep =~ / \A ${article_name} [:] \z/) {
                        my $rendered = render_local_item ($dep);
                        push (@problem, $rendered);
                    } else {
                        my $rendered = render_non_local_item ($dep);
                        push (@problem, $rendered);
                    }
                }
            }
            my $theorem_file = "${problem_dir}/t${theorem_number}";
            open (my $theorem_fh, '>', $theorem_file)
                or confess 'Unable to open output filehandle at ', $theorem_file;
            foreach my $formula (@problem) {
                say {$theorem_fh} $formula;
            }
            close $theorem_fh
                or confess 'Unable to close output filehandle for ', $theorem_file;
        }
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
