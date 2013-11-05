#!/usr/bin/perl

use warnings;
use strict;
use feature 'say';
use File::Basename qw(basename);
use File::Path qw(rmtree);
use File::Spec::Functions qw(catfile catdir);
use Getopt::Long;
use Pod::Usage;
use Carp qw(croak carp confess);
use XML::LibXML;
use List::Util qw(shuffle);
use List::MoreUtils qw(none first_index all any);
use Regexp::DefaultFlags;
use IPC::Run qw(run start);
use charnames qw( :full ); # for referring to characters in regular expressions
use Readonly;
use FindBin qw($RealBin);
use lib catdir ($RealBin, '..', 'src', 'perl');
use Data::Dumper;
use Parallel::Loops;

use Utils qw(parse_xml_file);

Readonly my $LF => "\N{LF}";
Readonly my $PREFIX_LC => lc 'ckb';
Readonly my $PREFIX_UC => uc $PREFIX_LC;

Readonly my $MAX_PROCESSES => 1;
my $pl = Parallel::Loops->new ($MAX_PROCESSES);

my $opt_verbose = 0;
my $opt_man = 0;
my $opt_help = 0;
my $opt_paranoid = 0;

GetOptions(
    'help|?'   => \$opt_help,
    'man'      => \$opt_man,
    'verbose'  => \$opt_verbose,
    'paranoid' => \$opt_paranoid,
) or pod2usage(2);
pod2usage(1) if $opt_help;
pod2usage(-exitstatus => 0, -verbose => 2) if $opt_man;

my $mizfiles = undef;
my $global_prel = undef;
my $prel = undef;
my $miztmp_dir = undef;
my $article = undef;
my $article_xml_path = undef;

sub ensure_sensible_environment {
    $mizfiles = $ENV{'MIZFILES'};
    if (! defined $mizfiles) {
        confess 'Error: MIZFILES is not set.';
    }
    $global_prel = catdir ($mizfiles, 'prel');
    if (! -d $global_prel) {
        confess 'No prel directory at ', $global_prel;
    }
    $miztmp_dir = catdir ($mizfiles, 'miztmp');
    if (! -d $miztmp_dir) {
        confess 'miztmp dir missing under ', $mizfiles;
    }
    return 1;
}

sub ensure_sensible_commandline {
    if (scalar @ARGV != 1) {
        pod2usage (1);
    }
    $article = $ARGV[0];
    $article_xml_path = catfile ($miztmp_dir, "${article}.xml1");
    if (! -e $article_xml_path) {
        confess '', $article_xml_path, ' does not exist.';
    }
    my $first = first_letter ($article);
    $prel = catdir ($global_prel, $first);
    if (! -d $prel) {
        confess 'First-letter prel subdirectory missing: ', $prel;
    }
    return;
}

sub prel_for_article {
    my $article = shift;
    my $first = first_letter ($article);
    my $prel_dir = catdir ($prel, $first);
    if (! -d $prel_dir) {
        confess 'First-letter prel subdirectory missing: ', $prel_dir;
    }
    return $prel_dir;
}

sub tptp_formula {
    my $name = shift;
    my $role = shift;
    my $content = shift;
    if (! defined $name) {
        confess 'TPTP formula name missing.';
    }
    if (! defined $role) {
        confess 'TPTP formula role missing.';
    }
    if (! defined $content) {
        confess 'TPTP formula content missing.';
    }
    return "fof(${name},${role},${content})."
}

sub formula_name {
    my $formula = shift;
    if ($formula =~ /\A fof [(] ([a-z_0-9]+) [,] /) {
        return $1;
    } else {
        confess 'Cannot make sense of TPTP formula \'', $formula, '\'.';
    }
}

sub formula_content {
    my $formula = shift;
    if ($formula =~ /\A fof [(] [a-z_0-9]+ [,] [a-z]+ [,] (.+) [)] [.] /) {
        return $1;
    } else {
        confess 'Cannot make sense of TPTP formula \'', $formula, '\'.';
    }
}

sub article_of_item {
    my $item = shift;
    if ($item =~ /\A ([a-z0-9_]+) [:] /) {
        return $1;
    } elsif ($item =~ / [_] ([a-z0-9_]+) \z /) {
        return $1;
    } else {
        confess 'Cannot extract article for \'', $item, '\'.';
    }
}

sub nr_of_item {
    my $item = shift;
    if ($item =~ / [:] (\d+) \z/) {
        return $1;
    } elsif ($item =~ /\A . (\d+) [_] [a-z0-9_]+ \z/) {
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

sub constructor_kind {
    my $constructor_item = shift;
    if ($constructor_item =~ / [:] (.) constructor [:] /) {
        return $1;
    } elsif ($constructor_item =~ /\A (.) \d+ [_] [a-z0-9_]+ \z/) {
        return $1;
    } else {
        confess 'Cannot extract constructor kind from \'', $constructor_item, '\'.';
    }
}

sub render_guard {
    my $variable = shift;
    my $type = shift;
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    my $type_aid = get_aid_attribute ($type);
    my $type_nr = get_absnr_attribute ($type);
    my $type_kind = get_kind_attribute ($type);
    # structure case
    if ($type_kind eq 'G') {
        $type_kind = 'L';
    }
    my $type_kind_lc = lc $type_kind;
    my $type_aid_lc = lc $type_aid;
    my $type_rendered = "${type_kind_lc}${type_nr}_${type_aid_lc}";
    (my $upper_cluster) = $type->findnodes ('Cluster[1]');
    (my $lower_cluster) = $type->findnodes ('Cluster[2]');
    my @adjectives = $upper_cluster->findnodes ('Adjective');
    my @type_parameters = $lower_cluster->findnodes ('following-sibling::*');
    my $num_parameters = scalar @type_parameters;
    my $guard = '(';
    # render the type itself
    $guard .= "${type_rendered}(";
    if ($num_parameters > 0) {
        foreach my $param (@type_parameters) {
            my $param_rendered = render_semantic_content ($param, \%parameters);
            $guard .= "${param_rendered},";
        }
    }
    $guard .= "${variable})";
    # now render any adjectives, if present
    my $num_adjectives = scalar @adjectives;
    my %rendered = ();
    foreach my $i (1 .. $num_adjectives) {
        my $adjective = $adjectives[$i - 1];
        my $adj_aid = get_aid_attribute ($adjective);
        my $adj_nr = get_absnr_attribute ($adjective);
        my $adj_aid_lc = lc $adj_aid;
        my $adj_guard = "v${adj_nr}_${adj_aid_lc}";
        my $adj_is_negated = ($adjective->hasAttribute ('value')) && ($adjective->getAttribute ('value') eq 'false');
        if ($adj_is_negated) {
            $adj_guard = '~' . $adj_guard;
        }
        $adj_guard .= '(';
        my @adj_children = $adjective->findnodes ('*');
        foreach my $adj_child (@adj_children) {
            my $rendered_adj_child = render_semantic_content ($adj_child, \%parameters);
            $adj_guard .= "${rendered_adj_child},";
        }
        $adj_guard .= "${variable}";
        $adj_guard .= ')';
        if (! defined $rendered{$adj_guard}) {
            $rendered{$adj_guard} = 0;
            $guard .= " & ${adj_guard}";
        }
    }
    $guard .= ')';
    return $guard;
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
    my @sorted_constants = sort { constant_less_than_constant ($a, $b) } @constants_no_dups;
    return @sorted_constants;
}

sub render_semantic_content {
    my $node = shift;
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    my $name = $node->nodeName ();
    my $aid = $node->getAttribute ('aid');
    my $kind = $node->getAttribute ('kind');
    my $nr = $node->getAttribute ('nr');
    my $kind_lc = defined $kind ? lc $kind : undef;
    my $aid_lc = defined $aid ? lc $aid : undef;
    if ($name eq 'Pred' || $name eq 'Func') {
        my $absnr = get_absnr_attribute ($node);
        my @children = $node->findnodes ('*');
        my $answer = "${kind_lc}${absnr}_${aid_lc}";
        if (scalar @children == 0) {
            return $answer;
        } elsif ($aid_lc eq 'hidden' && $kind_lc eq 'r' && $absnr eq '1') {
            (my $lhs) = $node->findnodes ('*[1]');
            (my $rhs) = $node->findnodes ('*[2]');
            my $rendered_lhs = render_semantic_content ($lhs, \%parameters);
            my $rendered_rhs = render_semantic_content ($rhs, \%parameters);
            return "(${rendered_lhs} = ${rendered_rhs})";
        } else {
            my $num_children = scalar @children;
            $answer .= '(';
            foreach my $i (1 .. $num_children) {
                my $child = $children[$i - 1];
                my $rendered_child = render_semantic_content ($child, \%parameters);
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
        if (defined $parameters{'const-prefix'}) {
            my $prefix = $parameters{'const-prefix'};
            return "${prefix}${nr}";
        } else {
            return "X${nr}";
        }
    } elsif ($name eq 'Verum') {
        return '$true';
    } elsif ($name eq 'PrivPred') {
        (my $val) = $node->findnodes ('*[position() = last()]');
        return render_semantic_content ($val, \%parameters);
    } elsif ($name eq 'PrivFunc') {
        (my $val) = $node->findnodes ('*[1]');
        return render_semantic_content ($val, \%parameters);
    } elsif ($name eq 'Fraenkel') {
        my $fraenkel_position = $node->findvalue ('count (preceding::Fraenkel) + 1');
        (my $root) = $node->findnodes ('ancestor::Article');
        if (! defined $root) {
            confess 'Article root not found.';
        }
        my $aid = get_aid_attribute ($root);
        my $aid_lc = lc $aid;
        my $tptp_name = "fraenkel_${fraenkel_position}_${aid_lc}";
        return $tptp_name; # later we render the content of the Fraenkel
    } elsif ($name eq 'Is') {
        my @children = $node->findnodes ('*');
        if (scalar @children != 2) {
            confess 'Is node does not have exactly 2 children.';
        }
        my $term = $children[0];
        my $type = $children[1];
        my $term_rendered = render_semantic_content ($term, \%parameters);
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
        my $rendered_matrix = render_semantic_content ($matrix, \%parameters);
        my $guard = render_guard ($var_name, $typ);
        return "(! [${var_name}] : (${guard} => ${rendered_matrix}))";
    } elsif ($name eq 'Typ') {
        confess 'We do not render bare Typ nodes!';
    } elsif ($name eq 'Var') {
        my $nr = get_nr_attribute ($node);
        if (defined $parameters{'var'}) {
            my $var_table_ref = $parameters{'var'};
            my %var_table = %{$var_table_ref};
            if (defined $var_table{$nr}) {
                return $var_table{$nr};
            } else {
                my %trimmed_parameters = %parameters;
                delete $trimmed_parameters{'var'};
                return render_semantic_content ($node, \%trimmed_parameters);
            }
        } else {
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
            return $var_name;
        }
    } elsif ($name eq 'And') {
        my @children = $node->findnodes ('*');
        if (scalar @children == 1) {
            my $child = $children[0];
            return render_semantic_content ($child, \%parameters);
        } elsif (scalar @children == 2) {
            my $lhs = $children[0];
            my $rhs = $children[1];
            my $lhs_rendered = render_semantic_content ($lhs, \%parameters);
            my $rhs_rendered = render_semantic_content ($rhs, \%parameters);
            return "(${lhs_rendered} & ${rhs_rendered})";
        } else {
            my $answer = '(';
            my $num_children = scalar @children;
            foreach my $i (1 .. $num_children) {
                my $child = $children[$i - 1];
                my $child_rendered = render_semantic_content ($child, \%parameters);
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
        my $rendered_arg = render_semantic_content ($arg, \%parameters);
        return "(~ ${rendered_arg})";
    } elsif ($name eq 'Const') {
        my $vid = get_attribute ($node, 'vid');
        return "X${vid}";
    } elsif ($name eq 'Num') {
        # uh oh
        return get_nr_attribute ($node);
    } elsif ($name eq 'FlexFrm') {
        # semantic content of flexary formulas is stored in the final child
        (my $content) = $node->findnodes ('*[position() = last()]');
        return render_semantic_content ($content, \%parameters);
    } else {
        confess 'Unable to generate a rendering for nodes of kind \'', $name, '\'.';
    }
}

sub render_adjective_guards {
    my $term = shift;
    my @adjectives = @_;
    my $num_adjectives = scalar @adjectives;
    if ($num_adjectives == 0) {
        return '$true';
    } else {
        my %rendered = ();
        my $guard = '(';
        foreach my $i (1 .. $num_adjectives) {
            my $adjective = $adjectives[$i - 1];
            my $adj_aid = get_aid_attribute ($adjective);
            my $adj_nr = get_absnr_attribute ($adjective);
            my $adj_aid_lc = lc $adj_aid;
            my $adj_guard = "v${adj_nr}_${adj_aid_lc}";
            my $adj_is_negated = ($adjective->hasAttribute ('value')) && ($adjective->getAttribute ('value') eq 'false');
            if ($adj_is_negated) {
                $adj_guard = '~' . $adj_guard;
            }
            $adj_guard .= '(';
            my @adj_children = $adjective->findnodes ('*');
            foreach my $adj_child (@adj_children) {
                my $rendered_adj_child = render_semantic_content ($adj_child);
                $adj_guard .= "${rendered_adj_child},";
            }
            $adj_guard .= "${term}";
            $adj_guard .= ')';
            if (! defined $rendered{$adj_guard}) {
                $rendered{$adj_guard} = 0;
                if ($i > 1) {
                    $guard .= ' & ';
                }
                $guard .= "${adj_guard}";
            }
        }
        $guard .= ')';
        return $guard;
    }
}

sub generalize_formula_from_arg_types {
    my $formula = shift;
    my @arg_types = @_;
    my $num_arg_types = scalar @arg_types;
    foreach my $i (1 .. $num_arg_types) {
        my $var_index = $num_arg_types - $i + 1;
        my $typ = $arg_types[$var_index - 1];
        my $var = "X${var_index}";
        my $guard = render_guard ($var, $typ);
        $formula = "(! [${var}] : (${guard} => ${formula}))";
    }
    return $formula;
}

sub render_commutativity {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 2) {
        confess 'How to deal with commutativity for a constructor that takes fewer than 2 arguments?';
    }
    my $var_prefix = 'X';
    my $last_var_1 = "${var_prefix}" . ($num_arg_types - 1);
    my $last_var_2 = "${var_prefix}" . ($num_arg_types);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    my $rhs = "${constructor_tptp}";
    $lhs .= '(';
    $rhs .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $lhs .= "${var},";
        $rhs .= "${var},";
    }
    $lhs .= "${last_var_1},${last_var_2}";
    $rhs .= "${last_var_2},${last_var_1}";
    $lhs .= ')';
    $rhs .= ')';
    my $equation = "(${lhs} = ${rhs})";
    return generalize_formula_from_arg_types ($equation, @arg_types);
}

sub render_tptp_constructor {
    my $constructor = shift;
    my $kind = get_kind_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $nr = get_nr_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind_lc = lc $kind;
    return "${kind_lc}${nr}_${aid_lc}";
}

sub render_idempotence {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 1) {
        confess 'How to deal with idempotence for a constructor that takes no arguments?';
    }
    my $var_prefix = 'X';
    my $last_var = "${var_prefix}" . ($num_arg_types - 1);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    $lhs .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $lhs .= "${var},";
    }
    $lhs .= "${last_var},";
    $lhs .= "${last_var}";
    $lhs .= ')';
    my $equation = "(${lhs} = ${last_var})";
    return generalize_formula_from_arg_types ($equation, @arg_types);
}

sub render_symmetry {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 2) {
        confess 'How to deal with symmetry for a constructor that takes fewer than 2 arguments?';
    }
    my $var_prefix = 'X';
    my $last_var_1 = "${var_prefix}" . ($num_arg_types - 1);
    my $last_var_2 = "${var_prefix}" . ($num_arg_types);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    my $rhs = "${constructor_tptp}";
    $lhs .= '(';
    $rhs .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $lhs .= "${var},";
        $rhs .= "${var},";
    }
    $lhs .= "${last_var_1},${last_var_2}";
    $rhs .= "${last_var_2},${last_var_1}";
    $lhs .= ')';
    $rhs .= ')';
    my $equivalence = "(${lhs} => ${rhs})";
    return generalize_formula_from_arg_types ($equivalence, @arg_types);
}

sub render_irreflexivity {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 2) {
        confess 'How to deal with irreflexivity for a constructor that takes fewer than 2 arguments?';
    }
    my $last_var = 'X' . ($num_arg_types - 1);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $reflexivity_content = "~ ${constructor_tptp}";
    $reflexivity_content .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $reflexivity_content .= "${var},";
    }
    $reflexivity_content .= "${last_var},${last_var}";
    $reflexivity_content .= ')';
    return generalize_formula_from_arg_types ($reflexivity_content, @arg_types);
}

sub render_asymmetry {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 2) {
        confess 'How to deal with asymmetry for a constructor that takes fewer than 2 arguments?';
    }
    my $var_prefix = 'X';
    my $last_var_1 = "${var_prefix}" . ($num_arg_types - 1);
    my $last_var_2 = "${var_prefix}" . ($num_arg_types);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    my $rhs = "${constructor_tptp}";
    $lhs .= '(';
    $rhs .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $lhs .= "${var},";
        $rhs .= "${var},";
    }
    $lhs .= "${last_var_1},${last_var_2}";
    $rhs .= "${last_var_2},${last_var_1}";
    $lhs .= ')';
    $rhs .= ')';
    my $equivalence = "(${lhs} => (~ ${rhs}))";
    return generalize_formula_from_arg_types ($equivalence, @arg_types);
}

sub render_connectedness {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 2) {
        confess 'How to deal with connectedness for a constructor that takes fewer than 2 arguments?';
    }
    my $var_prefix = 'X';
    my $last_var_1 = "${var_prefix}" . ($num_arg_types - 1);
    my $last_var_2 = "${var_prefix}" . ($num_arg_types);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    my $rhs = "${constructor_tptp}";
    $lhs .= '(';
    $rhs .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $lhs .= "${var},";
        $rhs .= "${var},";
    }
    $lhs .= "${last_var_1},${last_var_2}";
    $rhs .= "${last_var_2},${last_var_1}";
    $lhs .= ')';
    $rhs .= ')';
    my $disjunction = "(${lhs} | ${rhs})";
    return generalize_formula_from_arg_types ($disjunction, @arg_types);
}

sub render_reflexivity {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 2) {
        confess 'How to deal with reflexivity for a constructor that takes fewer than 2 arguments?';
    }
    my $last_var = 'X' . ($num_arg_types - 1);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $reflexivity_content = "${constructor_tptp}";
    $reflexivity_content .= '(';
    foreach my $i (1 .. $num_arg_types - 2) {
        my $var = "X${i}";
        $reflexivity_content .= "${var},";
    }
    $reflexivity_content .= "${last_var},${last_var}";
    $reflexivity_content .= ')';
    return generalize_formula_from_arg_types ($reflexivity_content, @arg_types);
}

sub render_abstractness {
    my $vconstructor = shift;
    my $vconstructor_kind = get_kind_attribute ($vconstructor);
    if ($vconstructor_kind ne 'V') {
        confess 'We assume that abstractness is formulated only for attribute constructors.';
    }
    my $vconstructor_aid = get_aid_attribute ($vconstructor);
    my $vconstructor_nr = get_nr_attribute ($vconstructor);
    my $vconstructor_aid_lc = lc $vconstructor_aid;
    (my $lconstructor) = $vconstructor->findnodes ('following-sibling::Constructor[@kind = "L"]');
    if (! defined $lconstructor) {
        confess 'Where is the L constructor following a V constructor?';
    }
    my $l_nr = get_nr_attribute ($lconstructor);
    my $l_aid = get_aid_attribute ($lconstructor);
    my $l_aid_lc = lc $l_aid;
    (my $gconstructor) = $vconstructor->findnodes ('following-sibling::Constructor[@kind = "G"]');
    if (! defined $gconstructor) {
        confess 'Where is the G constructor following a V constructor?';
    }
    (my $fields) = $lconstructor->findnodes ('Fields');
    if (! defined $fields) {
        confess 'Fields node not found under an L constructor node.';
    }
    my @fields = $fields->findnodes ('Field');
    my $var = 'X';
    my $l_guard = "l${l_nr}_${l_aid_lc}(${var})";
    my $v_guard = "v${vconstructor_nr}_${vconstructor_aid_lc}(${var})";
    my $g_nr = get_nr_attribute ($gconstructor);
    my $g_aid = get_aid_attribute ($gconstructor);
    my $g_aid_lc = lc $g_aid;
    my $num_fields = scalar @fields;
    my $g_tptp = "g${g_nr}_${g_aid_lc}";
    if ($num_fields > 0) {
        $g_tptp .= '(';
        foreach my $i (1 .. $num_fields) {
            my $field = $fields[$i - 1];
            my $field_nr = get_nr_attribute ($field);
            my $field_aid = get_aid_attribute ($field);
            my $field_aid_lc = lc $field_aid;
            my $field_tptp = "u${field_nr}_${field_aid_lc}(${var})";
            $g_tptp .= $field_tptp;
            if ($i < $num_fields) {
                $g_tptp .= ',';
            }
        }
        $g_tptp .= ')';
    }
    my $equation = "${var} = ${g_tptp}";
    my $content = "(! [${var}] : (${l_guard} => (${v_guard} => ${equation})))";
    return $content;
}

sub render_sethood {
    my $constructor = shift;
    my $mode_nr = get_nr_attribute ($constructor);
    my $mode_aid = get_aid_attribute ($constructor);
    my $mode_aid_lc = lc $mode_aid;
    my $tptp_constructor = "m${mode_nr}_${mode_aid_lc}";
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node not found under an M constructor node.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    my $var_prefix = 'X';
    my $var = "${var_prefix}";
    my $other_var = 'Y';
    my $generic_mconstructor = "${tptp_constructor}";
    $generic_mconstructor .= '(';
    if ($num_arg_types > 0) {
        foreach my $i (1 .. $num_arg_types) {
            my $var = "${var_prefix}${i}";
            $generic_mconstructor .= "${var},";
        }
    }
    $generic_mconstructor .= "${var}";
    $generic_mconstructor .= ')';
    my $content = "(? [${other_var}] : (m2_hidden(${other_var}) & (! [${var}] : (r2_hidden(${var},${other_var}) <=> ${generic_mconstructor}))))";
    if ($num_arg_types > 0) {
        foreach my $i (1 .. $num_arg_types) {
            my $var_index = $num_arg_types - $i + 1;
            my $typ = $arg_types[$var_index - 1];
            my $var = "X${var_index}";
            my $guard = render_guard ($var, $typ);
            $content = "(! [${var}] : (${guard} => ${content}))";
        }
    }
    return $content;
}

sub render_projectivity {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 1) {
        confess 'How to deal with projectivity for a constructor that takes no arguments?';
    }
    my $var_prefix = 'X';
    my $final_var = 'X';
    my $value_var = 'Y';
    my $last_var = "${var_prefix}" . ($num_arg_types);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    $lhs .= '(';
    foreach my $i (1 .. $num_arg_types - 1) {
        my $var = "X${i}";
        $lhs .= "${var},";
    }
    $lhs .= "${last_var}";
    $lhs .= ')';
    my $left_equation = "(${lhs} = ${value_var})";
    my $rhs = "${constructor_tptp}";
    $rhs .= '(';
    foreach my $i (1 .. $num_arg_types - 1) {
        my $var = "X${i}";
        $rhs .= "${var},";
    }
    $rhs .= "${value_var}";
    $rhs .= ')';
    my $right_equation = "${rhs} = ${lhs}";
    my $implication = "(${left_equation} => ${right_equation})";
    (my $value_typ) = $constructor->findnodes ('Typ');
    if (! defined $value_typ) {
        confess 'Value Typ not found under a constructor node.';
    }
    my $value_guard = render_guard ($value_var, $value_typ);
    $implication = "(! [${value_var}] : (${value_guard} => ${implication}))";
    return generalize_formula_from_arg_types ($implication, @arg_types);
}

sub render_involutiveness {
    my $constructor = shift;
    my $nr = get_nr_attribute ($constructor);
    my $aid = get_aid_attribute ($constructor);
    my $aid_lc = lc $aid;
    my $kind = get_kind_attribute ($constructor);
    my $kind_lc = lc $kind;
    (my $arg_types_node) = $constructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    if ($num_arg_types < 1) {
        confess 'How to deal with involutiveness for a constructor that takes no arguments?';
    }
    my $var_prefix = 'X';
    my $final_var = 'X';
    my $value_var = 'Y';
    my $last_var = "${var_prefix}" . ($num_arg_types);
    my $constructor_tptp = "${kind_lc}${nr}_${aid_lc}";
    my $lhs = "${constructor_tptp}";
    $lhs .= '(';
    foreach my $i (1 .. $num_arg_types - 1) {
        my $var = "X${i}";
        $lhs .= "${var},";
    }
    $lhs .= "${last_var}";
    $lhs .= ')';
    my $left_equation = "(${lhs} = ${value_var})";
    my $rhs = "${constructor_tptp}";
    $rhs .= '(';
    foreach my $i (1 .. $num_arg_types - 1) {
        my $var = "X${i}";
        $rhs .= "${var},";
    }
    $rhs .= "${value_var}";
    $rhs .= ')';
    my $right_equation = "${rhs} = ${last_var}";
    my $implication = "(${left_equation} => ${right_equation})";
    (my $value_typ) = $constructor->findnodes ('Typ');
    if (! defined $value_typ) {
        confess 'Value Typ not found under a constructor node.';
    }
    my $value_guard = render_guard ($value_var, $value_typ);
    $implication = "(! [${value_var}] : (${value_guard} => ${implication}))";
    return generalize_formula_from_arg_types ($implication, @arg_types);
}

sub render_deftheorem {
    my $deftheorem_node = shift;
    (my $proposition_node) = $deftheorem_node->findnodes ('Proposition[1]');
    if (! defined $proposition_node) {
        confess 'Proposition node not found under a definitional theorem node';
    }
    return render_proposition ($proposition_node);
}

sub render_item {
    my $item = shift;
    my $name = $item->nodeName;
    if ($name eq 'JustifiedTheorem') {
        return render_justified_theorem ($item);
    } elsif ($name eq 'Proposition') {
        return render_proposition ($item);
    } elsif ($name eq 'CCluster') {
        (my $coherence_node) = $item->findnodes ('following-sibling::Coherence');
        if (defined $coherence_node) {
            (my $proposition) = $coherence_node->findnodes ('Proposition');
            if (! defined $proposition) {
                confess 'Proposition node not found under a Coherence node.';
            }
            return render_proposition ($proposition);
        } else {
            (my $correctness_node) = $item->findnodes ('following-sibling::*[1][self::Correctness]');
            if (! defined $correctness_node) {
                confess 'Neither a Coherence nor a Correctness node was found following a CCluster node.';
            }
            (my $coherence_node) = $correctness_node->findnodes ('Coherence');
            if (! defined $coherence_node) {
                confess 'Coherence node not found under a Correctness node following a CCluster node.';
            }
            (my $proposition) = $coherence_node->findnodes ('following-sibling::*[1][self::Proposition]');
            if (! defined $proposition) {
                confess 'Proposition node not found where expected:', $LF, $LF, $coherence_node->toString;
            }
            return render_proposition ($proposition);
        }
    } elsif ($name eq 'RCluster') {
        (my $existence_node) = $item->findnodes ('following-sibling::*[1][self::Existence]');
        if (defined $existence_node) {
            (my $proposition_node) = $existence_node->findnodes ('Proposition[1]');
            if (! defined $proposition_node) {
                confess 'No Proposition node found under the Existence node.';
            }
            return render_proposition ($proposition_node);
        } else {
            (my $correctness_node) = $item->findnodes ('following-sibling::*[1][self::Correctness]');
            if (! defined $correctness_node) {
                confess 'Neither an Existence nor a Correctness node was found immediately following an RCluster.';
            }
            (my $existence_node) = $correctness_node->findnodes ('Existence');
            if (! defined $existence_node) {
                confess 'Existence node not found under a Correctness node immediately following an RCluster node.';
            }
            (my $proposition) = $existence_node->findnodes ('following-sibling::*[1][self::Proposition]');
            if (! defined $proposition) {
                confess 'Proposition node not found where expected:', $LF, $LF, $existence_node->toString;
            }
            return render_proposition ($proposition);
        }
    } elsif ($name eq 'FCluster') {
        (my $coherence_node) = $item->findnodes ('following-sibling::Coherence');
        if (defined $coherence_node) {
            (my $proposition) = $coherence_node->findnodes ('Proposition');
            if (! defined $proposition) {
                confess 'Proposition node not found under a Coherence node.';
            }
            return render_proposition ($proposition);
        } else {
            (my $correctness_node) = $item->findnodes ('following-sibling::*[1][self::Correctness]');
            if (! defined $correctness_node) {
                confess 'Neither a Coherence nor a Correctness node was found following an FCluster node.';
            }
            (my $coherence_node) = $correctness_node->findnodes ('Coherence');
            if (! defined $coherence_node) {
                confess 'Coherence node not found under a Correctness node following an FCluster node.';
            }
            (my $proposition) = $coherence_node->findnodes ('following-sibling::*[1][self::Proposition]');
            if (! defined $proposition) {
                confess 'Proposition node not found where expected.';
            }
            return render_proposition ($proposition);
        }
    } elsif ($name eq 'Identify') {
        (my $compatibility_node) = $item->findnodes ('following-sibling::Compatibility');
        if (defined $compatibility_node) {
            (my $proposition) = $compatibility_node->findnodes ('Proposition');
            if (! defined $proposition) {
                confess 'Proposition node not found under a Compatibility node.';
            }
            return render_proposition ($proposition);
        } else {
            (my $correctness_node) = $item->findnodes ('following-sibling::Correctness');
            if (! defined $correctness_node) {
                confess 'Neither a Compatibility nor a Correctness node was found immediately following an Identify node.';
            }
            (my $compatibility_node) = $correctness_node->findnodes ('Compatibility');
            if (! defined $compatibility_node) {
                confess 'Compatibility node not found under a Correctness node immediately following an Identify node.';
            }
            return render_proposition ($compatibility_node);
        }

    } elsif ($name eq 'Reduction') {
        (my $reducibility_node) = $item->findnodes ('following-sibling::Reducibility');
        if (! defined $reducibility_node) {
            confess 'Reducibility node not found following an Identify.';
        }
        (my $proposition) = $reducibility_node->findnodes ('Proposition');
        if (! defined $proposition) {
            confess 'Proposition node not found under a Reducibility node.';
        }
        return render_proposition ($proposition);
    } elsif ($name eq 'Commutativity') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_commutativity ($constructor);
    } elsif ($name eq 'Idempotence') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_idempotence ($constructor);
    } elsif ($name eq 'Involutiveness') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_involutiveness ($constructor);
    } elsif ($name eq 'Projectivity') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_projectivity ($constructor);
    } elsif ($name eq 'Symmetry') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_symmetry ($constructor);
    } elsif ($name eq 'Antisymmetry') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_asymmetry ($constructor);
    } elsif ($name eq 'Connectedness') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_connectedness ($constructor);
    } elsif ($name eq 'Reflexivity') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_reflexivity ($constructor);
    } elsif ($name eq 'Irreflexivity') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_irreflexivity ($constructor);
    } elsif ($name eq 'Abstractness') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_abstractness ($constructor);
    } elsif ($name eq 'Sethood') {
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        return render_sethood ($constructor);
    } elsif ($name eq 'DefTheorem') {
        return render_deftheorem ($item);
    } elsif ($name eq 'Coherence') {
        if ($item->exists ('parent::Correctness')) {
            return render_proposition ($item);
        } else {
            (my $proposition) = $item->findnodes ('Proposition');
            if (! defined $proposition) {
                confess 'Proposition node not found under a Coherence node.';
            }
            return render_proposition ($proposition);
        }
    } elsif ($name eq 'Existence') {
        (my $proposition) = $item->findnodes ('Proposition');
        if (! defined $proposition) {
            confess 'Proposition node not found under an Existence node.';
        }
        return render_proposition ($proposition);
    } elsif ($name eq 'Uniqueness') {
        (my $proposition) = $item->findnodes ('Proposition');
        if (! defined $proposition) {
            confess 'Proposition node not found under a Uniqueness node.';
        }
        return render_proposition ($proposition);
    } elsif ($name eq 'Compatibility') {
        (my $proposition) = $item->findnodes ('Proposition');
        if (! defined $proposition) {
            confess 'Proposition node not found under a Compatibility node.';
        }
        return render_proposition ($proposition);
    } else {
        confess 'How to render items of kind \'', $name, '\'?';
    }
}

sub type_for_constant {
    my $constant = shift;
    my $vid = get_vid_attribute ($constant);
    my $nr = get_nr_attribute ($constant);
    my $node = $constant;
    my $preceding_sibling_binder_xpath = "preceding-sibling::*[(self::Typ[\@vid = \"${vid}\"]) or ((self::Let or self::Reconsider or self::Consider or self::Given or self::TakeAsVar) and Typ[\@vid = \"${vid}\"])]";
    my $binder = undef;
    until ((! defined $node) || (defined $binder)) {
        if ($node->exists ($preceding_sibling_binder_xpath)) {
            ($binder) = $node->findnodes ($preceding_sibling_binder_xpath);
        } else {
            $node = $node->parentNode;
        }
    }
    if (! defined $binder) {
        confess 'We failed to find a binder for ', $constant->toString;
    }
    return type_from_binder ($binder);
}

sub type_from_binder {
    my $binder = shift;
    my $binder_name = $binder->nodeName ();
    if ($binder_name eq 'For') {
        (my $typ) = $binder->findnodes ('Typ');
        if (! defined $typ) {
            confess 'For node lacks a Typ child.';
        }
        return $typ;
    } elsif ($binder_name eq 'Typ') {
        return $binder;
    } elsif ($binder_name eq 'Let') {
        (my $typ) = $binder->findnodes ('Typ');
        if (! defined $typ) {
            confess 'Let node lacks a Typ child.';
        }
        return $typ;
    } elsif ($binder_name eq 'Reconsider') {
        (my $typ) = $binder->findnodes ('Typ');
        if (! defined $typ) {
            confess 'Reconsider node lacks a Typ child.';
        }
        return $typ;
    } elsif ($binder_name eq 'Consider') {
        (my $typ) = $binder->findnodes ('Typ');
        if (! defined $typ) {
            confess 'Reconsider node lacks a Typ child.';
        }
        return $typ;
    } elsif ($binder_name eq 'Given') {
        (my $typ) = $binder->findnodes ('Typ');
        if (! defined $typ) {
            confess 'Given node lacks a Typ child.';
        }
        return $typ;
    } elsif ($binder_name eq 'TakeAsVar') {
        (my $typ) = $binder->findnodes ('Typ');
        if (! defined $typ) {
            confess 'TakeAsVar node lacks a Typ child.';
        }
        return $typ;
    } else {
        confess 'What kind of binder is', $LF, $binder->toString (), $LF, '?';
    }
}

sub render_justified_theorem {
    my $justified_theorem_node = shift;
    (my $proposition_node) = $justified_theorem_node->findnodes ('Proposition[1]');
    if (! defined $proposition_node) {
        confess 'Proposition node not found under a justified theorem node';
    }
    return render_proposition ($proposition_node);
}

sub render_proposition {
    my $proposition_node = shift;
    my $name = $proposition_node->nodeName;
    (my $content_node) = $proposition_node->findnodes ('*[position() = last()]');
    my $content = render_semantic_content ($content_node);
    my @constants = constants_under_node ($content_node);
    return generalize_formula_from_constants ($content, @constants);
}

sub tptp_name_for_existence {
    my $kconstructor = shift;
    my $k_nr = get_nr_attribute ($kconstructor);
    my $k_aid = get_aid_attribute ($kconstructor);
    my $k_aid_lc = lc $k_aid;
    return "existence_k${k_nr}_${k_aid_lc}";
}

sub tptp_name_for_uniqueness {
    my $kconstructor = shift;
    my $k_nr = get_nr_attribute ($kconstructor);
    my $k_aid = get_aid_attribute ($kconstructor);
    my $k_aid_lc = lc $k_aid;
    return "uniqueness_k${k_nr}_${k_aid_lc}";
}

sub tptp_name_for_free {
    my $gconstructor = shift;
    my $g_nr = get_nr_attribute ($gconstructor);
    my $g_aid = get_aid_attribute ($gconstructor);
    my $g_aid_lc = lc $g_aid;
    return "free_g${g_nr}_${g_aid_lc}";
}

sub free_for_constructor {
    my $gconstructor = shift;
    my $gconstructor_kind = get_kind_attribute ($gconstructor);
    if ($gconstructor_kind ne 'G') {
        confess 'We assume that freeness is formulated only for G constructors.';
    }
    my $g_nr = get_nr_attribute ($gconstructor);
    my $g_aid = get_aid_attribute ($gconstructor);
    my $g_aid_lc = lc $g_aid;
    (my $arg_types_node) = $gconstructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a G cosntructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    my $var_prefix_1 = 'A';
    my $var_prefix_2 = 'B';
    my $lhs_equation_lhs = "g${g_nr}_${g_aid_lc}";
    my $lhs_equation_rhs = "g${g_nr}_${g_aid_lc}";
    if ($num_arg_types > 0) {
        my @var_1_list = map { "${var_prefix_1}${_}" } (1 .. $num_arg_types);
        my @var_2_list = map { "${var_prefix_2}${_}" } (1 .. $num_arg_types);
        my $var_1_list = join ',', @var_1_list;
        my $var_2_list = join ',', @var_2_list;
        $lhs_equation_lhs = "${lhs_equation_lhs}(${var_1_list})";
        $lhs_equation_rhs = "${lhs_equation_rhs}(${var_2_list})";
    }
    my $lhs_equation = "${lhs_equation_lhs} = ${lhs_equation_rhs}";
    my $rhs = undef;
    if ($num_arg_types == 0) {
        $rhs = '$true';
    } else {
        $rhs = '(';
        foreach my $i (1 .. $num_arg_types) {
            my $equation = "${var_prefix_1}${i} = ${var_prefix_2}${i}";
            $rhs .= $equation;
            if ($i < $num_arg_types) {
                $rhs .= ' & ';
            }
        }
        $rhs .= ')';
    }
    my $content = "(${lhs_equation} => ${rhs})";
    # now generalize
    foreach my $i (1 .. $num_arg_types) {
        my $var_index = $num_arg_types - $i + 1;
        my $typ = $arg_types[$var_index - 1];
        my $var_1 = "${var_prefix_1}${var_index}";
        my $var_2 = "${var_prefix_2}${var_index}";
        my $guard_1 = render_guard ($var_1, $typ, { 'const-prefix' => $var_prefix_1 });
        my $guard_2 = render_guard ($var_2, $typ, { 'const-prefix' => $var_prefix_2 });
        $content = "(! [${var_1},${var_2}] : ((${guard_1} & ${guard_2}) => ${content}))";
    }
    return $content;
}

sub existence_for_constructor {
    my $kconstructor = shift;
    my $relnr = get_relnr_attribute ($kconstructor);
    (my $block) = $kconstructor->findnodes ('ancestor::DefinitionBlock');
    if (! defined $block) {
        confess 'Where is the enclosing DefinitionBlock for a K constructor?', $LF, $kconstructor->toString;
    }
    my $definiens_xpath = "following-sibling::Definiens[\@constrkind = \"K\" and \@constrnr = \"${relnr}\"]";
    (my $definiens) = $block->findnodes ($definiens_xpath);
    if (! defined $definiens) {
        confess 'Where is the Definiens matching the XPath instruction', $LF, $LF, '  ', $definiens_xpath, $LF, $LF, 'following the constructor', $LF, $LF, $kconstructor->toString, $LF, $LF, '?';
    }
    (my $defmeaning) = $definiens->findnodes ('DefMeaning[@kind = "e"]');
    if (! defined $defmeaning) {
        confess 'We expect to find an expandable DefMeaning in the Definiens node', $LF, $LF, $definiens->toString;
    }
    (my $result_typ) = $kconstructor->findnodes ('*[position() = last()]');
    my $existential_var = 'Y';
    my $k_nr = get_nr_attribute ($kconstructor);
    my $k_aid = get_aid_attribute ($kconstructor);
    my $k_aid_lc = lc $k_aid;
    (my $arg_types_node) = $kconstructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a K cosntructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    my $var_prefix = 'X';
    my $formula = undef;
    if ($defmeaning->exists ('PartialDef')) {
        my $rhs = '(';
        my @partial_defs = $defmeaning->findnodes ('PartialDef');
        my $num_partial_defs = scalar @partial_defs;
        foreach my $i (1 .. $num_partial_defs) {
            my $partial_def = $partial_defs[$i - 1];
            (my $val_node) = $partial_def->findnodes ('*[1]');
            (my $condition_node) = $partial_def->findnodes ('*[2]');
            my $rendered_condition = render_semantic_content ($condition_node);
            my $rendered_value = render_semantic_content ($val_node);
            my $equation = "(${existential_var} = ${rendered_value})";
            my $guarded = "(${rendered_condition} => ${equation})";
            $rhs .= $guarded;
            if ($i < $num_partial_defs) {
                $rhs .= ' & ';
            }
        }
        $rhs .= ')';
        $formula = $rhs;
    } else {
        (my $content) = $defmeaning->findnodes ('*[position() = last()]');
        my $rhs = render_semantic_content ($content);
        my $lhs = "${existential_var}";
        my $equation = "(${lhs} = ${rhs})";
        $formula = $equation;
    }
    my $existential_guard = render_guard ($existential_var, $result_typ);
    my $existential = "(? [${existential_var}] : (${existential_guard} & ${formula}))";
    # now generalize
    foreach my $i (1 .. $num_arg_types) {
        my $var_index = $num_arg_types - $i + 1;
        my $typ = $arg_types[$var_index - 1];
        my $var = "${var_prefix}${var_index}";
        my $guard = render_guard ($var, $typ);
        $existential = "(! [${var}] : (${guard} => ${existential}))";
    }
    return $existential;
}

sub uniqueness_for_constructor {
    my $kconstructor = shift;
    my $relnr = get_relnr_attribute ($kconstructor);
    (my $block) = $kconstructor->findnodes ('ancestor::DefinitionBlock');
    if (! defined $block) {
        confess 'Where is the enclosing DefinitionBlock for a K constructor?', $LF, $kconstructor->toString;
    }
    my $definiens_xpath = "following-sibling::Definiens[\@constrkind = \"K\" and \@constrnr = \"${relnr}\"]";
    (my $definiens) = $block->findnodes ($definiens_xpath);
    if (! defined $definiens) {
        confess 'Where is the Definiens matching the XPath instruction', $LF, $LF, '  ', $definiens_xpath, $LF, $LF, 'following the constructor', $LF, $LF, $kconstructor->toString, $LF, $LF, '?';
    }
    (my $defmeaning) = $definiens->findnodes ('DefMeaning[@kind = "e"]');
    if (! defined $defmeaning) {
        confess 'We expect to find an expandable DefMeaning in the Definiens node', $LF, $LF, $definiens->toString;
    }
    (my $result_typ) = $kconstructor->findnodes ('*[position() = last()]');
    my $uniqueness_prefix_1 = 'A';
    my $uniqueness_prefix_2 = 'B';
    my $uniqueness_var_1 = "${uniqueness_prefix_1}1";
    my $uniqueness_var_2 = "${uniqueness_prefix_2}1";
    my $k_nr = get_nr_attribute ($kconstructor);
    my $k_aid = get_aid_attribute ($kconstructor);
    my $k_aid_lc = lc $k_aid;
    (my $arg_types_node) = $kconstructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under a K cosntructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    my $var_prefix = 'X';
    my $equation = "(${uniqueness_var_1} = ${uniqueness_var_2})";
    my $uniquess_guard_1 = render_guard ($uniqueness_var_1, $result_typ);
    my $uniquess_guard_2 = render_guard ($uniqueness_var_2, $result_typ);
    my $implication = undef;
    if ($defmeaning->exists ('PartialDef')) {
        $implication = '$true'; #punting for now; too weird to
                                #formulate uniqueness for partial
                                #definitions
    } else {
        (my $content) = $defmeaning->findnodes ('*[position() = last()]');
        my $term = render_semantic_content ($content);
        $implication = "(((${uniqueness_var_1} = ${term}) & (${uniqueness_var_2} = ${term})) => ${equation})";
    }

    $implication = "(! [${uniqueness_var_1},${uniqueness_var_2}] : ((${uniquess_guard_1} & ${uniquess_guard_2}) => ${implication}))";

    # now generalize
    foreach my $i (1 .. $num_arg_types) {
        my $var_index = $num_arg_types - $i + 1;
        my $typ = $arg_types[$var_index - 1];
        my $var = "${var_prefix}${var_index}";
        my $guard = render_guard ($var, $typ);
        $implication = "(! [${var}] : (${guard} => ${implication}))";
    }
    return $implication;
}

sub ensure_same_toplevel_origin {
    my $node_1 = shift;
    my $node_2 = shift;
    (my $top_1) = $node_1->findnodes ('ancestor::*[parent::Article]');
    (my $top_2) = $node_2->findnodes ('ancestor::*[parent::Article]');
    if (! defined $top_1) {
        confess 'Where is the toplevel node for', $LF, $node_1->toString;
    }
    if (! defined $top_2) {
        confess 'Where is the toplevel node for', $LF, $node_2->toString;
    }
    my $val_1 = $top_1->findvalue ('count (preceding-sibling::*) + 1');
    my $val_2 = $top_2->findvalue ('count (preceding-sibling::*) + 1');
    if ($val_1 == $val_2) {
        return 1;
    } else {
        confess 'Two nodes with distinct toplevel origins:', $LF, $LF, $node_1->toString (), $LF, $LF, 'and', $LF, $LF, $node_2->toString ();
    }
}

sub generalize_formula_from_constants {
    my $formula = shift;
    my @constants = @_;
    @constants = sort { constant_less_than_constant ($a, $b) } @constants;
    my $new_formula = "${formula}";
    my %handled_constants = ();
    until (scalar @constants == 0) {
        my $constant = pop @constants;
        my $vid = get_vid_attribute ($constant);
        my $nr = get_nr_attribute ($constant);
        my $typ = type_for_constant ($constant);
        # warn 'type for constant ', $constant->toString, ' is ', $LF, $typ->toString;
        # ensure_same_toplevel_origin ($typ, $constant);
        my $var_name = "X${vid}";
        my $guard = render_guard ($var_name, $typ);
        $new_formula = "(! [${var_name}] : (${guard} => ${new_formula}))";
        $handled_constants{$nr} = 0;
        # are there any new constants under the type?
        my @constants_under_typ = constants_under_node ($typ);
        foreach my $c (@constants_under_typ) {
            my $vid = get_vid_attribute ($c);
            my $nr = get_nr_attribute ($c);
            if (! defined $handled_constants{$nr}) {
                # we haven't seen it yet
                if (none { get_nr_attribute ($_) eq "${nr}" } @constants) {
                    # we wouldn't normally encounter it
                    # warn 'NEW CONSTANT: ', $c->toString;
                    push (@constants, $c);
                }
            }
        }
        @constants = sort { constant_less_than_constant ($a, $b) } @constants;
    }
    return $new_formula;
}

sub constant_less_than_constant {
    my $const_a = shift;
    my $const_b = shift;
    my $a_nr = get_nr_attribute ($const_a);
    my $b_nr = get_nr_attribute ($const_b);
    if ($a_nr < $b_nr) {
        return -1;
    } elsif ($a_nr > $b_nr) {
        return 1;
    } else {
        return 0;
    }
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
        confess '', $name, ' node lacks a(n) ', $attr, ' attribute:', $LF, $node->toString;
    }
}

sub get_aid_attribute {
    my $node = shift;
    return get_attribute ($node, 'aid');
}

sub get_relnr_attribute {
    my $node = shift;
    return get_attribute ($node, 'relnr');
}

sub get_name_attribute {
    my $node = shift;
    return get_attribute ($node, 'name');
}

sub get_nr_attribute {
    my $node = shift;
    return get_attribute ($node, 'nr');
}

sub get_absnr_attribute {
    my $node = shift;
    return get_attribute ($node, 'absnr');
}

sub get_kind_attribute {
    my $node = shift;
    return get_attribute ($node, 'kind');
}

sub get_vid_attribute {
    my $node = shift;
    return get_attribute ($node, 'vid');
}

sub first_letter {
    my $str = shift;
    if (! defined $str) {
        confess 'Missing first argument.';
    }
    if ($str =~ /\A (.) /) {
        return $1;
    } else {
        confess 'How to extract the first letter from \'', $str, '\'?';
    }
}

sub item_position {
    my $item = shift;
    my $name = $item->nodeName;
    if ($name eq 'JustifiedTheorem') {
        $item->findvalue ('count (preceding-sibling::JustifiedTheorem) + 1');
    } elsif ($name eq 'Proposition') {
        $item->findvalue ('count (preceding-sibling::Proposition) + 1');
    } elsif ($name eq 'CCluster') {
        $item->findvalue ('count (preceding::CCluster) + 1');
    } elsif ($name eq 'RCluster') {
        $item->findvalue ('count (preceding::RCluster) + 1');
    } elsif ($name eq 'FCluster') {
        $item->findvalue ('count (preceding::FCluster) + 1');
    } elsif ($name eq 'Identify') {
        $item->findvalue ('count (preceding::Identify) + 1');
    } elsif ($name eq 'Reduction') {
        $item->findvalue ('count (preceding::Reduction) + 1');
    } elsif ($name eq 'DefTheorem') {
        $item->findvalue ('count (preceding-sibling::DefTheorem) + 1');
    } else {
        confess 'How to find the position of nodes like \'', $name, '\'?';
    }
}

sub tptp_name_for_item {
    my $item = shift;
    my $name = $item->nodeName;
    if ($item->exists ('parent::Properties')) {
        $name = lc $name;
        (my $constructor) = $item->findnodes ('ancestor::Constructor');
        if (! defined $constructor) {
            confess 'No ancestor constructor for property ', $name, '.';
        }
        my $aid = get_aid_attribute ($constructor);
        my $nr = get_nr_attribute ($constructor);
        my $kind = get_kind_attribute ($constructor);
        $aid = lc $aid;
        $kind = lc $kind;
        return "${name}_${kind}${nr}_${aid}";
    } elsif ($item->exists ('following-sibling::*[1][self::From]')) {
        (my $from) = $item->findnodes ('following-sibling::*[1][self::From]');
        my $from_aid = get_aid_attribute ($from);
        my $from_aid_lc = lc $from_aid;
        my $from_nr = get_absnr_attribute ($from);
        my $pos_xpath = "preceding::From[not(ancestor::SchemeBlock) and \@aid = \"${from_aid}\" and \@absnr = \"${from_nr}\"]";
        my $pos = $from->findvalue ("count (${pos_xpath}) + 1");
        return "${from_aid_lc}_s${from_nr}_${article}_${pos}";
    } elsif (($name eq 'Existence') || ($name eq 'Uniqueness') || ($name eq 'Coherence')) {
        $name = lc $name;
        (my $constructor) = $item->findnodes ('following-sibling::Constructor');
        if (! defined $constructor) {
            if ($item->exists ('parent::Correctness')) {
                (my $correctness) = $item->findnodes ('parent::Correctness');
                ($constructor) = $correctness->findnodes ('following-sibling::Constructor');
                if (! defined $constructor) {
                    confess 'How to extract a constructor?';
                }
            }
        }
        my $aid = get_aid_attribute ($constructor);
        my $nr = get_nr_attribute ($constructor);
        my $kind = get_kind_attribute ($constructor);
        $aid = lc $aid;
        $kind = lc $kind;
        return "${name}_${kind}${nr}_${aid}";
    } else {
        my $pos = item_position ($item);
        if ($name eq 'JustifiedTheorem') {
            return "t${pos}_${article}";
        } elsif ($name eq 'Proposition') {
            return "l${pos}_${article}";
        } elsif ($name =~ /\A ([CFR]) Cluster \z/) {
            my $kind = $1;
            $kind = lc $kind;
            return "${kind}c${pos}_${article}";
        } elsif ($name eq 'Identify') {
            return "ie${pos}_${article}";
        } elsif ($name eq 'Reduction') {
            return "red${pos}_${article}";
        } elsif ($name eq 'DefTheorem') {
            return "d${pos}_${article}";
        } else {
            confess 'How to make a TPTP name for items of kind \'', $name, '\'?';
        }
    }
}

sub tptp_name_for_value_type {
    my $constructor_node = shift;
    if ($constructor_node->nodeName eq 'Choice') {
        my $name = render_choice_term ($constructor_node);
        return "dt_${name}";
    } else {
        my $constructor_kind = get_kind_attribute ($constructor_node);
        my $constructor_aid = get_aid_attribute ($constructor_node);
        my $constructor_kind_lc = lc $constructor_kind;
        my $constructor_aid_lc = lc $constructor_aid;
        my $nr = get_nr_attribute ($constructor_node);
        my $constructor_tptp = "${constructor_kind_lc}${nr}_${constructor_aid_lc}";
        return "dt_${constructor_tptp}";
    }
}

sub value_type_for_constructor {
    my $constructor_node = shift;
    (my $arg_types_node) = $constructor_node->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node not found under a Constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    (my $value_typ_node) = $constructor_node->findnodes ('Typ');
    if (! defined $value_typ_node) {
        confess 'Value type not found where we expect under a Constructor node:', $LF, $constructor_node->toString;
    }
    my $nr = get_nr_attribute ($constructor_node);
    my $constructor_kind = get_kind_attribute ($constructor_node);
    my $constructor_aid = get_aid_attribute ($constructor_node);
    my $constructor_kind_lc = lc $constructor_kind;
    my $constructor_aid_lc = lc $constructor_aid;
    my $constructor_tptp = "${constructor_kind_lc}${nr}_${constructor_aid_lc}";
    my $generic_term = $constructor_tptp;
    if ($num_arg_types > 0) {
        $generic_term .= '(';
        foreach my $i (1 .. $num_arg_types) {
            $generic_term .= "X${i}";
            if ($i < $num_arg_types) {
                $generic_term .= ',';
            }
        }
        $generic_term .= ')';
    }
    my $content = render_guard ($generic_term, $value_typ_node);
    return generalize_formula_from_arg_types ($content, @arg_types);
}

sub tptp_name_for_widening {
    my $gconstructor = shift;
    (my $lconstructor) = $gconstructor->findnodes ('preceding-sibling::Constructor[@kind = "L"]');
    if (! defined $lconstructor) {
        confess 'L Constructor node not found following a G constructor.';
    }
    my $l_nr = get_nr_attribute ($lconstructor);
    my $l_aid = get_aid_attribute ($lconstructor);
    my $l_aid_lc = lc $l_aid;
    my $new_constructor = "l${l_nr}_${l_aid_lc}";
    return "widening_${new_constructor}";
}

sub widening_for_structure_constructor {
    my $gconstructor = shift;
    (my $lconstructor) = $gconstructor->findnodes ('preceding-sibling::Constructor[@kind = "L"]');
    if (! defined $lconstructor) {
        confess 'L Constructor node not found following a G constructor.';
    }
    my $l_nr = get_nr_attribute ($lconstructor);
    my $l_aid = get_aid_attribute ($lconstructor);
    my $l_aid_lc = lc $l_aid;
    my $new_constructor = "l${l_nr}_${l_aid_lc}";
    my $tptp_name = "widening_${new_constructor}";
    (my $arg_types_node) = $lconstructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node missing under an L constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    my $var_prefix = 'X';
    my $var = "${var_prefix}";
    my $new = "${new_constructor}";
    $new .= '(';
    foreach my $i (1 .. $num_arg_types) {
        my $var = "${var_prefix}${i}";
        $new .= "${var},";
    }
    $new .= "${var}";
    $new .= ')';
    my $old = undef;
    my @typs = $lconstructor->findnodes ('Typ');
    my $num_typs = scalar @typs;
    if ($num_typs == 0) {
        $old = '$true';
    } else {
        $old = '(';
        foreach my $i (1 .. $num_typs) {
            my $typ = $typs[$i - 1];
            my $guard = render_guard ($var, $typ);
            $old .= $guard;
            if ($i < $num_typs) {
                $old .= ' & ';
            }
        }
        $old .= ')';
    }
    my $content = "(! [${var}] : (${new} => ${old}))";
    return generalize_formula_from_arg_types ($content, @arg_types);
}

sub constructor_node_of_constructor_item {
    my $constructor_item = shift;
    my $article = article_of_item ($constructor_item);
    my $nr = nr_of_item ($constructor_item);
    my $kind = constructor_kind ($constructor_item);
    my $item_xml = "${miztmp_dir}/${article}.xml1";
    if (! -e $item_xml) {
        confess 'Absolutized XML for ', $article, ' missing under ', $miztmp_dir;
    }
    my $item_xml_doc = parse_xml_file ($item_xml);
    my $item_xml_root = $item_xml_doc->documentElement ();
    my $kind_uc = uc $kind;
    my $article_uc = uc $article;
    my $constructor_xpath = "descendant::Constructor[\@kind = \"${kind_uc}\"][${nr}]";
    (my $constructor_node) = $item_xml_root->findnodes ($constructor_xpath);
    if (! defined $constructor_node) {
        confess 'Constructor not found in ', $item_xml, '.';
    }
    return $constructor_node;
}

sub arg_types_of_constructor {
    my $constructor_node = shift;
    (my $arg_types_node) = $constructor_node->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node not found under a Constructor node:', $LF, $constructor_node->toString;
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    return @arg_types;
}

sub projections_for_structure_constructor {
    my $gconstructor = shift;
    my @projections = ();
    my $gconstructor_kind = get_kind_attribute ($gconstructor);
    if ($gconstructor_kind ne 'G') {
        confess 'We assume that projections are formulated only for G constructors.';
    }
    my $g_nr = get_nr_attribute ($gconstructor);
    my $g_aid = get_aid_attribute ($gconstructor);
    my $g_aid_lc = lc $g_aid;
    my $gconstructor_tptp = "g${g_nr}_${g_aid_lc}";
    (my $arg_types_node) = $gconstructor->findnodes ('ArgTypes');
    if (! defined $arg_types_node) {
        confess 'ArgTypes node not found under a G constructor.';
    }
    my @arg_types = $arg_types_node->findnodes ('*');
    my $num_arg_types = scalar @arg_types;
    my $generic_gconstructor = "${gconstructor_tptp}";
    my $var_prefix = 'X';
    if ($num_arg_types > 0) {
        $generic_gconstructor .= '(';
        foreach my $i (1 .. $num_arg_types) {
            my $var = "${var_prefix}${i}";
            $generic_gconstructor .= "${var}";
            if ($i < $num_arg_types) {
                $generic_gconstructor .= ',';
            }
        }
        $generic_gconstructor .= ')';
    }
    (my $fields_node) = $gconstructor->findnodes ('Fields');
    if (! defined $fields_node) {
        confess 'Fields node not found in a G constructor node.';
    }
    my @fields = $fields_node->findnodes ('*');
    my $num_fields = scalar @fields;
    foreach my $i (1 .. $num_fields) {
        my $field  = $fields[$i - 1];
        my $field_aid = get_aid_attribute ($field);
        my $field_nr = get_absnr_attribute ($field);
        my $field_aid_lc = lc $field_aid;
        my $uconstructor_name = "u${field_nr}_${field_aid_lc}";
        my $uconstructor = constructor_node_of_constructor_item ($uconstructor_name);
        my @field_arg_types = arg_types_of_constructor ($uconstructor);
        my $num_field_arg_types = scalar @field_arg_types;
        my $field_tptp = "u${field_nr}_${field_aid_lc}";
        my $var = "${var_prefix}${i}";
        my $rhs = "${var}";
        my $lhs = "${field_tptp}";
        $lhs .= '(';
        foreach my $j (1 .. $num_field_arg_types - 1) {
            my $var = "${var_prefix}${j}";
            $lhs .= "${var},";
        }
        $lhs .= "${generic_gconstructor}";
        $lhs .= ')';
        my $content = "(${lhs} = ${rhs})";
        foreach my $i (1 .. $num_arg_types) {
            my $var_index = $num_arg_types - $i + 1;
            my $typ = $arg_types[$var_index - 1];
            my $var = "${var_prefix}${var_index}";
            my $guard = render_guard ($var, $typ);
            $content = "(! [${var}] : (${guard} => ${content}))";
        }
        my $tptp_name = "projection_${i}_g${g_nr}_${g_aid_lc}";
        my $formula = tptp_formula ($tptp_name, 'axiom', $content);
        push (@projections, $formula);
    }
    return @projections;
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
    # now render any adjectives, if present
    my @adjectives = $typ->findnodes ('Cluster[1]/Adjective');
    # # if there are any constants in the adjectives, die
    # if (any { $_->exists ('descendant::Const') } @adjectives) {
    #     confess 'How to deal with choice terms that have constants in some adjective? The choice term is:', $LF, $node->toString, $LF, $LF, 'The toplevel ancestor is:', $LF, $LF, $toplevel_ancestor->toString;
    # }
    my $num_adjectives = scalar @adjectives;
    foreach my $i (1 .. $num_adjectives) {
        my $adjective = $adjectives[$i - 1];
        my @constants = $adjective->findnodes ('descendant::Const');
        my $adj_aid = get_aid_attribute ($adjective);
        my $adj_nr = get_absnr_attribute ($adjective);
        my $adj_aid_lc = lc $adj_aid;
        my $adj_guard = "v${adj_nr}_${adj_aid_lc}";
        my $adj_is_negated = ($adjective->hasAttribute ('value')) && ($adjective->getAttribute ('value') eq 'false');
        if ($adj_is_negated) {
            $adj_guard = 'non_' . $adj_guard;
        }
        foreach my $constant (@constants) {
            my $const_typ = type_for_constant ($constant);
            my $const_typ_aid = get_aid_attribute ($const_typ);
            my $const_typ_kind = get_kind_attribute ($const_typ);
            my $const_typ_aid_lc = lc $const_typ_aid;
            my $const_typ_kind_lc = lc $const_typ_kind;
            my $const_typ_nr = get_absnr_attribute ($const_typ);
            my $const_typ_tptp = "${const_typ_kind_lc}${const_typ_nr}_${const_typ_aid_lc}";
            $typ_rendered .= "__${const_typ_tptp}";
        }
        $typ_rendered .= "_${adj_guard}";
    }
    return "epsilon_${typ_rendered}";
}

ensure_sensible_environment ();
ensure_sensible_commandline ();

my $article_doc = parse_xml_file ($article_xml_path);
my $article_root = $article_doc->documentElement ();

my %items = ();

my @xpaths = (
    'JustifiedTheorem',
    'Proposition',
    'RegistrationBlock/Registration/*[self::CCluster or self::FCluster or self::RCluster]',
    'RegistrationBlock/IdentifyRegistration/Identify',
    'RegistrationBlock/ReductionRegistration/Reduction',
    'DefinitionBlock/Definition/Constructor/Properties/*',
    'DefinitionBlock/Definition[not(@redefinition = "true")]/Existence',
    'DefinitionBlock/Definition[not(@redefinition = "true")]/Uniqueness',
    'DefinitionBlock/Definition[not(@redefinition = "true")]/Coherence',
    'DefinitionBlock/Definition[not(@redefinition = "true")]/Correctness/Coherence',
    'DefTheorem',
    'descendant::Proposition[following-sibling::*[1][self::From[not(ancestor::SchemeBlock)]]]',
);

my $xpath = join (' | ', @xpaths);

my @items = $article_root->findnodes ($xpath);

$pl->share (\%items);

$pl->foreach (\@items,
              sub { my $item = $_;
                    my $tptp_name = tptp_name_for_item ($item);
                    my $content = render_item ($item);
                    $items{$tptp_name} = $content;
                    return;
                }
          );

# Deal with value types of constructors
my @constructors = $article_root->findnodes ('descendant::Constructor[@kind = "K" or @kind = "G"]');
$pl->foreach (\@constructors,
              sub { my $constructor = $_;
                    my $tptp_name = tptp_name_for_value_type ($constructor);
                    my $content = value_type_for_constructor ($constructor);
                    $items{$tptp_name} = $content;
                    return;
                }
          );

sub content_node {
    my $item = shift;
    my $name = $item->nodeName;
    if ($name eq 'JustifiedTheorem') {
        (my $proposition) = $item->findnodes ('Proposition');
        return $proposition;
    } elsif ($name eq 'DefTheorem') {
        (my $proposition) = $item->findnodes ('Proposition');
        return $proposition;
    } else {
        return $item;
    }
}

# Value types of choice terms
foreach my $item (@items) {
    my $content_node = content_node ($item);
    my @choice_terms = $content_node->findnodes ('descendant::Choice');
    foreach my $choice (@choice_terms) {
        (my $typ) = $choice->findnodes ('Typ');
        if (! defined $typ) {
            confess 'Choice node lacks a Typ child.';
        }
        my $typ_aid = get_aid_attribute ($typ);
        my $typ_nr = get_nr_attribute ($typ);
        my $typ_kind = get_kind_attribute ($typ);
        my $typ_aid_lc = lc $typ_aid;
        my $typ_kind_lc = lc $typ_kind;
        my $typ_rendered = "${typ_kind_lc}${typ_nr}_${typ_aid_lc}";
        # now render any adjectives, if present
        my @adjectives = $typ->findnodes ('Cluster[1]/Adjective');
        # if there are any constants in the adjectives, die
        # if (any { $_->exists ('descendant::Const') } @adjectives) {
        #     confess 'How to deal with choice terms that have constants in some adjective?', $LF, $choice->toString, $LF, 'The item is:', $LF, $item->toString;
        # }
        my $num_adjectives = scalar @adjectives;
        foreach my $i (1 .. $num_adjectives) {
            my $adjective = $adjectives[$i - 1];
            my @constants = $adjective->findnodes ('descendant::Const');
            my $adj_aid = get_aid_attribute ($adjective);
            my $adj_nr = get_absnr_attribute ($adjective);
            my $adj_aid_lc = lc $adj_aid;
            my $adj_guard = "v${adj_nr}_${adj_aid_lc}";
            my $adj_is_negated = ($adjective->hasAttribute ('value')) && ($adjective->getAttribute ('value') eq 'false');
            if ($adj_is_negated) {
                $adj_guard = 'non_' . $adj_guard;
            }
            foreach my $constant (@constants) {
                my $const_typ = type_for_constant ($constant);
                my $const_typ_aid = get_aid_attribute ($const_typ);
                my $const_typ_kind = get_kind_attribute ($const_typ);
                my $const_typ_aid_lc = lc $const_typ_aid;
                my $const_typ_kind_lc = lc $const_typ_kind;
                my $const_typ_nr = get_absnr_attribute ($const_typ);
                my $const_typ_tptp = "${const_typ_kind_lc}${const_typ_nr}_${const_typ_aid_lc}";
                $typ_rendered .= "__${const_typ_tptp}";
            }
            $typ_rendered .= "_${adj_guard}";
        }
        my $choice_term = "epsilon_${typ_rendered}";
        my $content = "${typ_kind_lc}${typ_nr}_${typ_aid_lc}(${choice_term})";
        if ($num_adjectives > 0) {
            foreach my $i (1 .. $num_adjectives) {
                my $adjective = $adjectives[$i - 1];
                my $adj_aid = get_aid_attribute ($adjective);
                my $adj_nr = get_absnr_attribute ($adjective);
                my $adj_aid_lc = lc $adj_aid;
                my $adj_guard = "v${adj_nr}_${adj_aid_lc}";
                my $adj_is_negated = ($adjective->hasAttribute ('value')) && ($adjective->getAttribute ('value') eq 'false');
                if ($adj_is_negated) {
                    $adj_guard = '~' . $adj_guard;
                }
                $content .= " & ${adj_guard}(${choice_term})";
            }
        }
        $content = "(${content})";
        my $tptp_name = "dt_${choice_term}";
        $items{$tptp_name} = $content;
    }
}

# Structures: freeness, projections, widenings

my @gconstructors = $article_root->findnodes ('descendant::Constructor[@kind = "G"]');
$pl->foreach (\@gconstructors,
              sub { my $constructor = $_;
                    my $tptp_free_name = tptp_name_for_free ($constructor);
                    my $free_content = free_for_constructor ($constructor);
                    $items{$tptp_free_name} = $free_content;
                    return;
                }
          );

$pl->foreach (\@gconstructors,
              sub { my $constructor = $_;
                    my $tptp_widening_name = tptp_name_for_widening ($constructor);
                    my $widening_content = widening_for_structure_constructor ($constructor);
                    $items{$tptp_widening_name} = $widening_content;
                    return;
                }
          );

$pl->foreach (\@gconstructors,
              sub { my $constructor = $_;
                    my @projections = projections_for_structure_constructor ($constructor);
                    foreach my $projection (@projections) {
                        my $name = formula_name ($projection);
                        my $content = formula_content ($projection);
                        $items{$name} = $content;
                    }
                    return;
                }
          );

# Functors that don't have existence or uniqueness conditions
my @kconstructors = $article_root->findnodes ('descendant::Constructor[@kind = "K" and not(preceding-sibling::Existence) and not(preceding-sibling::Uniqueness) and not(parent::Definition[@redefinition = "true"]) and not(preceding-sibling::Correctness)]');
$pl->foreach (\@kconstructors,
              sub { my $constructor = $_;
                    my $tptp_existence_name = tptp_name_for_existence ($constructor);
                    my $tptp_uniqueness_name = tptp_name_for_uniqueness ($constructor);
                    my $existence_content = existence_for_constructor ($constructor);
                    my $uniqueness_content = uniqueness_for_constructor ($constructor);
                    if (defined $existence_content) {
                        $items{$tptp_existence_name} = $existence_content;
                    }
                    if (defined $uniqueness_content) {
                        $items{$tptp_uniqueness_name} = $uniqueness_content;
                    }
                    return;
                }
          );

foreach my $item (keys %items) {
    my $content = $items{$item};
    my $formula = tptp_formula ($item, 'theorem', $content);
    say $formula;
}

__END__
