#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;
use Data::Dumper;

my %mptp_for_item = ();
my %redefined_constructors = ();
my %mptp_lemmas = ();

my @function_properties = ('projectivity',
			   'involutiveness',
			   'idempotence',
			   'commutativity');
my @relation_properties = ('reflexivity',
			   'irreflexivity',
			   'symmetry',
			   'asymmetry',
			   'connectedness');
my @properties = (@function_properties, @relation_properties);

my %mptp_items = ();

sub load_redefined_constructors {
    while (defined (my $line = <DATA>)) {
	chomp $line;
	(my $new, my $old) = split (' ', $line);
	$redefined_constructors{$new} = $old;
    }
    return;
}

sub load_mptp_items {
    open (my $items_fh, '<', '00allmmlax.items')
	or die 'Cannot open 00allmmlax.items: ', $!;
    while (defined (my $item = <$items_fh>)) {
	chomp $item;
	$mptp_items{$item} = 0;
    }
    close $items_fh
	or die 'Cannot close input filehandle for 00allmmlax.items: ', $!;
    return;
}

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

    my $answer = undef;

    if ($item =~ / \A ([a-z_0-9]+) [:] ([a-z]+) [:] ([0-9]+) ([[] ([a-z]+) []])? \z /) {
	(my $article, my $kind, my $number, my $property) = ($1, $2, $3, $5);

	if (defined $property) {
	    if ($kind =~ /\A ([glmvrk])constructor \z/) {
		my $constructor_kind = $1;
		if ($property eq 'asymmetry') {
		    $answer = "antisymmetry_${1}${number}_${article}";
		} else {
		    $answer = "${property}_${constructor_kind}${number}_${article}";
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
	    } elsif ($kind =~ /([gklmu])constructor/) {
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
    } elsif ($item =~ /rq[a-zA-Z]/) {
	$answer = $item;
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

    # symmetry and reflexivity of equality is built-in
    if ($item eq 'hidden:rconstructor:1[symmetry]') {
	return 0;
    }

    if ($item eq 'hidden:rconstructor:1[reflexivity]') {
	return 0;
    }

    # rqAny is bogus
    if ($item eq 'rqAny') {
	return 0;
    }

    # the 'set' constructor does not need to be handled
    if ($item eq 'hidden:mconstructor:1') {
	return 0;
    }

    my $answer = undef;

    if ($item =~ / \A ([a-z_0-9]+) [:] ([a-z]+) [:] ([0-9]+) ([[] ([a-z]+) []] )? \z /) {
	(my $article, my $kind, my $number, my $property) = ($1, $2, $3, $5);
	if (defined $property) {
	    if ($property eq 'coherence') {
		$answer = 0;
	    } elsif ($property eq 'existence') {
		if ($kind =~ /\A [k]constructor \z/) {
		    $answer = 0;
		} else {
		    $answer = 1;
		}
	    } elsif ($property eq 'uniqueness') {
		$answer = 0;
	    } elsif ($property eq 'compatibility') {
		$answer = 0;
	    } elsif ($property eq 'sethood') {
		$answer = 0;
	    } else {
		$answer = 1;
	    }
	} elsif ($kind =~ /\A . definiens \z/) {
	    $answer = 1;
	} elsif ($kind =~ /[rv]constructor/) {
	    $answer = 0;
	} elsif ($kind =~ /[gjklmruv]pattern/) {
	    $answer = 0;
	} elsif ($kind eq 'lemma') {
	    $answer = 0;
	} else {
	    $answer = 1;
	}
    } elsif ($item =~ /\A rq [a-zA-Z]+ \z /) {
	$answer = 0;
    } else {
	die 'Error: cannot make sense of the item ', $item;
    }

    $handled_table{$item} = $answer;

    return $answer;

}

# load_lemmas ();

sub is_redefined_constructor {
    my $item = shift;
    if ($item =~ / \A ([a-z0-9_]+) [:] (.) constructor [:] ([0-9]+) \z /) {
	my $mptp = "${2}${3}_${1}";
	return (defined $redefined_constructors{$mptp});
    } elsif ($item =~ /\A ([a-z0-9_]+) [:] (.) definiens [:] ([0-9]+) \z/) {
	my $mptp = "${2}${3}_${1}";
	return (defined $redefined_constructors{$mptp});
    } else {
	return 0;
    }
}

sub redefine_constructor {
    my $item = shift;
    if ($item =~ / \A ([a-z0-9_]+) [:] (.) constructor [:] ([0-9]+) \z /) {
	my $mptp = "${2}${3}_${1}";
	return "redefinition_${mptp}";
    } elsif ($item =~ /\A ([a-z0-9_]+) [:] (.) definiens [:] ([0-9]+) \z/) {
	my $mptp = "${2}${3}_${1}";
	return "redefinition_${mptp}";
    } else {
	die 'Error: cannot redefine \'', $item, '\', because it was not redefined.';
    }
}

load_redefined_constructors ();
load_mptp_items ();

my %dependency_table = ();
my @items = ();
my %encountered = ();

my %ordering = ();

while (defined (my $line = <STDIN>)) {
    chomp $line;

    (my $item, my @deps) = split (' ', $line);

    my $num_items_so_far = scalar @items;
    push (@items, $item);

    $dependency_table{$item} = \@deps;

    $encountered{$item} = 0;
    foreach my $dep_item (@deps) {
	$encountered{$dep_item} = 0;
    }

    # Order the elements
    foreach my $dep_item (@deps) {
	$ordering{$item}{$dep_item} = -1;
	$ordering{$dep_item}{$item} = 1;
    }

    my $num_deps = scalar @deps;
    foreach my $i (1 .. $num_deps) {
	my $dep_i = $deps[$i - 1];
	foreach my $j ($i +1 .. $num_deps) {
	    my $dep_j = $deps[$j - 1];
	    $ordering{$dep_i}{$dep_j} = -1;
	    $ordering{$dep_j}{$dep_i} = 1;
	}
    }

    foreach my $i (1 .. $num_items_so_far) {
	my $earlier_item = $items[$i - 1];
	my @earlier_deps = @{$dependency_table{$earlier_item}};
	$ordering{$item}{$earlier_item} = 1;
	$ordering{$earlier_item}{$item} = -1;
	foreach my $earlier_dep (@earlier_deps) {
	    $ordering{$item}{$earlier_dep} = 1;
	    $ordering{$earlier_dep}{$item} = -1;
	}
    }
}

close *STDIN;

sub item_less_than {
    my $item_1 = shift;
    my $item_2 = shift;
    if ($item_1 eq $item_2) {
	return 0;
    } elsif (defined $ordering{$item_1}{$item_2}) {
	my $order = $ordering{$item_1}{$item_2};
	return $order;
    } else {
	print {*STDERR} 'We do not know how to order ', $item_1, ' and ', $item_2, '.', "\n";
	exit 1;
    }
}

foreach my $item (keys %encountered) {
    if (! defined $dependency_table{$item}) {
	$dependency_table{$item} = [];
    }
}

foreach my $item (sort { item_less_than ($a, $b) } keys %encountered) {

    my %printed_for_this_item = (
	'symmetry_r1_hidden' => 0,
	'reflexivity_r1_hidden' => 0,
    );

    if (handled ($item)) {
	my $mptp = to_mptp ($item);
	if (is_redefined_constructor ($item)) {
	    my $redefined = redefine_constructor ($item);
	    print $redefined;
	} else {
	    print $mptp;
	}

	my @deps = @{$dependency_table{$item}};
	my @sorted_deps = sort { item_less_than ($a, $b) } @deps;

	foreach my $dep_item (@sorted_deps) {
	    if ($item eq $dep_item) {
		# ignore self-dependencies.  These come from structure constructors
	    } elsif (is_redefined_constructor ($dep_item)) {
		my $redefined = redefine_constructor ($dep_item);
		print ' ', $redefined;
		$printed_for_this_item{$redefined} = 0;
	    } elsif (handled ($dep_item)) {
		my $dep_mptp = to_mptp ($dep_item);
		if (defined $printed_for_this_item{$dep_mptp}) {
		    # don't print
		} else {
		    print ' ', $dep_mptp;
		    $printed_for_this_item{$dep_mptp} = 0;
		}
	    }
	}

	# todo: for every constructor that's included, throw in
	# its associated deftheorem

	# Redefinition case
	if (is_redefined_constructor ($item)) {
	    # Look up a coherence/compatibility condition for the constructor
	    if ($item =~ /\A [a-z0-9_]+ [:] . (constructor | definiens) [:] [0-9]+ \z/) {
		my $coherence_item = "${item}[coherence]";
		my $compatibility_item = "${item}[compatibility]";
		if (defined $dependency_table{$coherence_item}) {
		    my @coherence_deps = @{$dependency_table{$coherence_item}};
		    foreach my $dep_item (@coherence_deps) {
			if (handled ($dep_item)) {
			    my $dep_mptp = to_mptp ($dep_item);
			    if (defined $printed_for_this_item{$dep_mptp}) {
				# don't print
			    } elsif (is_redefined_constructor ($dep_item)) {
				print ' ', redefine_constructor ($dep_item);
			    } else {
				print ' ', $dep_mptp;
			    }
			    $printed_for_this_item{$dep_mptp} = 0;
			}
		    }
		} elsif (defined $dependency_table{$compatibility_item}) {
		    my @compatibility_deps = @{$dependency_table{$compatibility_item}};
		    foreach my $dep_item (@compatibility_deps) {
			if (handled ($dep_item)) {
			    my $dep_mptp = to_mptp ($dep_item);
			    if (defined $printed_for_this_item{$dep_mptp}) {
				# don't print
			    } elsif (is_redefined_constructor ($dep_item)) {
				print ' ', redefine_constructor ($dep_item);
			    } else {
				print ' ', $dep_mptp;
			    }
			    $printed_for_this_item{$dep_mptp} = 0;
			}
		    }
		} else {
		    # warn 'To ', $item, ' neither a compatibility nor a coherence item is associated.';
		}
	    } else {
		die 'Error: cannot make sense of \'', $item, '\'.';
	    }
	}

	foreach my $dep_item (@sorted_deps) {
	    if (is_redefined_constructor ($dep_item)) {
		if ($dep_item =~ /\A ([a-z0-9_]+) [:] (.) constructor [:] ([0-9]+) \z/) {
		    (my $new_article, my $new_kind, my $new_number) = ($1, $2, $3);
		    my $new_mptp = "${new_kind}${new_number}_${new_article}";

		    foreach my $property (@properties) {

			if ($property eq 'asymmetry') {
			    $property = 'antisymmetry';
			}

			my $new_with_property = "${property}_${new_mptp}";

			if (defined $mptp_items{$new_with_property}) {
			    if (! defined $printed_for_this_item{$new_with_property}) {
				print ' ', $new_with_property;
				$printed_for_this_item{$new_with_property} = 0;
				# warn 'HEY: throwing in ', $new_with_property;
			    }
			}

			my $old_mptp = $redefined_constructors{$new_mptp};

			if (defined $old_mptp) {
			    if ($old_mptp =~ /\A (.) ([0-9]+) [_] ([a-z0-9_]+) \z/) {
				(my $old_kind, my $old_number, my $old_article)
				    = ($1, $2, $3);
				my $old_with_property = "${property}_${old_mptp}";
				if (defined $mptp_items{$old_with_property}) {
				    if (! defined $printed_for_this_item{$old_with_property}) {
					print ' ', $old_with_property;
					$printed_for_this_item{$old_with_property} = 0;
					# warn 'HEY: throwing in ', $old_with_property;
				    }
				}
			    } else {
				die 'Error: cannot make sense of the MPTP constructor \'', $old_mptp, '\'.';
			    }

			}
		    }
		}
	    }
	}

	foreach my $dep_item (@sorted_deps) {
	    if ($dep_item =~ /\A ([a-z0-9_]+) [:] gconstructor [:] ([0-9]+) \z/) {
		(my $article, my $number) = ($1, $2);
		my $free_item = "free_g${number}_${article}";
		if (! defined $printed_for_this_item{$free_item}) {
		    print ' ', $free_item;
		    $printed_for_this_item{$free_item} = 0;
		}
	    }
	}

	print "\n";

	# Typing for redefined functors
	if (is_redefined_constructor ($item)) {
	    if ($item =~ / [:] . constructor [:] /) {
		my $redefinition = redefine_constructor ($item);
		print $mptp, ' ', $redefinition, "\n";
	    }
	}

	# Mode existence conditions
	%printed_for_this_item = ();
	if ($item =~ / \A ([a-z0-9_]+) [:] ([lm]) constructor [:] ([0-9]+) \z/) {
	    (my $article, my $kind, my $number) = ($1, $2, $3);
	    my $mptp_existence = "existence_${kind}${number}_${article}";

	    print $mptp_existence;

	    foreach my $dep_item (@sorted_deps) {
		if ($item eq $dep_item) {
		    # ignore self-dependencies.  These come from structure constructors
		} elsif (handled ($dep_item)) {
		    my $dep_mptp = to_mptp ($dep_item);
		    if (defined $printed_for_this_item{$dep_mptp}) {
			# don't print
		    } elsif (is_redefined_constructor ($dep_item)) {
			print ' ', redefine_constructor ($dep_item);
		    } else {
			print ' ', $dep_mptp;

		    }
		    $printed_for_this_item{$dep_mptp} = 0;
		}
	    }

	    print "\n";

	}
    }
}

# Redefined constructors, accoding to the MPTP table for MML 4.181.1147
#
# Available at
# http://mizar.cs.ualberta.ca/~mptp/7.13.01_4.181.1147/MPTP2/00allmmlax

__DATA__
k2_compos_0 k4_xtuple_0
k1_normsp_1 k1_funct_1
k1_yellow14 k1_funct_1
k2_yellow14 k1_funct_1
r1_mfold_2 r1_t_0topsp
k1_square_1 k3_xxreal_0
k2_square_1 k4_xxreal_0
k4_square_1 k3_square_1
k5_square_1 k3_square_1
k7_square_1 k6_square_1
k1_pepin k3_square_1
k1_convex4 k13_pre_poly
r1_convex4 r1_hidden
k5_convex4 k1_valued_1
k6_convex4 k1_valued_1
k4_uproots k3_uproots
k15_sin_cos k14_sin_cos
k18_sin_cos k17_sin_cos
k21_sin_cos k20_sin_cos
k26_sin_cos k25_sin_cos
k32_sin_cos k31_sin_cos
k1_scmring4 k16_funcop_1
k1_mesfunc9 k1_funct_1
k5_mesfunc9 k1_binop_1
k10_mesfunc9 k1_funct_1
k1_random_1 k1_zfmisc_1
k3_random_1 k1_valued_1
k4_random_1 k45_valued_1
k5_random_1 k24_valued_1
k6_random_1 k18_valued_1
k7_random_1 k54_valued_1
k3_complex1 k1_complex1
k4_complex1 k2_complex1
k5_complex1 k1_xboole_0
k7_complex1 k1_xcmplx_0
k8_complex1 k2_xcmplx_0
k9_complex1 k3_xcmplx_0
k10_complex1 k4_xcmplx_0
k11_complex1 k6_xcmplx_0
k12_complex1 k5_xcmplx_0
k13_complex1 k7_xcmplx_0
k15_complex1 k14_complex1
k17_complex1 k16_complex1
k18_complex1 k16_complex1
k16_valued_2 k15_valued_2
k17_valued_2 k15_valued_2
k18_valued_2 k15_valued_2
k19_valued_2 k15_valued_2
k22_valued_2 k21_valued_2
k23_valued_2 k21_valued_2
k24_valued_2 k21_valued_2
k26_valued_2 k25_valued_2
k27_valued_2 k25_valued_2
k28_valued_2 k25_valued_2
k29_valued_2 k25_valued_2
k31_valued_2 k30_valued_2
k32_valued_2 k30_valued_2
k33_valued_2 k30_valued_2
k34_valued_2 k30_valued_2
k35_valued_2 k30_valued_2
k37_valued_2 k36_valued_2
k38_valued_2 k36_valued_2
k39_valued_2 k36_valued_2
k40_valued_2 k36_valued_2
k42_valued_2 k41_valued_2
k43_valued_2 k41_valued_2
k44_valued_2 k41_valued_2
k45_valued_2 k41_valued_2
k46_valued_2 k41_valued_2
k48_valued_2 k47_valued_2
k49_valued_2 k47_valued_2
k50_valued_2 k47_valued_2
k52_valued_2 k51_valued_2
k53_valued_2 k51_valued_2
k54_valued_2 k51_valued_2
k55_valued_2 k51_valued_2
k56_valued_2 k51_valued_2
k58_valued_2 k57_valued_2
k59_valued_2 k57_valued_2
k60_valued_2 k57_valued_2
k61_valued_2 k57_valued_2
k63_valued_2 k62_valued_2
k64_valued_2 k62_valued_2
k65_valued_2 k62_valued_2
k66_valued_2 k62_valued_2
k67_valued_2 k62_valued_2
k69_valued_2 k68_valued_2
k70_valued_2 k68_valued_2
k71_valued_2 k68_valued_2
k73_valued_2 k72_valued_2
k74_valued_2 k72_valued_2
k75_valued_2 k72_valued_2
k76_valued_2 k72_valued_2
k77_valued_2 k72_valued_2
k79_valued_2 k78_valued_2
k80_valued_2 k78_valued_2
k81_valued_2 k78_valued_2
k82_valued_2 k78_valued_2
k84_valued_2 k83_valued_2
k85_valued_2 k83_valued_2
k86_valued_2 k83_valued_2
k87_valued_2 k83_valued_2
k88_valued_2 k83_valued_2
k90_valued_2 k89_valued_2
k91_valued_2 k89_valued_2
k92_valued_2 k89_valued_2
r1_lfuzzy_1 r1_fuzzy_1
k1_lfuzzy_1 k1_fuzzy_1
k2_lfuzzy_1 k2_fuzzy_1
k3_lfuzzy_1 k4_funct_3
k2_topalg_4 k13_funct_3
k3_topalg_4 k11_mcart_1
k4_topalg_4 k12_mcart_1
k5_topalg_4 k13_funct_3
k6_topalg_4 k13_funct_3
k7_topalg_4 k11_mcart_1
k8_topalg_4 k12_mcart_1
k9_topalg_4 k11_mcart_1
k10_topalg_4 k12_mcart_1
k2_euclid_4 k1_euclid_4
k4_euclid_4 k3_euclid_4
k1_seqfunc k1_funct_1
k2_valuat_1 k1_valuat_1
k4_valuat_1 k3_relat_1
k6_valuat_1 k1_funct_1
k7_valuat_1 k1_funct_1
k1_fomodel2 k8_fomodel1
k3_fomodel2 k4_relat_1
m2_fomodel2 m1_fomodel2
k4_fomodel2 k1_funct_1
k12_fomodel2 k1_funct_1
k14_fomodel2 k13_fomodel2
k16_fomodel2 k15_fomodel2
k30_fomodel2 k7_finseq_1
k31_fomodel2 k5_finseq_1
k33_fomodel2 k27_fomodel2
k34_fomodel2 k7_fomodel1
k35_fomodel2 k29_fomodel2
k36_fomodel2 k28_fomodel2
k37_fomodel2 k5_fomodel2
k42_fomodel2 k41_fomodel2
k1_fuzzy_4 k1_binop_1
k2_fuzzy_4 k2_funct_4
r2_bhsp_3 r1_bhsp_3
k1_topgen_3 k10_xtuple_0
k1_relset_2 k9_relat_1
k2_relset_2 k10_relat_1
k4_relset_2 k3_relset_2
k6_relset_2 k5_relset_2
k7_relset_2 k3_relat_1
m1_glib_004 m1_subset_1
k5_glib_004 k1_xtuple_0
k9_glib_004 k1_funct_1
k15_glib_004 k1_funct_1
k9_filter_0 k8_filter_0
k10_filter_0 k8_filter_0
k1_ndiff_5 k1_funct_1
k4_urysohn1 k1_funct_1
k2_measure6 k17_member_1
k4_measure6 k5_member_1
v3_measure6 v2_seq_2
v4_measure6 v1_seq_2
k7_measure6 k35_valued_1
k3_msualg_8 k5_card_3
k1_lopban_2 k3_relat_1
k1_roughs_1 k1_funct_1
k2_roughs_1 k3_card_3
k3_radix_3 k1_radix_3
k4_radix_3 k2_radix_3
k1_clopban2 k3_relat_1
r2_compos_2 r1_compos_2
k2_scmfsa_2 k2_scm_inst
k18_scmfsa_2 k1_funct_1
m1_graph_5 m1_subset_1
k8_graph_5 k2_arytm_2
r1_vectsp_6 r1_hidden
v4_finset_1 v2_finset_1
k17_coh_sp k2_xtuple_0
k18_coh_sp k1_xtuple_0
k4_prelamb k1_funct_1
k6_prelamb k4_tarski
k8_prelamb k3_relat_1
k1_scm_inst k1_xboole_0
k2_groupp_1 k1_groupp_1
m2_valued_0 m1_valued_0
k1_valued_0 k9_nat_1
k2_valued_0 k3_relat_1
v9_valued_0 v3_funct_1
k1_pcs_0 k1_relat_1
k2_pcs_0 k2_xboole_0
r2_pcs_0 r1_pcs_0
v8_pcs_0 v7_pcs_0
k6_pcs_0 k1_funct_1
k9_pcs_0 k4_tarski
k10_pcs_0 k1_xtuple_0
k11_pcs_0 k2_xtuple_0
v18_pcs_0 v16_pcs_0
v19_pcs_0 v17_pcs_0
k15_pcs_0 k1_funct_1
k16_pcs_0 k1_funct_1
k27_pcs_0 k26_pcs_0
k3_rewrite2 k1_funct_1
k4_rewrite2 k1_rewrite2
k5_rewrite2 k2_rewrite2
k6_rewrite2 k4_relat_1
k1_matrix_7 k6_matrix_3
v1_matrix_7 v2_matrix_1
k3_matrix_7 k2_funct_1
k1_mmlquery k9_relat_1
k2_mmlquery k7_relat_1
k15_mmlquery k3_xboole_0
k16_mmlquery k2_xboole_0
k17_mmlquery k4_xboole_0
k19_mmlquery k3_xboole_0
k20_mmlquery k2_xboole_0
k21_mmlquery k4_xboole_0
k22_mmlquery k3_relat_1
k1_matrixr2 k6_matrixr1
k2_matrixr2 k1_matrixr1
k2_rfinseq k1_rfinseq
v4_ideal_1 v1_zfmisc_1
k4_ideal_1 k7_finseq_1
k5_ideal_1 k7_finseq_1
k6_ideal_1 k7_finseq_1
k12_ideal_1 k11_ideal_1
k13_ideal_1 k3_xboole_0
k15_ideal_1 k14_ideal_1
m1_armstrng m1_subset_1
k6_armstrng k4_tarski
k4_ordinal6 k1_funct_1
k5_ordinal6 k12_ordinal2
k13_ordinal6 k12_ordinal6
k1_integra5 k5_relat_1
r2_euclidlp r1_euclidlp
k4_functor2 k1_functor2
k2_integr15 k1_funct_1
k5_integr15 k1_funct_1
k3_quaterni k2_quaterni
k11_quaterni k4_quaterni
k12_quaterni k5_quaterni
k17_quaterni k13_quaterni
k18_quaterni k14_quaterni
k19_quaterni k15_quaterni
k20_quaterni k16_quaterni
k26_quaterni k7_quaterni
k27_quaterni k10_quaterni
k28_quaterni k8_quaterni
k29_quaterni k9_quaterni
k31_quaterni k30_quaterni
k3_rlvect_2 k13_pre_poly
r1_rlvect_2 r1_hidden
k7_rlvect_2 k1_valued_1
k1_seq_1 k1_funct_1
k2_simplex2 k1_simplex2
k1_stacks_1 k7_finseq_1
k2_stacks_1 k2_finseq_3
k4_stacks_1 k1_funct_4
r2_clvect_2 r1_clvect_2
k1_scm_1 k5_finseq_1
k2_waybel28 k1_waybel28
k7_scmringi k10_finseq_1
r1_borsuk_6 r1_borsuk_2
m3_msalimit m1_subset_1
k1_pua2mss1 k10_xtuple_0
m1_pua2mss1 m1_subset_1
k3_pua2mss1 k10_xtuple_0
k4_pua2mss1 k4_relat_1
k10_pua2mss1 k10_xtuple_0
r5_pua2mss1 r4_pua2mss1
k12_pua2mss1 k1_xtuple_0
k13_pua2mss1 k2_xtuple_0
k11_arytm_3 k1_xboole_0
k12_arytm_3 k1_arytm_3
k3_scmpds_1 k10_finseq_1
k3_ratfunc1 k4_tarski
k4_ratfunc1 k1_xtuple_0
k5_ratfunc1 k2_xtuple_0
k3_radix_1 k2_radix_1
k2_lexbfs k1_funct_1
k5_lexbfs k1_funct_1
k7_lexbfs k1_xtuple_0
k8_lexbfs k2_xtuple_0
k13_lexbfs k1_funct_1
k14_lexbfs k40_glib_000
k17_lexbfs k1_funct_1
k23_lexbfs k1_funct_1
k24_lexbfs k40_glib_000
k1_functor3 k13_functor0
k2_functor3 k13_functor0
k3_functor3 k13_functor0
k4_functor3 k13_functor0
k9_functor3 k1_functor2
k3_jordan10 k1_jordan10
k4_jordan10 k2_jordan10
k2_power k1_power
k4_power k3_power
k6_power k5_power
k8_power k7_power
k2_kolmog01 k5_relat_1
k3_kolmog01 k3_card_3
k5_setfam_1 k3_tarski
k6_setfam_1 k1_setfam_1
k9_setfam_1 k1_zfmisc_1
m2_cat_2 m1_subset_1
k3_cat_2 k1_funct_1
k4_cat_2 k2_cat_2
k6_cat_2 k15_funct_3
k7_cat_2 k3_funct_4
k9_cat_2 k4_tarski
k10_cat_2 k4_tarski
k11_cat_2 k1_binop_1
k16_cat_2 k13_funct_3
k17_cat_2 k15_funct_3
k1_nat_1 k2_xcmplx_0
k2_nat_1 k2_xcmplx_0
k3_nat_1 k3_xcmplx_0
k4_nat_1 k3_xcmplx_0
k6_nat_1 k3_xxreal_0
k7_nat_1 k4_xxreal_0
k8_nat_1 k1_funct_1
k10_nat_1 k9_nat_1
k15_mesfunc1 k11_mesfunc1
k16_mesfunc1 k12_mesfunc1
k17_mesfunc1 k13_mesfunc1
k18_mesfunc1 k14_mesfunc1
k19_mesfunc1 k10_relat_1
k2_pralg_2 k7_funct_6
k5_pralg_2 k1_funct_1
k1_member_1 k2_xxreal_3
k2_member_1 k5_xxreal_3
k3_member_1 k4_xxreal_3
k1_matrix15 k8_pre_poly
k6_matrix15 k5_matrix15
k6_pdiff_1 k5_pdiff_1
k11_pdiff_1 k10_pdiff_1
k2_setlim_1 k1_setlim_1
k4_setlim_1 k3_setlim_1
k5_setlim_1 k1_setlim_1
k6_setlim_1 k3_setlim_1
k2_borsuk_7 k2_xcmplx_0
k3_borsuk_7 k6_xcmplx_0
k4_borsuk_7 k7_valued_1
k5_borsuk_7 k13_valued_1
k6_borsuk_7 k7_valued_1
k7_borsuk_7 k13_valued_1
k14_borsuk_7 k13_borsuk_7
k1_metric_1 k1_binop_1
k3_metric_1 k7_funct_5
k4_metric_1 k2_metric_1
m2_finseq_2 m1_subset_1
k3_finseq_2 k13_finseq_1
k5_finseq_2 k2_finseq_2
k2_scmfsa_1 k2_scm_inst
k3_scmfsa_1 k1_scmfsa_i
k9_scmfsa_1 k1_funct_1
k2_matrix_2 k1_matrix_2
k4_matrix_2 k5_finseq_1
k5_matrix_2 k5_finseq_1
k6_matrix_2 k3_matrix_2
k8_matrix_2 k2_finseq_3
k11_matrix_2 k10_matrix_2
m1_matrix_2 m1_subset_1
k1_dynkin k13_funct_7
v1_dynkin v1_prob_2
k7_dynkin k6_dynkin
k2_topalg_2 k3_topmetr
r2_wellord2 r2_tarski
k1_goboard9 k3_finseq_5
k3_cohsp_1 k5_finsub_1
k4_cohsp_1 k3_tarski
k6_cohsp_1 k5_cohsp_1
k8_cohsp_1 k7_cohsp_1
k11_cohsp_1 k10_cohsp_1
k1_index_1 k2_xtuple_0
k2_index_1 k1_funct_1
k5_index_1 k4_tarski
k6_index_1 k4_tarski
k7_index_1 k1_funct_1
k8_index_1 k2_xtuple_0
k3_rfunct_3 k4_partfun1
m2_rfunct_3 m1_subset_1
k4_rfunct_3 k2_funcop_1
k5_rfunct_3 k1_valued_1
k6_rfunct_3 k45_valued_1
k7_rfunct_3 k18_valued_1
k8_rfunct_3 k1_rfunct_1
k9_rfunct_3 k54_valued_1
k10_rfunct_3 k30_valued_1
k11_rfunct_3 k4_rfunct_1
k12_rfunct_3 k24_valued_1
v1_knaster v6_cohsp_1
k5_knaster k3_knaster
k6_knaster k4_knaster
k2_funct_3 k1_funct_3
k5_funct_3 k4_funct_3
k6_funct_3 k4_relat_1
k9_funct_3 k7_funct_3
k10_funct_3 k8_funct_3
k12_funct_3 k11_funct_3
k14_funct_3 k13_funct_3
k16_funct_3 k15_funct_3
k2_scm_comp k6_trees_4
k3_scm_comp k1_trees_4
k11_scm_comp k1_funct_1
k1_groeb_3 k2_tarski
k2_yellow_3 k1_yellow_3
k4_yellow_3 k9_xtuple_0
k5_yellow_3 k10_xtuple_0
k6_yellow_3 k2_zfmisc_1
k7_yellow_3 k4_tarski
k8_yellow_3 k1_xtuple_0
k9_yellow_3 k2_xtuple_0
k10_yellow_3 k2_zfmisc_1
k11_yellow_3 k2_zfmisc_1
k12_yellow_3 k2_zfmisc_1
k13_yellow_3 k2_zfmisc_1
k1_prvect_2 k1_funct_1
k3_prvect_2 k1_funct_1
k5_prvect_2 k1_funct_1
k11_prvect_2 k1_funct_1
k1_matrix_6 k2_matrix_3
k2_matrix_6 k3_matrix_3
k3_matrix_6 k1_matrix_4
k4_matrix_6 k4_matrix_3
k1_kurato_0 k3_card_3
k2_kurato_0 k4_funct_6
v1_kurato_0 v2_prob_1
v2_kurato_0 v3_prob_1
k6_funcop_1 k3_funcop_1
k7_funcop_1 k2_funcop_1
k8_funcop_1 k2_funcop_1
k9_funcop_1 k4_funcop_1
k10_funcop_1 k5_funcop_1
k11_funcop_1 k10_xtuple_0
k12_funcop_1 k1_funcop_1
k15_funcop_1 k14_funcop_1
k17_funcop_1 k13_funcop_1
k18_funcop_1 k16_funcop_1
v1_prob_3 v1_prob_2
k4_prob_3 k1_funct_1
k5_prob_3 k3_card_3
m1_prob_3 m1_finseq_1
k8_prob_3 k1_funct_1
k9_prob_3 k3_relat_1
k2_matrix10 k4_matrixr1
k3_matrix10 k3_matrixr1
k4_matrix10 k5_matrixr1
k5_matrix10 k7_matrixr1
k7_aofa_i00 k7_valued_1
k8_aofa_i00 k13_valued_1
k9_aofa_i00 k24_valued_1
k10_aofa_i00 k2_aofa_i00
k11_aofa_i00 k3_aofa_i00
k12_aofa_i00 k4_aofa_i00
k13_aofa_i00 k5_aofa_i00
k14_aofa_i00 k6_aofa_i00
k15_aofa_i00 k45_valued_1
k16_aofa_i00 k1_valued_1
k18_aofa_i00 k17_aofa_i00
k19_aofa_i00 k2_funct_1
k20_aofa_i00 k2_funct_1
k21_aofa_i00 k1_funct_1
k28_aofa_i00 k30_valued_1
k29_aofa_i00 k7_valued_1
k30_aofa_i00 k13_valued_1
k31_aofa_i00 k24_valued_1
k32_aofa_i00 k45_valued_1
k33_aofa_i00 k1_valued_1
k34_aofa_i00 k18_valued_1
k35_aofa_i00 k2_aofa_i00
k36_aofa_i00 k3_aofa_i00
k37_aofa_i00 k4_aofa_i00
k38_aofa_i00 k5_aofa_i00
k39_aofa_i00 k6_aofa_i00
k89_aofa_i00 k1_binop_1
k2_ec_pf_2 k1_xtuple_0
k3_ec_pf_2 k2_xtuple_0
k9_ec_pf_2 k1_funct_1
k11_ec_pf_2 k1_binop_1
k3_catalg_1 k2_catalg_1
k8_catalg_1 k5_catalg_1
k9_catalg_1 k6_catalg_1
k10_catalg_1 k7_catalg_1
k1_stirl2_1 k8_relat_1
k2_stirl2_1 k1_funct_1
k4_altcat_1 k1_multop_1
k3_lattice5 k1_binop_1
k12_lattice5 k11_lattice5
k15_lattice5 k14_lattice5
k3_chord k4_finseq_4
k4_chord k1_chord
k2_lfuzzy_0 k4_xxreal_0
k3_lfuzzy_0 k3_xxreal_0
k7_lfuzzy_0 k1_funct_1
k8_lfuzzy_0 k1_binop_1
k9_lfuzzy_0 k1_funct_1
k6_lattice8 k5_lattice8
k9_lattice8 k8_lattice8
k1_wsierp_1 k7_finseq_1
k2_wsierp_1 k16_rvsum_1
k3_wsierp_1 k19_rvsum_1
k4_wsierp_1 k2_finseq_3
k5_wsierp_1 k2_finseq_3
k3_normsp_0 k2_normsp_0
k4_normsp_0 k2_normsp_0
k1_filter_1 k3_xboole_0
k6_filter_1 k3_funct_4
k9_filter_1 k4_tarski
k5_ringcat1 k4_ringcat1
m2_ringcat1 m1_subset_1
m3_ringcat1 m1_subset_1
m5_ringcat1 m1_subset_1
k11_ringcat1 k1_ringcat1
k12_ringcat1 k2_ringcat1
k3_lattices k1_lattices
k4_lattices k2_lattices
r3_lattices r1_lattices
k5_numbers k4_ordinal1
k6_numbers k1_xboole_0
k1_euler_2 k24_valued_1
k1_lopban_1 k5_funcop_1
k6_lopban_1 k1_funct_1
k9_lopban_1 k1_funct_1
k12_lopban_1 k1_funct_1
k17_lopban_1 k1_funct_1
k2_lpspace1 k1_binop_1
k8_lpspace1 k2_funcop_1
k4_tex_4 k2_tex_4
k3_midsp_1 k1_midsp_1
k5_midsp_1 k4_midsp_1
k2_fib_num3 k4_tarski
k4_glib_005 k1_funct_1
k1_waybel_6 k9_funct_7
k2_waybel_6 k1_funct_1
k2_nat_3 k1_nat_3
k3_nat_3 k1_nat_3
k8_nat_3 k7_nat_3
k11_nat_3 k10_nat_3
k2_scmpds_i k10_finseq_1
k1_nat_5 k5_relat_1
r1_filter_2 r1_hidden
k11_filter_2 k1_lattice2
k1_gr_cy_1 k16_rvsum_1
k5_real_ns1 k1_funct_1
k5_matrix_3 k4_matrix_3
k8_matrix_3 k2_zfmisc_1
k9_matrix_3 k6_finseqop
k2_seq_2 k1_seq_2
k1_abian k9_funct_7
r2_abian r1_abian
k1_finseqop k3_funcop_1
k2_finseqop k4_funcop_1
k3_finseqop k5_funcop_1
k4_finseqop k3_relat_1
k7_finseqop k6_finseqop
k1_rcomp_1 k1_xxreal_1
k2_rcomp_1 k4_xxreal_1
k3_rcomp_1 k2_xxreal_1
k4_rcomp_1 k3_xxreal_1
m3_topgrp_1 m2_topgrp_1
k3_topgrp_1 k3_struct_0
k4_topgrp_1 k3_struct_0
k6_topgrp_1 k1_topgrp_1
k7_topgrp_1 k2_topgrp_1
k8_topgrp_1 k3_group_1
v1_waybel_1 v2_funct_1
k1_waybel_1 k4_tarski
k1_jordan2b k5_finseq_1
k5_topmetr k17_borsuk_1
m1_card_lar m1_subset_1
k5_funct_5 k1_xboole_0
k8_funct_5 k6_funct_5
k9_funct_5 k7_funct_5
k10_funct_5 k1_funct_1
k11_funct_5 k1_funct_5
k12_funct_5 k3_funct_5
k13_funct_5 k2_funct_5
k2_complfld k14_complex1
m1_complfld m1_comptrig
k15_csspace k14_csspace
k16_csspace k2_normsp_1
k1_modelc_3 k13_modelc_2
k3_bvfunc26 k1_bvfunc26
k4_bvfunc26 k2_bvfunc26
k5_bvfunc26 k1_bvfunc26
k6_bvfunc26 k2_bvfunc26
k1_nattra_1 k5_relat_1
k2_nattra_1 k3_funct_4
k6_nattra_1 k3_nattra_1
k1_group_7 k1_funct_1
k4_group_7 k5_finseq_1
k5_group_7 k10_finseq_1
k6_group_7 k11_finseq_1
k2_absvalue k1_absvalue
k1_mcart_1 k4_xtuple_0
k2_mcart_1 k5_xtuple_0
k3_mcart_1 k2_xtuple_0
k4_mcart_1 k7_xtuple_0
k5_mcart_1 k8_xtuple_0
k6_mcart_1 k5_xtuple_0
k7_mcart_1 k2_xtuple_0
k8_mcart_1 k2_zfmisc_1
k9_mcart_1 k3_zfmisc_1
k10_mcart_1 k4_zfmisc_1
v1_goboard1 v3_relat_1
r2_fuzzy_1 r1_fuzzy_1
k1_descip_1 k3_finseq_5
k2_descip_1 k16_finseq_1
k3_descip_1 k1_rfinseq
k4_descip_1 k16_finseq_1
k5_descip_1 k1_rfinseq
k9_descip_1 k7_finseq_1
k10_descip_1 k1_funct_1
k11_descip_1 k1_funct_1
k12_descip_1 k4_bvfunc_1
k13_descip_1 k1_funct_1
k14_descip_1 k1_funct_1
k15_descip_1 k1_funct_1
k16_descip_1 k1_ordinal1
k2_substlat k2_xboole_0
k1_isocat_2 k2_funct_5
k8_isocat_2 k13_funct_3
k2_comseq_3 k1_newton
k5_comseq_3 k3_comseq_3
k6_comseq_3 k4_comseq_3
k7_comseq_3 k3_comseq_3
k8_comseq_3 k4_comseq_3
k9_comseq_3 k3_relat_1
k10_comseq_3 k2_series_1
k1_measure8 k1_funct_1
k3_measure8 k1_funct_1
k9_measure8 k6_measure8
k10_measure8 k3_relat_1
k11_measure8 k3_relat_1
k3_jgraph_1 k2_jgraph_1
k4_jgraph_1 k2_jgraph_1
k3_msafree k2_card_3
k2_henmodel k10_qc_lang1
k2_morph_01 k6_rusub_4
k2_lattice3 k8_filter_1
k6_lattice3 k2_relat_1
k12_lattice3 k11_lattice3
k13_lattice3 k10_lattice3
k2_setwiseo k1_tarski
k3_setwiseo k2_tarski
k4_setwiseo k1_enumset1
k5_setwiseo k2_xboole_0
k6_setwiseo k4_xboole_0
k8_setwiseo k7_relat_1
k2_waybel26 k5_card_3
k1_asympt_0 k1_valued_1
k2_asympt_0 k24_valued_1
k3_asympt_0 k7_valued_1
k4_asympt_0 k7_valued_1
k4_card_1 k1_ordinal1
k5_card_1 k1_card_1
k7_card_1 k6_card_1
k1_transgeo k3_relat_1
k2_funct_2 k2_funct_1
k3_funct_2 k1_funct_1
k4_funct_2 k11_mcart_1
k5_funct_2 k12_mcart_1
r1_funct_2 r1_hidden
r2_funct_2 r1_hidden
k9_funct_2 k1_funct_2
m2_funct_2 m1_subset_1
k10_funct_2 k7_partfun1
r1_borsuk_3 r1_t_0topsp
r2_borsuk_3 r1_t_0topsp
k2_comseq_2 k1_comseq_2
m1_osalg_1 m1_subset_1
k3_ami_wstd k2_ami_wstd
k4_pcomps_2 k1_funct_1
k3_trees_3 k1_funct_2
k6_trees_3 k7_funct_3
k7_trees_3 k8_funct_3
m2_trees_3 m1_subset_1
m3_trees_3 m2_trees_1
k10_trees_3 k5_trees_1
k1_msualg_6 k8_pboole
k2_msualg_6 k8_pboole
k1_msualg_3 k1_funct_1
k3_msualg_3 k8_pboole
r6_msualg_3 r5_msualg_3
k1_unialg_3 k9_unialg_2
m2_unialg_3 m1_subset_1
k2_newton k1_newton
k5_newton k4_newton
k6_newton k4_newton
k9_newton k3_newton
k13_newton k1_newton
k2_closure2 k1_closure2
m1_closure2 m1_subset_1
k5_closure2 k4_closure2
k6_closure2 k1_closure2
k7_closure2 k1_funct_1
k8_closure2 k3_relat_1
m1_nat_lat m1_subset_1
k1_euclid_7 k13_funct_3
k6_euclid_7 k6_matrixr2
m2_rlaffin3 m1_rlaffin3
k29_modelc_1 k9_funct_7
k3_waybel_3 k1_funct_1
k4_waybel_3 k1_funct_1
k5_waybel_3 k5_card_3
k2_partfun3 k1_partfun3
k3_partfun3 k1_partfun3
k4_partfun3 k4_rfunct_1
k5_partfun3 k1_rfunct_1
k1_finance1 k2_xxreal_1
k3_finance1 k18_valued_1
k2_finsop_1 k1_funct_4
k3_finsop_1 k9_xtuple_0
k1_real_1 k4_xcmplx_0
k2_real_1 k5_xcmplx_0
k3_real_1 k2_xcmplx_0
k4_real_1 k3_xcmplx_0
k5_real_1 k6_xcmplx_0
k6_real_1 k7_xcmplx_0
k7_real_1 k2_xcmplx_0
k8_real_1 k3_xcmplx_0
k9_real_1 k6_xcmplx_0
k10_real_1 k7_xcmplx_0
k3_group_2 k2_group_2
r1_group_2 r1_hidden
k10_group_2 k9_group_2
k1_yellow_1 k1_wellord2
k4_yellow_1 k1_funct_1
k2_eqrel_1 k3_xboole_0
k3_eqrel_1 k2_xboole_0
k4_eqrel_1 k3_xboole_0
k6_eqrel_1 k9_relat_1
k8_eqrel_1 k7_eqrel_1
k9_eqrel_1 k9_relat_1
r1_ordinal1 r1_tarski
k1_series_1 k1_valued_1
k3_series_1 k2_series_1
k5_series_1 k3_power
r1_polynom6 r2_quofield
k2_trees_4 k1_trees_4
k7_trees_4 k2_funct_6
k8_trees_4 k4_trees_4
m1_trees_4 m1_finseq_1
k2_msaterm k6_msafree
k3_msaterm k1_funct_1
k6_msaterm k4_trees_4
k8_msaterm k5_trees_2
r2_borsuk_2 r1_borsuk_2
k3_borsuk_2 k15_funct_3
r4_borsuk_2 r3_borsuk_2
k5_cqc_sim1 k3_relat_1
k8_cqc_sim1 k1_funct_1
k11_cqc_sim1 k4_relat_1
k7_cat_4 k3_cat_4
k8_cat_4 k4_cat_4
k25_cat_4 k21_cat_4
k26_cat_4 k22_cat_4
k27_cat_4 k21_cat_4
k28_cat_4 k22_cat_4
k1_ec_pf_1 k7_struct_0
k6_fomodel4 k5_fomodel4
k7_fomodel4 k4_tarski
k36_fomodel4 k35_fomodel4
k39_fomodel4 k38_fomodel4
k40_fomodel4 k13_fomodel0
k41_fomodel4 k13_fomodel0
k43_fomodel4 k42_fomodel4
k47_fomodel4 k46_fomodel4
k1_circuit2 k1_funct_4
k1_functor0 k2_funct_4
k2_functor0 k2_funct_4
k5_functor0 k4_functor0
k7_functor0 k4_functor0
k12_functor0 k11_functor0
k4_euclid_5 k11_finseq_1
k2_ami_2 k2_scm_inst
k6_qmax_1 k1_xtuple_0
k7_qmax_1 k2_xtuple_0
k11_normform k1_binop_1
k1_sublemma k1_funct_4
k2_sublemma k1_xtuple_0
k4_sublemma k17_substut1
k5_sublemma k20_substut1
k7_sublemma k4_tarski
k8_sublemma k1_xtuple_0
k12_sublemma k16_funcop_1
k14_sublemma k1_funct_4
k2_int_3 k16_complex1
k6_int_3 k3_int_3
k1_int_2 k16_complex1
k2_fomodel0 k13_finseq_1
k8_fomodel0 k7_fomodel0
k9_fomodel0 k4_funct_3
k18_fomodel0 k17_fomodel0
k19_fomodel0 k17_fomodel0
m1_orders_3 m1_subset_1
m1_autalg_1 m1_subset_1
k4_quofield k2_quofield
k5_quofield k3_quofield
k10_quofield k6_quofield
k6_bcialg_1 k1_bcialg_1
k1_rvsum_2 k10_xtuple_0
k3_rvsum_2 k1_valued_1
k4_rvsum_2 k1_valued_1
k5_rvsum_2 k30_valued_1
k6_rvsum_2 k30_valued_1
k7_rvsum_2 k45_valued_1
k8_rvsum_2 k45_valued_1
k9_rvsum_2 k24_valued_1
k10_rvsum_2 k24_valued_1
k11_rvsum_2 k18_valued_1
k12_rvsum_2 k18_valued_1
k4_matrix11 k3_relat_1
k1_pre_poly k7_finseq_1
k2_pre_poly k6_finseq_1
k3_pre_poly k5_finseq_1
k4_pre_poly k10_finseq_1
k6_pre_poly k2_wellord1
m1_pre_poly m1_subset_1
k9_pre_poly k7_partfun1
k10_pre_poly k1_card_3
k11_pre_poly k1_valued_1
k15_pre_poly k14_pre_poly
k19_pre_poly k13_pre_poly
k2_tbsp_1 k1_funct_1
k2_pre_ff k1_funct_1
k1_finsub_1 k2_xboole_0
k2_finsub_1 k4_xboole_0
k3_finsub_1 k3_xboole_0
k4_finsub_1 k5_xboole_0
k1_prob_2 k1_funct_1
v2_prob_2 v1_prob_2
k1_turing_1 k1_funct_4
k2_turing_1 k16_funcop_1
k3_turing_1 k1_ordinal1
k4_turing_1 k16_finseq_1
k24_turing_1 k1_xtuple_0
k25_turing_1 k2_xtuple_0
m5_bcialg_2 m4_bcialg_2
k1_amistd_4 k2_funct_7
k2_polyeq_1 k1_polyeq_1
k4_polyeq_1 k3_polyeq_1
k6_polyeq_1 k5_polyeq_1
k8_polyeq_1 k7_polyeq_1
k10_polyeq_1 k9_polyeq_1
k2_aff_1 k1_aff_1
r4_aff_1 r3_aff_1
r5_aff_1 r3_aff_1
r3_pboole r1_pboole
r6_pboole r1_hidden
v1_pboole v3_relat_1
v2_pboole v2_relat_1
r8_pboole r1_hidden
k1_yellow12 k1_binop_1
k1_closure1 k15_pralg_1
k2_closure1 k15_pralg_1
k3_closure1 k8_pboole
r1_boolealg r1_hidden
r3_boolealg r2_boolealg
k3_boolealg k2_boolealg
r4_boolealg r2_boolealg
k1_card_fil k7_setfam_1
k9_cat_1 k3_relat_1
k6_bilinear k2_bilinear
k1_scmfsa_3 k16_funcop_1
v1_yellow18 v2_funct_1
k1_laplace k1_binop_1
k10_laplace k5_finseq_1
k1_afinsq_1 k1_card_1
k2_afinsq_1 k9_xtuple_0
k5_afinsq_1 k3_afinsq_1
k14_afinsq_1 k1_ordinal4
k15_afinsq_1 k1_ordinal4
k4_fvsum_1 k3_fvsum_1
k6_fvsum_1 k5_fvsum_1
k8_fvsum_1 k7_fvsum_1
k10_fvsum_1 k9_fvsum_1
k12_fvsum_1 k11_fvsum_1
k5_matrix_1 k4_matrix_1
k8_matrix_1 k6_matrix_1
k9_matrix_1 k7_matrix_1
k2_interva1 k1_interva1
r1_interva1 r1_hidden
k7_interva1 k1_setfam_1
k8_interva1 k3_tarski
r2_interva1 r1_hidden
k5_glib_002 k4_glib_002
k2_measure4 k3_tarski
k1_waybel23 k3_tarski
k1_scmring3 k4_funct_4
k1_rearran1 k24_valued_1
k8_matrixc1 k5_matrixc1
k7_rusub_4 k6_rusub_4
k1_twoscomp k1_funct_1
v1_mesfunc2 v3_valued_0
k3_mesfunc2 k4_funct_3
k1_flang_1 k1_ordinal4
k2_flang_1 k4_afinsq_1
k3_flang_1 k3_afinsq_1
k4_flang_1 k1_tarski
k5_flang_1 k1_funct_1
r1_relset_1 r1_tarski
r2_relset_1 r1_hidden
k1_relset_1 k9_xtuple_0
k2_relset_1 k10_xtuple_0
k3_relset_1 k2_relat_1
k4_relset_1 k3_relat_1
k5_relset_1 k5_relat_1
k6_relset_1 k6_relat_1
k7_relset_1 k7_relat_1
k8_relset_1 k8_relat_1
r2_robbins4 r1_robbins4
k2_cqc_lang k16_funcop_1
k4_cqc_lang k10_qc_lang1
k5_cqc_lang k12_qc_lang1
k6_cqc_lang k13_qc_lang1
k7_cqc_lang k14_qc_lang1
k8_cqc_lang k2_qc_lang2
k9_cqc_lang k3_qc_lang2
k10_cqc_lang k4_qc_lang2
k11_cqc_lang k15_qc_lang1
k12_cqc_lang k5_qc_lang2
k1_scmpds_4 k1_ordinal4
k5_scmpds_4 k2_funct_7
k1_arrow k1_funct_1
m1_graph_3 m1_subset_1
k1_graph_3 k5_graph_2
k1_binop_2 k4_xcmplx_0
k2_binop_2 k5_xcmplx_0
k3_binop_2 k2_xcmplx_0
k4_binop_2 k6_xcmplx_0
k5_binop_2 k3_xcmplx_0
k6_binop_2 k7_xcmplx_0
k7_binop_2 k4_xcmplx_0
k8_binop_2 k5_xcmplx_0
k9_binop_2 k2_xcmplx_0
k10_binop_2 k6_xcmplx_0
k11_binop_2 k3_xcmplx_0
k12_binop_2 k7_xcmplx_0
k13_binop_2 k4_xcmplx_0
k14_binop_2 k5_xcmplx_0
k15_binop_2 k2_xcmplx_0
k16_binop_2 k6_xcmplx_0
k17_binop_2 k3_xcmplx_0
k18_binop_2 k7_xcmplx_0
k19_binop_2 k4_xcmplx_0
k20_binop_2 k2_xcmplx_0
k21_binop_2 k6_xcmplx_0
k22_binop_2 k3_xcmplx_0
k23_binop_2 k2_xcmplx_0
k24_binop_2 k3_xcmplx_0
k3_compos_1 k1_funct_1
k8_compos_1 k7_compos_1
k9_compos_1 k3_afinsq_1
k1_scmpds_3 k16_funcop_1
k1_mod_4 k2_funct_4
k1_seq_4 k1_tarski
k4_seq_4 k2_seq_4
k5_seq_4 k3_seq_4
k6_seq_4 k2_xxreal_2
k9_seq_4 k1_valued_1
k10_seq_4 k45_valued_1
k11_seq_4 k30_valued_1
k12_seq_4 k24_valued_1
k13_seq_4 k54_valued_1
k15_seq_4 k1_valued_1
k17_seq_4 k16_seq_4
k18_seq_4 k30_valued_1
k19_seq_4 k45_valued_1
k20_seq_4 k24_valued_1
k21_seq_4 k54_valued_1
k4_scmfsa_m k1_tarski
k5_scmfsa_m k2_tarski
k6_scmfsa_m k1_enumset1
k7_scmfsa_m k2_enumset1
k1_jordan k1_xtuple_0
k2_jordan k2_xtuple_0
k3_graph_1 k1_graph_1
k4_graph_1 k2_graph_1
m2_graph_1 m1_graph_1
v8_graph_1 v2_funct_1
k1_dist_1 k10_relat_1
k1_yellow16 k5_card_3
k2_yellow16 k1_funct_1
k1_group_10 k1_funct_1
m1_comput_1 m1_subset_1
k8_comput_1 k2_funct_7
k1_closure3 k13_pre_poly
k3_closure3 k2_xboole_0
k4_closure3 k3_xboole_0
k1_euclid_8 k11_finseq_1
k5_abcmiz_a k17_abcmiz_1
k6_abcmiz_a k18_abcmiz_1
k7_abcmiz_a k19_abcmiz_1
k12_abcmiz_a k29_abcmiz_1
k14_abcmiz_a k13_abcmiz_a
k2_vectsp_8 k3_vectsp_5
m2_vectsp_8 m1_subset_1
k2_binop_1 k1_binop_1
k3_binop_1 k1_binop_1
k5_binop_1 k1_binop_1
r8_binop_1 r1_hidden
k2_quin_1 k1_quin_1
k1_matrlin2 k10_matrlin
k5_matrlin2 k3_matrix_3
k6_matrlin2 k3_relat_1
k7_matrlin2 k2_matrlin2
k8_matrlin2 k2_matrlin2
k3_idea_1 k2_idea_1
r3_orders_2 r1_orders_2
k2_finseq_1 k1_finseq_1
k3_finseq_1 k1_card_1
k4_finseq_1 k9_xtuple_0
m2_finseq_1 m1_finseq_1
k8_finseq_1 k7_finseq_1
k9_finseq_1 k5_finseq_1
k12_finseq_1 k5_finseq_1
k17_finseq_1 k16_finseq_1
k2_realset1 k1_realset1
r2_gcd_1 r1_gcd_1
r4_gcd_1 r3_gcd_1
r7_gcd_1 r6_gcd_1
k5_waybel_9 k4_waybel_9
v1_measure1 v6_supinf_2
k1_measure1 k2_xboole_0
k2_measure1 k3_xboole_0
k3_measure1 k4_xboole_0
k4_measure1 k10_xtuple_0
k1_glib_003 k15_finseq_1
k9_glib_003 k8_glib_003
k2_zf_refle k1_zf_refle
k3_zf_refle k2_xboole_0
k4_zf_refle k3_card_3
k5_zf_refle k1_funct_1
k2_calcul_2 k1_calcul_2
k6_calcul_2 k2_finseq_2
r9_analmetr r8_analmetr
r10_analmetr r7_analmetr
k3_yellow20 k10_functor0
k4_yellow20 k14_functor0
k5_yellow20 k14_functor0
k3_yellow21 k1_yellow21
k4_yellow21 k1_yellow21
k2_finseq_4 k10_finseq_1
k3_finseq_4 k11_finseq_1
k9_finseq_4 k7_finseq_4
k10_finseq_4 k8_finseq_4
k11_ami_3 k16_funcop_1
k12_ami_3 k4_funct_4
k3_rlvect_1 k1_algstr_0
k13_osalg_2 k12_osalg_2
k13_fomodel1 k12_fomodel1
k15_fomodel1 k1_fomodel1
k16_fomodel1 k2_fomodel1
k17_fomodel1 k10_fomodel1
k18_fomodel1 k7_fomodel1
k24_fomodel1 k23_fomodel1
k26_fomodel1 k11_fomodel1
k28_fomodel1 k25_fomodel1
k29_fomodel1 k27_fomodel1
k32_fomodel1 k31_fomodel1
k35_fomodel1 k27_fomodel1
k37_fomodel1 k9_fomodel1
k39_fomodel1 k25_fomodel1
k1_monoid_0 k7_finseq_1
m4_monoid_0 m2_monoid_0
m5_monoid_0 m2_monoid_0
m6_monoid_0 m2_monoid_0
m7_monoid_0 m3_monoid_0
k3_monoid_0 k2_gr_cy_1
k1_supinf_1 k1_xxreal_0
k2_supinf_1 k2_xxreal_0
k2_robbins3 k8_filter_1
k4_robbins3 k2_lattices
k5_robbins3 k1_lattices
k7_group_1 k7_struct_0
k8_group_1 k6_algstr_0
k1_domain_1 k4_tarski
k2_domain_1 k1_xtuple_0
k3_domain_1 k2_xtuple_0
k4_domain_1 k3_xtuple_0
k5_domain_1 k6_xtuple_0
k6_domain_1 k1_tarski
k7_domain_1 k2_tarski
k8_domain_1 k1_enumset1
k9_domain_1 k2_enumset1
k10_domain_1 k3_enumset1
k11_domain_1 k4_enumset1
k12_domain_1 k5_enumset1
k13_domain_1 k6_enumset1
k14_domain_1 k13_mcart_1
k15_domain_1 k14_mcart_1
k16_domain_1 k15_mcart_1
k17_domain_1 k16_mcart_1
k3_int_5 k1_card_3
r3_yellow_4 r1_yellow_4
r4_yellow_4 r2_yellow_4
k2_yellow_4 k1_yellow_4
k4_yellow_4 k3_yellow_4
k3_yellow_5 k2_yellow_5
r2_yellow_5 r1_yellow_5
k4_ordinal4 k1_funct_1
k5_ordinal4 k3_relat_1
k6_ordinal4 k1_ordinal1
k7_ordinal4 k10_ordinal2
k8_ordinal4 k11_ordinal2
k2_setwop_2 k1_finseq_1
k2_topgen_4 k3_setfam_1
k3_topgen_4 k2_setfam_1
k3_trees_2 k1_funct_1
k6_trees_2 k4_trees_2
r2_necklace r1_necklace
k1_extreal1 k4_xxreal_3
k2_extreal1 k6_xxreal_3
k1_lmod_7 k3_vectsp_5
m2_lmod_7 m1_subset_1
m5_lmod_7 m1_subset_1
m8_lmod_7 m1_subset_1
m1_bintree2 m1_subset_1
k3_zf_lang k5_finseq_1
k1_extreal2 k4_xxreal_0
k2_extreal2 k3_xxreal_0
k3_finseq_6 k2_finseq_6
m1_trees_1 m1_subset_1
k9_matrix_5 k1_funct_1
k1_urysohn3 k1_funct_1
k4_urysohn3 k1_funct_1
k5_equation k4_tarski
m1_petri m1_subset_1
k2_petri k1_xtuple_0
k3_petri k2_xtuple_0
k4_petri k1_xtuple_0
k5_petri k2_xtuple_0
k10_petri k2_relat_1
v2_tsp_1 v6_pre_topc
v3_tsp_1 v6_pre_topc
k3_vfunct_2 k1_vfunct_1
k9_freealg k8_freealg
k12_freealg k8_freealg
k17_freealg k16_freealg
k19_freealg k16_freealg
k1_waybel18 k1_funct_1
k4_waybel18 k1_funct_1
k5_waybel18 k1_funct_1
k3_polynom1 k1_funct_1
k5_polynom1 k4_polynom1
k10_polynom1 k9_polynom1
k1_finseq_7 k2_funct_7
k2_finseq_7 k10_funct_7
k1_measure5 k4_xxreal_1
k1_finseq_8 k7_finseq_1
k3_finseq_8 k2_finseq_8
r3_finseq_8 r1_tarski
k20_glib_001 k9_glib_001
m5_glib_001 m1_subset_1
m6_glib_001 m1_subset_1
m7_glib_001 m1_subset_1
m8_glib_001 m1_subset_1
m9_glib_001 m1_subset_1
m10_glib_001 m1_subset_1
r1_pnproc_1 r1_hidden
k2_pnproc_1 k1_valued_1
k4_pnproc_1 k1_xtuple_0
k5_pnproc_1 k2_xtuple_0
m3_pnproc_1 m1_subset_1
k2_zf_model k1_zf_model
k5_zf_model k4_zf_model
k2_quatern3 k1_quatern3
k4_quatern3 k3_quatern3
k1_nat_d k5_int_1
k2_nat_d k6_int_1
k3_nat_d k5_int_1
k4_nat_d k6_int_1
r1_nat_d r1_int_1
k5_nat_d k2_int_2
k6_nat_d k3_int_2
k7_nat_d k1_xreal_0
k1_matrlin k2_finseq_3
k2_matrlin k5_finseq_1
k7_matrlin k8_pre_poly
k8_matrlin k7_finseq_1
k1_quatern2 k21_quaterni
k2_quatern2 k22_quaterni
k3_quatern2 k32_quaterni
k4_quatern2 k1_xcmplx_0
k6_quatern2 k5_quatern2
k8_quatern2 k7_quatern2
k17_quatern2 k30_quaterni
v5_mesfunc5 v3_mesfunc5
v6_mesfunc5 v4_mesfunc5
k4_mesfunc5 k1_funct_1
k7_mod_2 k5_mod_2
k5_robbins1 k1_lattices
k6_robbins1 k4_robbins1
k2_scpinvar k1_funct_1
k2_scmring2 k1_funct_1
r1_lattice7 r1_tarski
k3_topgen_2 k3_card_3
k4_topgen_2 k1_funct_1
m2_subset_1 m1_subset_1
k4_subset_1 k2_xboole_0
k5_subset_1 k5_xboole_0
k6_subset_1 k4_xboole_0
k7_subset_1 k4_xboole_0
k8_subset_1 k3_xboole_0
k9_subset_1 k3_xboole_0
r1_subset_1 r1_xboole_0
r2_subset_1 r1_xboole_0
k1_borsuk_1 k8_relat_1
k3_borsuk_1 k2_zfmisc_1
k4_borsuk_1 k4_tarski
k5_borsuk_1 k2_zfmisc_1
k6_borsuk_1 k2_zfmisc_1
k14_borsuk_1 k13_borsuk_1
k15_borsuk_1 k13_borsuk_1
k16_borsuk_1 k11_borsuk_1
k3_abcmiz_1 k1_xtuple_0
k5_abcmiz_1 k2_funct_1
k21_abcmiz_1 k1_xtuple_0
k22_abcmiz_1 k2_xtuple_0
k23_abcmiz_1 k1_xtuple_0
k24_abcmiz_1 k2_xtuple_0
k32_abcmiz_1 k15_abcmiz_1
k33_abcmiz_1 k16_abcmiz_1
k40_abcmiz_1 k4_tarski
k41_abcmiz_1 k1_xtuple_0
k42_abcmiz_1 k2_xtuple_0
k51_abcmiz_1 k1_lang1
k52_abcmiz_1 k2_lang1
k53_abcmiz_1 k2_matrix_7
k58_abcmiz_1 k57_abcmiz_1
k59_abcmiz_1 k56_abcmiz_1
k60_abcmiz_1 k56_abcmiz_1
k61_abcmiz_1 k56_abcmiz_1
k62_abcmiz_1 k56_abcmiz_1
k1_catalan2 k1_ordinal4
k3_catalan2 k8_afinsq_1
k5_exchsort k10_funct_7
r1_exchsort r2_orders_2
k2_simplex0 k1_simplex0
k9_simplex0 k8_simplex0
k1_funcsdom k1_binop_1
k2_funcsdom k1_funct_1
k3_funcsdom k3_funcop_1
k4_funcsdom k5_funcop_1
k3_sin_cos2 k2_sin_cos2
k6_sin_cos2 k5_sin_cos2
k9_sin_cos2 k8_sin_cos2
r2_topalg_6 r1_topalg_6
k1_classes2 k1_tarski
k2_classes2 k1_zfmisc_1
k3_classes2 k3_tarski
k4_classes2 k1_setfam_1
k5_classes2 k2_tarski
k6_classes2 k4_tarski
k7_classes2 k2_xboole_0
k8_classes2 k3_xboole_0
k9_classes2 k4_xboole_0
k10_classes2 k5_xboole_0
k11_classes2 k2_zfmisc_1
k12_classes2 k1_funct_2
k1_polynom2 k13_pre_poly
k2_polynom2 k3_relat_1
k1_scmfsa10 k4_funct_4
k1_gobrd13 k1_funct_1
k1_prvect_1 k3_funcop_1
k7_prvect_1 k5_funcop_1
k8_prvect_1 k1_funct_1
k9_prvect_1 k1_funct_1
k10_prvect_1 k1_funct_1
k11_prvect_1 k7_funct_6
k12_prvect_1 k1_binop_1
k14_prvect_1 k1_funct_1
k16_prvect_1 k1_funct_1
k1_prob_4 k1_funct_1
m2_prob_4 m1_prob_4
k11_facirc_1 k1_funct_1
k4_finseq_5 k3_finseq_5
k1_supinf_2 k1_xboole_0
k2_supinf_2 k2_xxreal_3
k3_supinf_2 k1_xxreal_3
k4_supinf_2 k3_xxreal_3
k5_supinf_2 k8_member_1
k6_supinf_2 k4_member_1
k7_supinf_2 k2_xxreal_2
k8_supinf_2 k1_xxreal_2
k9_supinf_2 k10_xtuple_0
k12_supinf_2 k1_funct_1
v4_supinf_2 v4_card_3
k17_supinf_2 k10_xtuple_0
k4_lang1 k1_tarski
k5_lang1 k2_tarski
k10_lang1 k2_lang1
k11_lang1 k3_relat_1
k13_lang1 k18_finseq_1
k1_pdiff_6 k1_funct_1
k1_midsp_3 k2_funct_7
k2_midsp_3 k5_finseq_1
k3_midsp_3 k7_finseq_1
k5_midsp_3 k1_funct_1
k6_midsp_3 k2_funct_7
m2_conlat_1 m1_subset_1
k8_conlat_1 k7_conlat_1
m3_conlat_1 m1_subset_1
k2_zf_lang1 k2_funct_7
k4_zf_lang1 k3_zf_lang1
k6_zf_lang1 k5_zf_lang1
k1_bvfunc_1 k13_margrel1
k2_bvfunc_1 k14_margrel1
k5_bvfunc_1 k3_bvfunc_1
k6_bvfunc_1 k4_bvfunc_1
k9_bvfunc_1 k7_bvfunc_1
k10_bvfunc_1 k8_bvfunc_1
m1_bvfunc_1 m1_subset_1
k15_bvfunc_1 k11_eqrel_1
k19_bvfunc_1 k8_xboolean
k20_bvfunc_1 k9_xboolean
k21_bvfunc_1 k7_xboolean
k1_integr16 k3_comseq_3
k2_integr16 k4_comseq_3
k4_integr16 k1_funct_1
k5_scmring1 k1_funct_1
k6_scmring1 k10_finseq_1
k2_fin_topo k16_funcop_1
k3_cgames_1 k1_cgames_1
k12_cgames_1 k10_cgames_1
k2_lopclset k2_xboole_0
k3_lopclset k3_xboole_0
k9_lopclset k8_lopclset
k13_lopclset k8_lopclset
k1_polyform k3_xcmplx_0
k2_polyform k1_newton
k3_polyform k5_finseq_1
k4_polyform k10_finseq_1
k5_polyform k11_finseq_1
k6_polyform k7_finseq_1
r2_group_3 r1_group_3
r4_group_3 r3_group_3
r6_group_3 r5_group_3
k6_substut1 k5_relat_1
k18_substut1 k1_xtuple_0
k19_substut1 k2_xtuple_0
k22_substut1 k1_xtuple_0
k23_substut1 k2_xtuple_0
k25_substut1 k4_tarski
k33_substut1 k1_xtuple_0
k34_substut1 k2_xtuple_0
k39_substut1 k37_substut1
k2_matrix14 k1_matrix14
k2_monoid_1 k1_monoid_1
k3_monoid_1 k3_funcop_1
k4_monoid_1 k2_finseq_2
k5_monoid_1 k2_funcop_1
k6_monoid_1 k5_funcop_1
k7_monoid_1 k4_funcop_1
k10_monoid_1 k1_funct_1
k11_monoid_1 k9_monoid_1
k12_monoid_1 k9_monoid_1
k14_monoid_1 k10_xtuple_0
k15_monoid_1 k1_funct_1
k16_monoid_1 k4_funct_3
k17_monoid_1 k2_funcop_1
k23_monoid_1 k22_monoid_1
k5_funct_4 k4_funct_4
k7_funct_4 k1_funct_4
k2_yellow_7 k5_funct_6
k2_measure7 k1_funct_1
k4_measure7 k1_funct_1
k12_measure7 k10_measure7
k2_bintree1 k3_xtuple_0
k2_openlatt k2_xboole_0
k3_openlatt k3_xboole_0
k11_openlatt k2_xboole_0
k12_openlatt k3_xboole_0
k16_openlatt k8_openlatt
k18_openlatt k8_openlatt
k1_scmpds_9 k4_funct_4
k2_jordan1k k7_weierstr
k3_jordan1k k10_weierstr
k4_jordan1k k1_jordan1k
k1_bor_cant k5_xxreal_0
k2_toprealb k1_toprealb
k3_toprealb k1_toprealb
k1_chain_1 k1_zfmisc_1
k2_chain_1 k1_funct_1
k7_chain_1 k5_xboole_0
k11_chain_1 k5_chain_1
k12_chain_1 k6_chain_1
k13_chain_1 k5_xboole_0
k14_chain_1 k10_chain_1
k1_waybel10 k7_lattice3
m2_modcat_1 m1_subset_1
m4_modcat_1 m1_subset_1
m6_modcat_1 m1_subset_1
k3_modcat_1 k2_modcat_1
k1_zf_fund1 k3_relat_1
k4_dickson k3_dickson
m1_trees_9 m1_subset_1
k4_trees_9 k3_trees_9
k5_trees_9 k3_trees_9
k10_trees_9 k9_trees_9
k11_trees_9 k9_trees_9
v1_vectsp10 v3_funct_1
k4_fomodel3 k3_fomodel3
k6_fomodel3 k5_fomodel3
k7_fomodel3 k5_fomodel3
k10_fomodel3 k9_fomodel3
k12_fomodel3 k11_fomodel3
k13_fomodel3 k9_fomodel3
k16_fomodel3 k11_fomodel3
k19_fomodel3 k18_fomodel3
k21_fomodel3 k20_fomodel3
k23_fomodel3 k22_fomodel3
k24_fomodel3 k17_fomodel0
k25_fomodel3 k13_fomodel0
k27_fomodel3 k26_fomodel3
k29_fomodel3 k28_fomodel3
k30_fomodel3 k28_fomodel3
k35_fomodel3 k34_fomodel3
k36_fomodel3 k34_fomodel3
k9_bspace k8_bspace
k10_bspace k1_funct_1
k9_sincos10 k5_sincos10
k10_sincos10 k6_sincos10
k11_sincos10 k7_sincos10
k12_sincos10 k8_sincos10
k2_zfrefle1 k1_zfrefle1
k1_matrprob k1_funct_1
k2_matrprob k5_finseq_1
k1_mssubfam k1_mboolean
k2_mssubfam k1_funct_1
k4_mssubfam k3_mssubfam
k5_mssubfam k1_mboolean
k6_vfunct_1 k1_vfunct_1
k2_multop_1 k1_multop_1
k4_multop_1 k3_multop_1
k1_partfun2 k4_relat_1
k2_partfun2 k2_funct_1
k3_partfun2 k6_relat_1
k4_partfun2 k2_funcop_1
v1_partfun2 v3_funct_1
k3_prepower k2_prepower
k5_prepower k4_prepower
k7_prepower k6_prepower
k10_prepower k9_prepower
k2_topalg_5 k1_topalg_5
k6_topalg_5 k5_topalg_5
k1_pencil_1 k1_funct_1
k2_pencil_1 k1_funct_1
k1_prob_1 k3_card_3
m1_prob_1 m1_subset_1
k5_prob_1 k3_xboole_0
k6_prob_1 k2_xboole_0
k7_prob_1 k4_xboole_0
k8_prob_1 k3_relat_1
k13_prob_1 k1_funct_1
k4_waybel35 k3_waybel35
k6_waybel35 k5_waybel35
k2_matrix13 k6_matrix_3
k3_matrix13 k1_tarski
k4_matrix13 k2_tarski
k5_matrix13 k14_finseq_1
k9_matrix13 k10_xtuple_0
m1_group_6 m1_group_2
k2_group_6 k9_group_2
k3_group_6 k9_group_2
r2_group_6 r1_group_6
k1_groeb_1 k1_tarski
k3_funct_7 k2_funct_7
k8_funct_7 k4_funct_7
k12_funct_7 k11_funct_7
k14_funct_7 k13_funct_7
k15_funct_7 k2_funct_7
k2_pencil_2 k7_relat_1
k3_pencil_2 k8_relat_1
m1_dtconstr m1_subset_1
k1_dtconstr k14_trees_3
k2_dtconstr k11_mcart_1
k3_dtconstr k12_mcart_1
k6_dtconstr k1_lang1
k7_dtconstr k2_lang1
k8_dtconstr k1_trees_4
k9_dtconstr k4_trees_4
k10_dtconstr k5_trees_4
k4_circcmb3 k5_circcomb
k5_circcmb3 k7_circcomb
k6_circcmb3 k2_circcomb
k7_circcmb3 k3_circcomb
k1_cat_5 k13_mcart_1
k2_cat_5 k14_mcart_1
m1_cat_5 m1_subset_1
k5_cat_5 k13_mcart_1
k6_cat_5 k14_mcart_1
k8_cat_5 k2_xtuple_0
k13_cat_5 k2_xtuple_0
k14_cat_5 k13_mcart_1
k15_cat_5 k14_mcart_1
k16_cat_5 k2_xtuple_0
k17_cat_5 k13_mcart_1
k18_cat_5 k14_mcart_1
k12_grcat_1 k10_grcat_1
k14_grcat_1 k13_grcat_1
m2_grcat_1 m1_subset_1
m3_grcat_1 m1_subset_1
m6_grcat_1 m1_subset_1
m7_grcat_1 m1_subset_1
k19_grcat_1 k7_grcat_1
k20_grcat_1 k8_grcat_1
k11_card_3 k5_relat_1
k3_classes1 k2_classes1
k9_osafree k3_osafree
k1_partfun1 k3_relat_1
k2_partfun1 k5_relat_1
k6_partfun1 k4_relat_1
k1_measure3 k10_xtuple_0
k1_circcomb k1_funct_4
k2_trees_a k1_tarski
k1_group_4 k1_finseq_3
r1_rmod_4 r1_hidden
r1_heyting1 r1_tarski
k2_heyting1 k1_tarski
k7_binom k4_newton
k2_rfunct_1 k1_rfunct_1
k3_rfunct_1 k1_rfunct_1
k5_rfunct_1 k4_rfunct_1
k6_rfunct_1 k4_rfunct_1
k7_rfunct_1 k4_funct_3
k1_cat_3 k16_funcop_1
k2_topreal7 k4_tarski
k3_topreal7 k1_xtuple_0
k4_topreal7 k2_xtuple_0
k2_ranknull k7_relat_1
k5_ranknull k7_relat_1
k7_ranknull k8_relat_1
k2_jordan1h k16_matrix_1
k8_ordinal3 k10_ordinal2
k9_ordinal3 k11_ordinal2
r2_tsep_2 r1_tsep_2
r4_tsep_2 r3_tsep_2
k1_bvfunc_2 k1_partit1
m1_bvfunc_2 m1_subset_1
k4_bvfunc_2 k1_tarski
k1_waybel_5 k1_funct_1
k2_waybel_5 k7_funct_6
k3_waybel_5 k1_funct_1
k6_waybel_5 k1_funct_5
k7_waybel_5 k2_funcop_1
k4_mesfunc7 k3_mesfunc7
k5_modal_1 k8_trees_2
k7_modal_1 k5_trees_2
k1_altcat_2 k2_funct_4
r2_altcat_2 r1_altcat_2
k2_algstr_4 k1_funct_1
k2_vectmetr k1_vectmetr
k1_topalg_3 k3_relat_1
k2_topalg_3 k3_relat_1
k2_diff_1 k1_diff_1
k12_diff_1 k1_funct_1
k1_margrel1 k2_funcop_1
r1_margrel1 r1_hidden
k7_margrel1 k1_xboolean
k8_margrel1 k2_xboolean
k9_margrel1 k3_xboolean
k10_margrel1 k4_xboolean
k12_margrel1 k11_margrel1
k15_margrel1 k13_margrel1
k16_margrel1 k14_margrel1
k17_margrel1 k5_finseq_1
k19_margrel1 k18_margrel1
m5_margrel1 m1_subset_1
k1_measure2 k1_setfam_1
k2_measure2 k3_tarski
k2_integr18 k1_funct_1
k7_genealg1 k1_genealg1
k8_genealg1 k2_genealg1
k9_genealg1 k3_genealg1
k10_genealg1 k4_genealg1
k11_genealg1 k5_genealg1
k12_genealg1 k6_genealg1
k1_graphsp k2_funct_7
k3_graphsp k3_relat_1
k4_graphsp k1_funct_1
k6_graphsp k1_funct_1
k12_graphsp k11_graphsp
k1_binarith k5_xboolean
k2_binarith k10_xboolean
k9_binarith k7_finseq_1
k10_binarith k5_finseq_1
k5_sin_cos9 k3_sin_cos9
k6_sin_cos9 k4_sin_cos9
k1_nagata_1 k1_funct_1
k2_nagata_1 k3_card_3
k3_nagata_1 k1_valued_1
k1_msualg_4 k1_funct_1
k2_msualg_4 k1_funct_1
k5_msualg_4 k1_funct_1
r2_osalg_3 r1_osalg_3
r3_osalg_3 r1_osalg_3
k1_integra2 k23_member_1
k2_integra2 k1_funct_1
k11_group_9 k3_relat_1
r3_group_9 r2_group_9
k1_pralg_1 k11_mcart_1
k2_pralg_1 k12_mcart_1
k10_pralg_1 k1_funct_1
k11_pralg_1 k1_funct_1
k14_pralg_1 k1_funct_1
k16_pralg_1 k15_pralg_1
k1_bcialg_6 k1_funct_1
k1_rvsum_1 k10_xtuple_0
k4_rvsum_1 k1_valued_1
k5_rvsum_1 k1_valued_1
k6_rvsum_1 k30_valued_1
k7_rvsum_1 k30_valued_1
k8_rvsum_1 k45_valued_1
k9_rvsum_1 k45_valued_1
k10_rvsum_1 k24_valued_1
k11_rvsum_1 k24_valued_1
k12_rvsum_1 k39_valued_1
k13_rvsum_1 k39_valued_1
k14_rvsum_1 k18_valued_1
k15_rvsum_1 k18_valued_1
k17_rvsum_1 k16_rvsum_1
k18_rvsum_1 k16_rvsum_1
k20_rvsum_1 k19_rvsum_1
k21_rvsum_1 k19_rvsum_1
k23_rvsum_1 k22_rvsum_1
k1_oppcat_1 k2_funct_4
k1_fintopo5 k9_relat_1
k1_polyeq_3 k3_square_1
k2_polyeq_3 k3_polyeq_1
m1_incproj m2_collsp
v1_revrot_1 v3_funct_1
r1_revrot_1 r2_finseq_4
k1_toler_1 k2_wellord1
k2_toler_1 k2_wellord1
k3_commacat k13_mcart_1
k4_commacat k14_mcart_1
k2_bhsp_1 k1_bhsp_1
k6_bhsp_1 k2_normsp_1
k2_matrixj1 k1_funct_1
k3_matrixj1 k7_finseq_1
k4_matrixj1 k5_finseq_1
k5_matrixj1 k10_finseq_1
k6_matrixj1 k16_finseq_1
k7_matrixj1 k1_rfinseq
k10_matrixj1 k8_matrixj1
k11_matrixj1 k9_matrixj1
k13_matrixj1 k12_matrixj1
k14_matrixj1 k1_funct_1
k15_matrixj1 k7_finseq_1
k16_matrixj1 k5_finseq_1
k17_matrixj1 k10_finseq_1
k18_matrixj1 k16_finseq_1
k19_matrixj1 k1_rfinseq
k20_matrixj1 k12_matrixj1
k22_matrixj1 k21_matrixj1
k3_euclid k54_valued_1
k5_euclid k4_euclid
k6_euclid k30_valued_1
k7_euclid k1_valued_1
k8_euclid k45_valued_1
k9_euclid k24_valued_1
k10_euclid k54_valued_1
k11_euclid k39_valued_1
k16_euclid k4_euclid
k19_euclid k10_finseq_1
k21_euclid k20_euclid
k5_groeb_2 k7_finseq_1
k3_pscomp_1 k5_relat_1
k3_sin_cos6 k2_sin_cos6
k6_sin_cos6 k5_sin_cos6
k1_waybel20 k15_funct_3
k1_grnilp_1 k8_group_5
r2_metric_2 r1_metric_2
r3_metric_2 r1_metric_2
k1_recdef_1 k1_funct_1
k2_complsp2 k45_valued_1
k3_complsp2 k1_valued_1
k4_complsp2 k24_valued_1
k5_complsp2 k30_valued_1
m1_partit_2 m1_subset_1
k2_partit_2 k2_zfmisc_1
v2_partit_2 v1_partit_2
k2_matrixj2 k1_matrixj2
k3_matrixj2 k7_finseq_1
k4_matrixj2 k16_finseq_1
k5_matrixj2 k1_rfinseq
k6_matrixj2 k7_finseq_1
k7_matrixj2 k16_finseq_1
k8_matrixj2 k1_rfinseq
k1_clopban1 k5_funcop_1
k4_clopban1 k1_funct_1
k7_clopban1 k1_funct_1
k10_clopban1 k1_funct_1
k15_clopban1 k1_funct_1
k4_int_1 k3_int_1
k10_glib_000 k8_glib_000
k11_glib_000 k9_glib_000
k15_glib_000 k14_glib_000
k17_glib_000 k16_glib_000
k24_glib_000 k6_glib_000
k25_glib_000 k7_glib_000
k32_glib_000 k30_glib_000
k33_glib_000 k31_glib_000
k35_glib_000 k34_glib_000
k2_yellow_6 k1_funct_1
k7_yellow_6 k1_funct_1
k2_graph_2 k1_graph_2
k4_graph_2 k3_graph_2
k1_toprealc k14_valued_2
k3_toprealc k2_toprealc
k4_toprealc k1_funct_1
k5_toprealc k1_funct_1
k6_toprealc k1_binop_1
k7_toprealc k7_valued_1
k8_toprealc k13_valued_1
k9_toprealc k24_valued_1
k10_toprealc k14_valued_2
k11_toprealc k18_valued_1
k12_toprealc k39_valued_1
k13_toprealc k50_valued_1
k14_toprealc k2_funct_7
k18_toprealc k2_toprealc
k1_substut2 k4_tarski
k2_substut2 k4_tarski
k3_substut2 k16_funcop_1
k5_substut2 k2_xtuple_0
r1_yellow10 r6_waybel_1
k1_waybel27 k1_funct_1
k3_waybel27 k1_funct_1
k1_polynom3 k2_finseq_3
k2_polynom3 k10_finseq_1
k3_polynom3 k7_finseq_1
k4_polynom3 k8_pre_poly
k8_polynom3 k2_normsp_1
k12_polynom3 k5_finseq_1
k13_polynom3 k11_polynom3
k2_valued_1 k1_valued_1
k3_valued_1 k1_valued_1
k4_valued_1 k1_valued_1
k5_valued_1 k1_valued_1
k6_valued_1 k1_valued_1
k8_valued_1 k7_valued_1
k9_valued_1 k7_valued_1
k10_valued_1 k7_valued_1
k11_valued_1 k7_valued_1
k12_valued_1 k7_valued_1
k14_valued_1 k13_valued_1
k15_valued_1 k13_valued_1
k16_valued_1 k13_valued_1
k17_valued_1 k13_valued_1
k19_valued_1 k18_valued_1
k20_valued_1 k18_valued_1
k21_valued_1 k18_valued_1
k22_valued_1 k18_valued_1
k23_valued_1 k18_valued_1
k25_valued_1 k24_valued_1
k26_valued_1 k24_valued_1
k27_valued_1 k24_valued_1
k28_valued_1 k24_valued_1
k29_valued_1 k24_valued_1
k31_valued_1 k30_valued_1
k32_valued_1 k30_valued_1
k33_valued_1 k30_valued_1
k34_valued_1 k30_valued_1
k36_valued_1 k35_valued_1
k37_valued_1 k35_valued_1
k38_valued_1 k35_valued_1
k40_valued_1 k39_valued_1
k41_valued_1 k39_valued_1
k42_valued_1 k39_valued_1
k43_valued_1 k39_valued_1
k44_valued_1 k39_valued_1
k46_valued_1 k45_valued_1
k47_valued_1 k45_valued_1
k48_valued_1 k45_valued_1
k49_valued_1 k45_valued_1
k51_valued_1 k50_valued_1
k52_valued_1 k50_valued_1
k53_valued_1 k50_valued_1
k55_valued_1 k54_valued_1
k56_valued_1 k54_valued_1
k57_valued_1 k54_valued_1
k58_valued_1 k54_valued_1
k59_valued_1 k54_valued_1
k60_valued_1 k54_valued_1
v1_valued_1 v5_valued_0
v2_valued_1 v6_valued_0
v3_valued_1 v7_valued_0
v4_valued_1 v8_valued_0
k1_aofa_000 k2_funcop_1
k3_aofa_000 k2_aofa_000
k4_aofa_000 k3_relat_1
k8_aofa_000 k7_aofa_000
k11_aofa_000 k10_aofa_000
