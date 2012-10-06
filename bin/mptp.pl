#!/usr/bin/perl

use strict;
use warnings;
use Regexp::DefaultFlags;

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
	} else {
	    if ($kind =~ /[rv]constructor/) {
		$answer = 0;
	    } elsif ($kind =~ /[gjklmruv]pattern/) {
		$answer = 0;
	    } elsif ($kind eq 'lemma') {
		$answer = 0;
	    } else {
		$answer = 1;
	    }
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

foreach my $line (<STDIN>) {
    chomp $line;
    (my $item, my @deps) = split (' ', $line);
    push (@items, $item);
    $dependency_table{$item} = \@deps;
}

my %printed = ();

foreach my $item (@items) {
    if (handled ($item)) {
	my $mptp = to_mptp ($item);
	my %printed_for_this_item = (
	    'symmetry_r1_hidden' => 0,
	    'reflexivity_r1_hidden' => 0,
	);
	if (defined $printed{$mptp}) {
	    # ignore
	} else {

	    if (is_redefined_constructor ($item)) {
		print redefine_constructor ($item);
	    } else {
		print $mptp;
	    }

	    my @deps = @{$dependency_table{$item}};

	    foreach my $dep_item (@deps) {
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
		    }
		    $printed_for_this_item{$dep_mptp} = 0;
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
			warn 'To ', $item, ' neither a compatibility nor a coherence item is associated.';
		    }
		} else {
		    die 'Error: cannot make sense of \'', $item, '\'.';
		}

		# todo: if the original constructor had any
		# properties, add them here

	    }

	    foreach my $dep_item (@deps) {
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
	    $printed{$mptp} = 0;

	    # Typing for redefined functors
	    if (is_redefined_constructor ($item)) {
		if ($item =~ / [:] . constructor [:] /) {
		    my $mptp = to_mptp ($item);
		    my $redefinition = redefine_constructor ($item);
		    print $mptp, ' ', $redefinition, "\n";
		    $printed{$mptp} = 0;
		}
	    }

	    # Mode existence conditions
	    %printed_for_this_item = ();
	    if ($item =~ / \A ([a-z0-9_]+) [:] ([lm]) constructor [:] ([0-9]+) \z/) {
		(my $article, my $kind, my $number) = ($1, $2, $3);
		my $mptp_existence = "existence_${kind}${number}_${article}";

		print $mptp_existence;

		foreach my $dep_item (@deps) {
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
		$printed{$mptp_existence} = 0;
	    }
	}
    }
}

# Redefined constructors, accoding to the MPTP table for MML 4.181.1147
#
# Available at
# http://mizar.cs.ualberta.ca/~mptp/7.13.01_4.181.1147/MPTP2/00allmmlax

__DATA__
k10_aofa_i00
k10_binarith
k10_binop_2
k10_bspace
k10_bvfunc_1
k10_cat_2
k10_catalg_1
k10_classes2
k10_clopban1
k10_complex1
k10_comseq_3
k10_cqc_lang
k10_descip_1
k10_domain_1
k10_dtconstr
k10_euclid
k10_filter_0
k10_finseq_4
k10_fomodel3
k10_funcop_1
k10_funct_2
k10_funct_3
k10_funct_5
k10_fvsum_1
k10_genealg1
k10_glib_000
k10_group_2
k10_lang1
k10_laplace
k10_margrel1
k10_matrixj1
k10_mcart_1
k10_measure8
k10_mesfunc9
k10_monoid_1
k10_nat_1
k10_pcs_0
k10_petri
k10_polyeq_1
k10_polynom1
k10_pralg_1
k10_pre_poly
k10_prepower
k10_prvect_1
k10_pua2mss1
k10_quofield
k10_real_1
k10_rfunct_3
k10_rvsum_1
k10_rvsum_2
k10_seq_4
k10_sincos10
k10_topalg_4
k10_toprealc
k10_trees_3
k10_trees_9
k10_valued_1
k10_yellow_3
k11_ami_3
k11_aofa_000
k11_aofa_i00
k11_arytm_3
k11_binop_2
k11_card_3
k11_cat_2
k11_chain_1
k11_classes2
k11_cohsp_1
k11_complex1
k11_cqc_lang
k11_cqc_sim1
k11_descip_1
k11_domain_1
k11_ec_pf_2
k11_euclid
k11_facirc_1
k11_filter_2
k11_funcop_1
k11_funct_5
k11_genealg1
k11_glib_000
k11_group_9
k11_lang1
k11_matrix_2
k11_matrixj1
k11_measure8
k11_monoid_1
k11_nat_3
k11_normform
k11_openlatt
k11_pcs_0
k11_pdiff_1
k11_pralg_1
k11_pre_poly
k11_prvect_1
k11_prvect_2
k11_quaterni
k11_rfunct_3
k11_ringcat1
k11_rvsum_1
k11_rvsum_2
k11_scm_comp
k11_seq_4
k11_sincos10
k11_toprealc
k11_trees_9
k11_valued_1
k11_yellow_3
k12_abcmiz_a
k12_ami_3
k12_aofa_i00
k12_arytm_3
k12_binop_2
k12_cgames_1
k12_chain_1
k12_classes2
k12_complex1
k12_cqc_lang
k12_descip_1
k12_diff_1
k12_domain_1
k12_finseq_1
k12_fomodel2
k12_fomodel3
k12_freealg
k12_funcop_1
k12_funct_3
k12_funct_5
k12_funct_7
k12_functor0
k12_fvsum_1
k12_genealg1
k12_graphsp
k12_grcat_1
k12_ideal_1
k12_lattice3
k12_lattice5
k12_lopban_1
k12_margrel1
k12_measure7
k12_monoid_1
k12_openlatt
k12_polynom3
k12_prvect_1
k12_pua2mss1
k12_quaterni
k12_rfunct_3
k12_ringcat1
k12_rvsum_1
k12_rvsum_2
k12_seq_4
k12_sincos10
k12_sublemma
k12_supinf_2
k12_toprealc
k12_valued_1
k12_yellow_3
k13_aofa_i00
k13_binop_2
k13_cat_5
k13_chain_1
k13_complex1
k13_descip_1
k13_domain_1
k13_fomodel1
k13_fomodel3
k13_funct_5
k13_ideal_1
k13_lang1
k13_lattice3
k13_lexbfs
k13_lopclset
k13_matrixj1
k13_newton
k13_ordinal6
k13_osalg_2
k13_polynom3
k13_prob_1
k13_pua2mss1
k13_rvsum_1
k13_seq_4
k13_toprealc
k13_yellow_3
k14_abcmiz_a
k14_afinsq_1
k14_aofa_i00
k14_binop_2
k14_borsuk_1
k14_borsuk_7
k14_cat_5
k14_chain_1
k14_descip_1
k14_domain_1
k14_fomodel2
k14_funct_3
k14_funct_7
k14_grcat_1
k14_lexbfs
k14_matrixj1
k14_monoid_1
k14_pralg_1
k14_prvect_1
k14_rvsum_1
k14_sublemma
k14_toprealc
k14_valued_1
k15_afinsq_1
k15_aofa_i00
k15_binop_2
k15_borsuk_1
k15_bvfunc_1
k15_cat_5
k15_clopban1
k15_complex1
k15_csspace
k15_descip_1
k15_domain_1
k15_fomodel1
k15_funcop_1
k15_funct_7
k15_glib_000
k15_glib_004
k15_ideal_1
k15_lattice5
k15_margrel1
k15_matrixj1
k15_mesfunc1
k15_mmlquery
k15_monoid_1
k15_pcs_0
k15_pre_poly
k15_rvsum_1
k15_seq_4
k15_sin_cos
k15_valued_1
k16_aofa_i00
k16_binop_2
k16_borsuk_1
k16_cat_2
k16_cat_5
k16_csspace
k16_descip_1
k16_domain_1
k16_euclid
k16_fomodel1
k16_fomodel2
k16_fomodel3
k16_funct_3
k16_margrel1
k16_matrixj1
k16_mesfunc1
k16_mmlquery
k16_monoid_1
k16_openlatt
k16_pcs_0
k16_pralg_1
k16_prvect_1
k16_valued_1
k16_valued_2
k17_binop_2
k17_cat_2
k17_cat_5
k17_coh_sp
k17_complex1
k17_domain_1
k17_finseq_1
k17_fomodel1
k17_freealg
k17_funcop_1
k17_glib_000
k17_lexbfs
k17_lopban_1
k17_margrel1
k17_matrixj1
k17_mesfunc1
k17_mmlquery
k17_monoid_1
k17_quatern2
k17_quaterni
k17_rvsum_1
k17_seq_4
k17_supinf_2
k17_valued_1
k17_valued_2
k18_aofa_i00
k18_binop_2
k18_cat_5
k18_coh_sp
k18_complex1
k18_fomodel0
k18_fomodel1
k18_funcop_1
k18_matrixj1
k18_mesfunc1
k18_openlatt
k18_quaterni
k18_rvsum_1
k18_scmfsa_2
k18_seq_4
k18_sin_cos
k18_substut1
k18_toprealc
k18_valued_2
k19_aofa_i00
k19_binop_2
k19_bvfunc_1
k19_euclid
k19_fomodel0
k19_fomodel3
k19_freealg
k19_grcat_1
k19_margrel1
k19_matrixj1
k19_mesfunc1
k19_mmlquery
k19_pre_poly
k19_quaterni
k19_seq_4
k19_substut1
k19_valued_1
k19_valued_2
k1_abian
k1_afinsq_1
k1_altcat_2
k1_amistd_4
k1_aofa_000
k1_arrow
k1_asympt_0
k1_bcialg_6
k1_binarith
k1_binop_2
k1_bor_cant
k1_borsuk_1
k1_bvfunc_1
k1_bvfunc_2
k1_card_fil
k1_cat_3
k1_cat_5
k1_catalan2
k1_chain_1
k1_circcomb
k1_circuit2
k1_classes2
k1_clopban1
k1_clopban2
k1_closure1
k1_closure3
k1_convex4
k1_descip_1
k1_dist_1
k1_domain_1
k1_dtconstr
k1_dynkin
k1_ec_pf_1
k1_euclid_7
k1_euclid_8
k1_euler_2
k1_extreal1
k1_extreal2
k1_filter_1
k1_finance1
k1_finseq_7
k1_finseq_8
k1_finseqop
k1_finsub_1
k1_fintopo5
k1_flang_1
k1_fomodel2
k1_funcsdom
k1_functor0
k1_functor3
k1_fuzzy_4
k1_glib_003
k1_goboard9
k1_gobrd13
k1_gr_cy_1
k1_graph_3
k1_graphsp
k1_grnilp_1
k1_groeb_1
k1_groeb_3
k1_group_10
k1_group_4
k1_group_7
k1_index_1
k1_int_2
k1_integr16
k1_integra2
k1_integra5
k1_isocat_2
k1_jordan
k1_jordan2b
k1_kurato_0
k1_laplace
k1_lfuzzy_1
k1_lmod_7
k1_lopban_1
k1_lopban_2
k1_margrel1
k1_matrix15
k1_matrix_6
k1_matrix_7
k1_matrixr2
k1_matrlin
k1_matrlin2
k1_matrprob
k1_mcart_1
k1_measure1
k1_measure2
k1_measure3
k1_measure5
k1_measure8
k1_member_1
k1_mesfunc9
k1_metric_1
k1_midsp_3
k1_mmlquery
k1_mod_4
k1_modelc_3
k1_monoid_0
k1_mssubfam
k1_msualg_3
k1_msualg_4
k1_msualg_6
k1_nagata_1
k1_nat_1
k1_nat_5
k1_nat_d
k1_nattra_1
k1_ndiff_5
k1_normsp_1
k1_oppcat_1
k1_partfun1
k1_partfun2
k1_pcs_0
k1_pdiff_6
k1_pencil_1
k1_pepin
k1_polyeq_3
k1_polyform
k1_polynom2
k1_polynom3
k1_pralg_1
k1_pre_poly
k1_prob_1
k1_prob_2
k1_prob_4
k1_prvect_1
k1_prvect_2
k1_pua2mss1
k1_quatern2
k1_random_1
k1_rcomp_1
k1_real_1
k1_rearran1
k1_recdef_1
k1_relset_1
k1_relset_2
k1_roughs_1
k1_rvsum_1
k1_rvsum_2
k1_scm_1
k1_scm_inst
k1_scmfsa10
k1_scmfsa_3
k1_scmpds_3
k1_scmpds_4
k1_scmpds_9
k1_scmring3
k1_scmring4
k1_seq_1
k1_seq_4
k1_seqfunc
k1_series_1
k1_square_1
k1_stacks_1
k1_stirl2_1
k1_sublemma
k1_substut2
k1_supinf_1
k1_supinf_2
k1_toler_1
k1_topalg_3
k1_topgen_3
k1_toprealc
k1_transgeo
k1_turing_1
k1_twoscomp
k1_unialg_3
k1_urysohn3
k1_valued_0
k1_waybel10
k1_waybel18
k1_waybel20
k1_waybel23
k1_waybel27
k1_waybel_1
k1_waybel_5
k1_waybel_6
k1_wsierp_1
k1_yellow12
k1_yellow14
k1_yellow16
k1_yellow_1
k1_zf_fund1
k20_aofa_i00
k20_binop_2
k20_bvfunc_1
k20_glib_001
k20_grcat_1
k20_matrixj1
k20_mmlquery
k20_quaterni
k20_rvsum_1
k20_seq_4
k20_valued_1
k21_abcmiz_1
k21_aofa_i00
k21_binop_2
k21_bvfunc_1
k21_euclid
k21_fomodel3
k21_mmlquery
k21_rvsum_1
k21_seq_4
k21_sin_cos
k21_valued_1
k22_abcmiz_1
k22_binop_2
k22_matrixj1
k22_mmlquery
k22_substut1
k22_valued_1
k22_valued_2
k23_abcmiz_1
k23_binop_2
k23_fomodel3
k23_lexbfs
k23_monoid_1
k23_rvsum_1
k23_substut1
k23_valued_1
k23_valued_2
k24_abcmiz_1
k24_binop_2
k24_fomodel1
k24_fomodel3
k24_glib_000
k24_lexbfs
k24_turing_1
k24_valued_2
k25_cat_4
k25_fomodel3
k25_glib_000
k25_substut1
k25_turing_1
k25_valued_1
k26_cat_4
k26_fomodel1
k26_quaterni
k26_sin_cos
k26_valued_1
k26_valued_2
k27_cat_4
k27_fomodel3
k27_pcs_0
k27_quaterni
k27_valued_1
k27_valued_2
k28_aofa_i00
k28_cat_4
k28_fomodel1
k28_quaterni
k28_valued_1
k28_valued_2
k29_aofa_i00
k29_fomodel1
k29_fomodel3
k29_modelc_1
k29_quaterni
k29_valued_1
k29_valued_2
k2_absvalue
k2_aff_1
k2_afinsq_1
k2_algstr_4
k2_ami_2
k2_asympt_0
k2_bhsp_1
k2_binarith
k2_binop_1
k2_binop_2
k2_bintree1
k2_borsuk_7
k2_bvfunc_1
k2_calcul_2
k2_cat_5
k2_chain_1
k2_classes2
k2_closure1
k2_closure2
k2_complfld
k2_complsp2
k2_compos_0
k2_comseq_2
k2_comseq_3
k2_cqc_lang
k2_descip_1
k2_diff_1
k2_domain_1
k2_dtconstr
k2_ec_pf_2
k2_eqrel_1
k2_euclid_4
k2_extreal1
k2_extreal2
k2_fib_num3
k2_fin_topo
k2_finseq_1
k2_finseq_4
k2_finseq_7
k2_finseqop
k2_finsop_1
k2_finsub_1
k2_flang_1
k2_fomodel0
k2_funcsdom
k2_funct_2
k2_funct_3
k2_functor0
k2_functor3
k2_fuzzy_4
k2_graph_2
k2_group_6
k2_groupp_1
k2_henmodel
k2_heyting1
k2_index_1
k2_int_3
k2_integr15
k2_integr16
k2_integr18
k2_integra2
k2_interva1
k2_jordan
k2_jordan1h
k2_jordan1k
k2_kolmog01
k2_kurato_0
k2_lattice3
k2_lexbfs
k2_lfuzzy_0
k2_lfuzzy_1
k2_lopclset
k2_lpspace1
k2_matrix10
k2_matrix13
k2_matrix14
k2_matrix_2
k2_matrix_6
k2_matrixj1
k2_matrixj2
k2_matrixr2
k2_matrlin
k2_matrprob
k2_mcart_1
k2_measure1
k2_measure2
k2_measure4
k2_measure6
k2_measure7
k2_member_1
k2_midsp_3
k2_mmlquery
k2_monoid_1
k2_morph_01
k2_msaterm
k2_mssubfam
k2_msualg_4
k2_msualg_6
k2_multop_1
k2_nagata_1
k2_nat_1
k2_nat_3
k2_nat_d
k2_nattra_1
k2_newton
k2_openlatt
k2_partfun1
k2_partfun2
k2_partfun3
k2_partit_2
k2_pcs_0
k2_pencil_1
k2_pencil_2
k2_petri
k2_pnproc_1
k2_polyeq_1
k2_polyeq_3
k2_polyform
k2_polynom2
k2_polynom3
k2_power
k2_pralg_1
k2_pralg_2
k2_pre_ff
k2_pre_poly
k2_quatern2
k2_quatern3
k2_quin_1
k2_ranknull
k2_rcomp_1
k2_real_1
k2_realset1
k2_relset_1
k2_relset_2
k2_rfinseq
k2_rfunct_1
k2_robbins3
k2_roughs_1
k2_scm_comp
k2_scmfsa_1
k2_scmfsa_2
k2_scmpds_i
k2_scmring2
k2_scpinvar
k2_seq_2
k2_setlim_1
k2_setwiseo
k2_setwop_2
k2_simplex0
k2_simplex2
k2_square_1
k2_stacks_1
k2_stirl2_1
k2_sublemma
k2_substlat
k2_substut2
k2_supinf_1
k2_supinf_2
k2_tbsp_1
k2_toler_1
k2_topalg_2
k2_topalg_3
k2_topalg_4
k2_topalg_5
k2_topgen_4
k2_topreal7
k2_toprealb
k2_trees_4
k2_trees_a
k2_turing_1
k2_valuat_1
k2_valued_0
k2_valued_1
k2_vectmetr
k2_vectsp_8
k2_waybel26
k2_waybel28
k2_waybel_5
k2_waybel_6
k2_wsierp_1
k2_yellow14
k2_yellow16
k2_yellow_3
k2_yellow_4
k2_yellow_6
k2_yellow_7
k2_zf_lang1
k2_zf_model
k2_zf_refle
k2_zfrefle1
k30_aofa_i00
k30_fomodel2
k30_fomodel3
k31_aofa_i00
k31_fomodel2
k31_quaterni
k31_valued_1
k31_valued_2
k32_abcmiz_1
k32_aofa_i00
k32_fomodel1
k32_glib_000
k32_sin_cos
k32_valued_1
k32_valued_2
k33_abcmiz_1
k33_aofa_i00
k33_fomodel2
k33_glib_000
k33_substut1
k33_valued_1
k33_valued_2
k34_aofa_i00
k34_fomodel2
k34_substut1
k34_valued_1
k34_valued_2
k35_aofa_i00
k35_fomodel1
k35_fomodel2
k35_fomodel3
k35_glib_000
k35_valued_2
k36_aofa_i00
k36_fomodel2
k36_fomodel3
k36_fomodel4
k36_valued_1
k37_aofa_i00
k37_fomodel1
k37_fomodel2
k37_valued_1
k37_valued_2
k38_aofa_i00
k38_valued_1
k38_valued_2
k39_aofa_i00
k39_fomodel1
k39_fomodel4
k39_substut1
k39_valued_2
k3_abcmiz_1
k3_ami_wstd
k3_aofa_000
k3_asympt_0
k3_binop_1
k3_binop_2
k3_boolealg
k3_borsuk_1
k3_borsuk_2
k3_borsuk_7
k3_bvfunc26
k3_cat_2
k3_catalan2
k3_catalg_1
k3_cgames_1
k3_chord
k3_classes1
k3_classes2
k3_closure1
k3_closure3
k3_cohsp_1
k3_commacat
k3_complex1
k3_complsp2
k3_compos_1
k3_descip_1
k3_domain_1
k3_dtconstr
k3_ec_pf_2
k3_eqrel_1
k3_euclid
k3_finance1
k3_finseq_1
k3_finseq_2
k3_finseq_4
k3_finseq_6
k3_finseq_8
k3_finseqop
k3_finsop_1
k3_finsub_1
k3_flang_1
k3_fomodel2
k3_funcsdom
k3_funct_2
k3_funct_7
k3_functor3
k3_graph_1
k3_graphsp
k3_group_2
k3_group_6
k3_idea_1
k3_int_5
k3_jgraph_1
k3_jordan10
k3_jordan1k
k3_kolmog01
k3_lattice5
k3_lattices
k3_lfuzzy_0
k3_lfuzzy_1
k3_lopclset
k3_matrix10
k3_matrix13
k3_matrix_6
k3_matrix_7
k3_matrixj1
k3_matrixj2
k3_mcart_1
k3_measure1
k3_measure8
k3_member_1
k3_mesfunc2
k3_metric_1
k3_midsp_1
k3_midsp_3
k3_modcat_1
k3_monoid_0
k3_monoid_1
k3_msafree
k3_msaterm
k3_msualg_3
k3_msualg_8
k3_nagata_1
k3_nat_1
k3_nat_3
k3_nat_d
k3_normsp_0
k3_openlatt
k3_partfun2
k3_partfun3
k3_pencil_2
k3_petri
k3_polyform
k3_polynom1
k3_polynom3
k3_pre_poly
k3_prepower
k3_prvect_2
k3_pscomp_1
k3_pua2mss1
k3_quatern2
k3_quaterni
k3_radix_1
k3_radix_3
k3_random_1
k3_ratfunc1
k3_rcomp_1
k3_real_1
k3_relset_1
k3_rewrite2
k3_rfunct_1
k3_rfunct_3
k3_rlvect_1
k3_rlvect_2
k3_rvsum_2
k3_scm_comp
k3_scmfsa_1
k3_scmpds_1
k3_series_1
k3_setwiseo
k3_sin_cos2
k3_sin_cos6
k3_substut2
k3_supinf_2
k3_topalg_4
k3_topgen_2
k3_topgen_4
k3_topgrp_1
k3_topreal7
k3_toprealb
k3_toprealc
k3_trees_2
k3_trees_3
k3_turing_1
k3_valued_1
k3_vfunct_2
k3_waybel27
k3_waybel_3
k3_waybel_5
k3_wsierp_1
k3_yellow20
k3_yellow21
k3_yellow_5
k3_zf_lang
k3_zf_refle
k40_abcmiz_1
k40_fomodel4
k40_valued_1
k40_valued_2
k41_abcmiz_1
k41_fomodel4
k41_valued_1
k42_abcmiz_1
k42_fomodel2
k42_valued_1
k42_valued_2
k43_fomodel4
k43_valued_1
k43_valued_2
k44_valued_1
k44_valued_2
k45_valued_2
k46_valued_1
k46_valued_2
k47_fomodel4
k47_valued_1
k48_valued_1
k48_valued_2
k49_valued_1
k49_valued_2
k4_altcat_1
k4_aofa_000
k4_asympt_0
k4_binop_2
k4_borsuk_1
k4_borsuk_7
k4_bvfunc26
k4_bvfunc_2
k4_card_1
k4_cat_2
k4_chord
k4_circcmb3
k4_classes2
k4_clopban1
k4_closure3
k4_cohsp_1
k4_commacat
k4_complex1
k4_complsp2
k4_cqc_lang
k4_descip_1
k4_dickson
k4_domain_1
k4_eqrel_1
k4_euclid_4
k4_euclid_5
k4_finseq_1
k4_finseq_5
k4_finseqop
k4_finsub_1
k4_flang_1
k4_fomodel2
k4_fomodel3
k4_funcsdom
k4_funct_2
k4_functor2
k4_functor3
k4_fvsum_1
k4_glib_005
k4_graph_1
k4_graph_2
k4_graphsp
k4_group_7
k4_ideal_1
k4_int_1
k4_integr16
k4_jgraph_1
k4_jordan10
k4_jordan1k
k4_lang1
k4_lattices
k4_matrix10
k4_matrix11
k4_matrix13
k4_matrix_2
k4_matrix_6
k4_matrixj1
k4_matrixj2
k4_mcart_1
k4_measure1
k4_measure6
k4_measure7
k4_mesfunc5
k4_mesfunc7
k4_metric_1
k4_monoid_1
k4_mssubfam
k4_multop_1
k4_nat_1
k4_nat_d
k4_normsp_0
k4_ordinal4
k4_ordinal6
k4_partfun2
k4_partfun3
k4_pcomps_2
k4_petri
k4_pnproc_1
k4_polyeq_1
k4_polyform
k4_polynom3
k4_power
k4_pre_poly
k4_prelamb
k4_prob_3
k4_pua2mss1
k4_quatern2
k4_quatern3
k4_quofield
k4_radix_3
k4_random_1
k4_ratfunc1
k4_rcomp_1
k4_real_1
k4_relset_1
k4_relset_2
k4_rewrite2
k4_rfunct_3
k4_robbins3
k4_rvsum_1
k4_rvsum_2
k4_scmfsa_m
k4_seq_4
k4_setlim_1
k4_setwiseo
k4_square_1
k4_stacks_1
k4_sublemma
k4_subset_1
k4_supinf_2
k4_tex_4
k4_topalg_4
k4_topgen_2
k4_topgrp_1
k4_topreal7
k4_toprealc
k4_trees_9
k4_turing_1
k4_uproots
k4_urysohn1
k4_urysohn3
k4_valuat_1
k4_valued_1
k4_waybel18
k4_waybel35
k4_waybel_3
k4_wsierp_1
k4_yellow20
k4_yellow21
k4_yellow_1
k4_yellow_3
k4_yellow_4
k4_zf_lang1
k4_zf_refle
k50_valued_2
k51_abcmiz_1
k51_valued_1
k52_abcmiz_1
k52_valued_1
k52_valued_2
k53_abcmiz_1
k53_valued_1
k53_valued_2
k54_valued_2
k55_valued_1
k55_valued_2
k56_valued_1
k56_valued_2
k57_valued_1
k58_abcmiz_1
k58_valued_1
k58_valued_2
k59_abcmiz_1
k59_valued_1
k59_valued_2
k5_abcmiz_1
k5_abcmiz_a
k5_afinsq_1
k5_binop_1
k5_binop_2
k5_borsuk_1
k5_borsuk_7
k5_bvfunc26
k5_bvfunc_1
k5_card_1
k5_cat_5
k5_circcmb3
k5_classes2
k5_closure2
k5_complex1
k5_complsp2
k5_comseq_3
k5_convex4
k5_cqc_lang
k5_cqc_sim1
k5_descip_1
k5_domain_1
k5_equation
k5_euclid
k5_exchsort
k5_finseq_2
k5_flang_1
k5_funct_2
k5_funct_3
k5_funct_4
k5_funct_5
k5_functor0
k5_glib_002
k5_glib_004
k5_groeb_2
k5_group_7
k5_ideal_1
k5_index_1
k5_integr15
k5_knaster
k5_lang1
k5_lexbfs
k5_matrix10
k5_matrix13
k5_matrix_1
k5_matrix_2
k5_matrix_3
k5_matrixj1
k5_matrixj2
k5_matrlin2
k5_mcart_1
k5_mesfunc9
k5_midsp_1
k5_midsp_3
k5_modal_1
k5_monoid_1
k5_mssubfam
k5_msualg_4
k5_nat_d
k5_newton
k5_numbers
k5_ordinal4
k5_ordinal6
k5_partfun3
k5_petri
k5_pnproc_1
k5_polyform
k5_polynom1
k5_pralg_2
k5_prepower
k5_prob_1
k5_prob_3
k5_prvect_2
k5_quofield
k5_random_1
k5_ranknull
k5_ratfunc1
k5_real_1
k5_real_ns1
k5_relset_1
k5_rewrite2
k5_rfunct_1
k5_rfunct_3
k5_ringcat1
k5_robbins1
k5_robbins3
k5_rvsum_1
k5_rvsum_2
k5_scmfsa_m
k5_scmpds_4
k5_scmring1
k5_seq_4
k5_series_1
k5_setfam_1
k5_setlim_1
k5_setwiseo
k5_sin_cos9
k5_square_1
k5_sublemma
k5_subset_1
k5_substut2
k5_supinf_2
k5_topalg_4
k5_topmetr
k5_toprealc
k5_trees_9
k5_valued_1
k5_waybel18
k5_waybel_3
k5_waybel_9
k5_wsierp_1
k5_yellow20
k5_yellow_3
k5_zf_model
k5_zf_refle
k60_abcmiz_1
k60_valued_1
k60_valued_2
k61_abcmiz_1
k61_valued_2
k62_abcmiz_1
k63_valued_2
k64_valued_2
k65_valued_2
k66_valued_2
k67_valued_2
k69_valued_2
k6_abcmiz_a
k6_armstrng
k6_bcialg_1
k6_bhsp_1
k6_bilinear
k6_binop_2
k6_borsuk_1
k6_borsuk_7
k6_bvfunc26
k6_bvfunc_1
k6_calcul_2
k6_cat_2
k6_cat_5
k6_circcmb3
k6_classes2
k6_closure2
k6_cohsp_1
k6_comseq_3
k6_convex4
k6_cqc_lang
k6_domain_1
k6_dtconstr
k6_eqrel_1
k6_euclid
k6_euclid_7
k6_filter_1
k6_fomodel3
k6_fomodel4
k6_funcop_1
k6_funct_3
k6_fvsum_1
k6_graphsp
k6_group_7
k6_ideal_1
k6_index_1
k6_int_3
k6_knaster
k6_lattice3
k6_lattice8
k6_lopban_1
k6_matrix15
k6_matrix_2
k6_matrixj1
k6_matrixj2
k6_matrlin2
k6_mcart_1
k6_midsp_3
k6_monoid_1
k6_msaterm
k6_nat_1
k6_nat_d
k6_nattra_1
k6_newton
k6_numbers
k6_ordinal4
k6_partfun1
k6_pcs_0
k6_pdiff_1
k6_polyeq_1
k6_polyform
k6_power
k6_pre_poly
k6_prelamb
k6_prob_1
k6_qmax_1
k6_quatern2
k6_random_1
k6_real_1
k6_relset_1
k6_relset_2
k6_rewrite2
k6_rfunct_1
k6_rfunct_3
k6_robbins1
k6_rvsum_1
k6_rvsum_2
k6_scmfsa_m
k6_scmring1
k6_seq_4
k6_setfam_1
k6_setlim_1
k6_setwiseo
k6_sin_cos2
k6_sin_cos6
k6_sin_cos9
k6_subset_1
k6_substut1
k6_supinf_2
k6_topalg_4
k6_topalg_5
k6_topgrp_1
k6_toprealc
k6_trees_2
k6_trees_3
k6_valuat_1
k6_valued_1
k6_vfunct_1
k6_waybel35
k6_waybel_5
k6_yellow_3
k6_zf_lang1
k70_valued_2
k71_valued_2
k73_valued_2
k74_valued_2
k75_valued_2
k76_valued_2
k77_valued_2
k79_valued_2
k7_abcmiz_a
k7_aofa_i00
k7_binom
k7_binop_2
k7_borsuk_7
k7_card_1
k7_cat_2
k7_cat_4
k7_chain_1
k7_circcmb3
k7_classes2
k7_clopban1
k7_closure2
k7_complex1
k7_comseq_3
k7_cqc_lang
k7_domain_1
k7_dtconstr
k7_dynkin
k7_euclid
k7_finseqop
k7_fomodel3
k7_fomodel4
k7_funcop_1
k7_funct_4
k7_functor0
k7_genealg1
k7_group_1
k7_index_1
k7_interva1
k7_lexbfs
k7_lfuzzy_0
k7_margrel1
k7_matrixj1
k7_matrixj2
k7_matrlin
k7_matrlin2
k7_mcart_1
k7_measure6
k7_mod_2
k7_modal_1
k7_monoid_1
k7_nat_1
k7_nat_d
k7_ordinal4
k7_prepower
k7_prob_1
k7_prvect_1
k7_qmax_1
k7_random_1
k7_ranknull
k7_real_1
k7_relset_1
k7_relset_2
k7_rfunct_1
k7_rfunct_3
k7_rlvect_2
k7_rusub_4
k7_rvsum_1
k7_rvsum_2
k7_scmfsa_m
k7_scmringi
k7_square_1
k7_sublemma
k7_subset_1
k7_supinf_2
k7_topalg_4
k7_topgrp_1
k7_toprealc
k7_trees_3
k7_trees_4
k7_valuat_1
k7_waybel_5
k7_yellow_3
k7_yellow_6
k80_valued_2
k81_valued_2
k82_valued_2
k84_valued_2
k85_valued_2
k86_valued_2
k87_valued_2
k88_valued_2
k89_aofa_i00
k8_aofa_000
k8_aofa_i00
k8_binop_2
k8_cat_4
k8_cat_5
k8_catalg_1
k8_classes2
k8_closure2
k8_cohsp_1
k8_complex1
k8_compos_1
k8_comput_1
k8_comseq_3
k8_conlat_1
k8_cqc_lang
k8_cqc_sim1
k8_domain_1
k8_dtconstr
k8_eqrel_1
k8_euclid
k8_finseq_1
k8_fomodel0
k8_funcop_1
k8_funct_5
k8_funct_7
k8_fvsum_1
k8_genealg1
k8_graph_5
k8_group_1
k8_index_1
k8_interva1
k8_isocat_2
k8_lexbfs
k8_lfuzzy_0
k8_lpspace1
k8_margrel1
k8_matrix_1
k8_matrix_2
k8_matrix_3
k8_matrixc1
k8_matrixj2
k8_matrlin
k8_matrlin2
k8_mcart_1
k8_msaterm
k8_nat_1
k8_nat_3
k8_ordinal3
k8_ordinal4
k8_polyeq_1
k8_polynom3
k8_power
k8_prelamb
k8_prob_1
k8_prob_3
k8_prvect_1
k8_quatern2
k8_real_1
k8_relset_1
k8_rfunct_3
k8_rvsum_1
k8_rvsum_2
k8_setwiseo
k8_sublemma
k8_subset_1
k8_supinf_2
k8_topalg_4
k8_topgrp_1
k8_toprealc
k8_trees_4
k8_valued_1
k8_yellow_3
k90_valued_2
k91_valued_2
k92_valued_2
k9_aofa_i00
k9_binarith
k9_binop_2
k9_bspace
k9_bvfunc_1
k9_cat_1
k9_cat_2
k9_catalg_1
k9_classes2
k9_complex1
k9_compos_1
k9_comseq_3
k9_cqc_lang
k9_descip_1
k9_domain_1
k9_dtconstr
k9_ec_pf_2
k9_eqrel_1
k9_euclid
k9_filter_0
k9_filter_1
k9_finseq_1
k9_finseq_4
k9_fomodel0
k9_freealg
k9_funcop_1
k9_funct_2
k9_funct_3
k9_funct_5
k9_functor3
k9_genealg1
k9_glib_003
k9_glib_004
k9_lattice8
k9_lfuzzy_0
k9_lopban_1
k9_lopclset
k9_margrel1
k9_matrix13
k9_matrix_1
k9_matrix_3
k9_matrix_5
k9_mcart_1
k9_measure8
k9_newton
k9_ordinal3
k9_osafree
k9_pcs_0
k9_pre_poly
k9_prob_3
k9_prvect_1
k9_real_1
k9_rfunct_3
k9_rvsum_1
k9_rvsum_2
k9_scmfsa_1
k9_seq_4
k9_setfam_1
k9_simplex0
k9_sin_cos2
k9_sincos10
k9_subset_1
k9_supinf_2
k9_topalg_4
k9_toprealc
k9_valued_1
k9_yellow_3
m10_glib_001
m1_armstrng
m1_autalg_1
m1_bintree2
m1_bvfunc_1
m1_bvfunc_2
m1_card_lar
m1_cat_5
m1_closure2
m1_complfld
m1_comput_1
m1_dtconstr
m1_glib_004
m1_graph_3
m1_graph_5
m1_group_6
m1_incproj
m1_matrix_2
m1_nat_lat
m1_orders_3
m1_osalg_1
m1_partit_2
m1_petri
m1_pre_poly
m1_prob_1
m1_prob_3
m1_pua2mss1
m1_trees_1
m1_trees_4
m1_trees_9
m2_cat_2
m2_conlat_1
m2_finseq_1
m2_finseq_2
m2_fomodel2
m2_funct_2
m2_graph_1
m2_grcat_1
m2_lmod_7
m2_modcat_1
m2_prob_4
m2_rfunct_3
m2_ringcat1
m2_rlaffin3
m2_subset_1
m2_trees_3
m2_unialg_3
m2_valued_0
m2_vectsp_8
m3_conlat_1
m3_grcat_1
m3_msalimit
m3_pnproc_1
m3_ringcat1
m3_topgrp_1
m3_trees_3
m4_modcat_1
m4_monoid_0
m5_bcialg_2
m5_glib_001
m5_lmod_7
m5_margrel1
m5_monoid_0
m5_ringcat1
m6_glib_001
m6_grcat_1
m6_modcat_1
m6_monoid_0
m7_glib_001
m7_grcat_1
m7_monoid_0
m8_glib_001
m8_lmod_7
m9_glib_001
r10_analmetr
r1_boolealg
r1_borsuk_3
r1_borsuk_6
r1_convex4
r1_exchsort
r1_filter_2
r1_funct_2
r1_group_2
r1_heyting1
r1_interva1
r1_lattice7
r1_lfuzzy_1
r1_margrel1
r1_mfold_2
r1_nat_d
r1_ordinal1
r1_pnproc_1
r1_polynom6
r1_relset_1
r1_revrot_1
r1_rlvect_2
r1_rmod_4
r1_subset_1
r1_vectsp_6
r1_yellow10
r2_abian
r2_altcat_2
r2_bhsp_3
r2_borsuk_2
r2_borsuk_3
r2_clvect_2
r2_compos_2
r2_euclidlp
r2_funct_2
r2_fuzzy_1
r2_gcd_1
r2_group_3
r2_group_6
r2_interva1
r2_metric_2
r2_necklace
r2_osalg_3
r2_pcs_0
r2_relset_1
r2_robbins4
r2_subset_1
r2_topalg_6
r2_tsep_2
r2_wellord2
r2_yellow_5
r3_boolealg
r3_finseq_8
r3_group_9
r3_lattices
r3_metric_2
r3_orders_2
r3_osalg_3
r3_pboole
r3_yellow_4
r4_aff_1
r4_boolealg
r4_borsuk_2
r4_gcd_1
r4_group_3
r4_tsep_2
r4_yellow_4
r5_aff_1
r5_pua2mss1
r6_group_3
r6_msualg_3
r6_pboole
r7_gcd_1
r8_binop_1
r8_pboole
r9_analmetr
v18_pcs_0
v19_pcs_0
v1_dynkin
v1_goboard1
v1_knaster
v1_kurato_0
v1_matrix_7
v1_measure1
v1_mesfunc2
v1_partfun2
v1_pboole
v1_prob_3
v1_revrot_1
v1_valued_1
v1_vectsp10
v1_waybel_1
v1_yellow18
v2_kurato_0
v2_partit_2
v2_pboole
v2_prob_2
v2_tsp_1
v2_valued_1
v3_measure6
v3_tsp_1
v3_valued_1
v4_finset_1
v4_ideal_1
v4_measure6
v4_supinf_2
v4_valued_1
v5_mesfunc5
v6_mesfunc5
v8_graph_1
v8_pcs_0
v9_valued_0
