package LocalDatabase;

use Moose;
use File::Basename qw(basename dirname);
use File::Spec;
use Carp qw(croak carp);
use Cwd;
use IPC::Run qw(run start);
use XML::LibXML;
use Data::Dumper;

# Our stuff
use Utils qw(ensure_directory ensure_readable_file slurp);
use Article;

has 'location' => (
    is => 'ro',
    reader => 'get_location',
);

has 'stylesheet_home' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_stylesheet_home',
    required => 1,
);

has 'script_home' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_script_home',
    required => 1,
);

sub dir_has_local_db_structure {
    my $dir = shift;

    if (ensure_directory ($dir)) {

	foreach my $s ('dict', 'prel', 'text') {
	    my $subdir = "${dir}/${s}";
	    if (! ensure_directory ($subdir)) {
		return 0;
	    }
	}

	return 1;

    } else {
	return 0;
    }

}

sub has_article {
    my $self = shift;
    my $article_name = shift;

    my $text_subdir = $self->get_text_subdir ();
    my $path = "${text_subdir}/${article_name}.miz";

    return ensure_readable_file ($path);
}

sub text_of_article {
    my $self = shift;
    my $article_name = shift;

    my $text_subdir = $self->get_text_subdir ();
    my $path = "${text_subdir}/${article_name}.miz";

    if (ensure_readable_file ($path)) {
	return slurp ($path);
    } else {
	my $location = $self->get_location ();
	croak ('Error: there is no article by the name ', $article_name, ' under ', $location, '.');
    }
}

sub BUILD {
    my $self = shift;

    my $path = $self->get_location ();

    my $path_dirname = File::Spec->catdir ($path);

    if ( ensure_directory ($path)) {

	# Make sure that the required subdirectories exist
	foreach my $dir ('dict', 'prel', 'text') {
	    my $subdir = "${path_dirname}/${dir}";
	    if (! ensure_directory ($subdir)) {
		mkdir $subdir
		    or croak ('Error: unable to make the required ', $dir, ' subdirectory under ', $path, '.');
	    }
	}
    } else {

	# Make the local db directory if it doesn't yet exist
	mkdir $path
	    or croak ('Error: unable to make a directory at ', $path, '.');
	foreach my $subdir_name ('dict', 'prel', 'text') {
	    my $subdir = "${path}/${subdir_name}";
	    mkdir $subdir
		or croak ('Error: unable to make the \'', $subdir_name, '\' subdirectory of ', $path, '.', "\n");
	}
    }

    my $sheet_home = $self->get_stylesheet_home ();
    if (! ensure_directory ($sheet_home)) {
	croak ('Error: the supplied path (', $sheet_home, ') is not a directory.');
    }

    return $self;

    my $script_home = $self->get_script_home ();
    if (! ensure_directory ($script_home)) {
	croak ('Error: the supplied path (', $script_home, ') is not a directory.');
    }

    return $self;
}

my $xml_parser = XML::LibXML->new (suppress_warnings => 1,
				   suppress_errors => 1);

sub get_text_subdir {
    my $self = shift;

    my $loc = $self->get_location ();
    my $loc_dir = File::Spec->catdir ($loc);
    return "${loc_dir}/text";
}

sub get_prel_subdir {
    my $self = shift;

    my $loc = $self->get_location ();
    my $loc_dir = File::Spec->catdir ($loc);
    return "${loc_dir}/prel";
}

sub get_dict_subdir {
    my $self = shift;

    my $loc = $self->get_location ();
    my $loc_dir = File::Spec->catdir ($loc);
    return "${loc_dir}/dict";
}

sub prel_files {
    my $self = shift;

    my $prel = $self->get_prel_subdir ();

    return glob "${prel}/*.*";
}

sub articles {
    my $self = shift;

    my $text = $self->get_text_subdir ();

    my @miz_paths = glob "${text}/*.miz";
    my @basenames = map { basename ($_, '.miz') } @miz_paths;
    return @basenames;
}

sub files_in_prel_with_extension {
    my $self = shift;
    my $extension = shift;
    if (defined $extension) {
	my $prel = $self->get_prel_subdir ();
	my @files = glob "${prel}/*.${extension}";
	my @basename_files = map { basename ($_) } @files;
	return @basename_files;
    } else {
	croak ('Error: please supply an extension.');
    }
}

sub notations_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('dno');
}

sub registrations_in_prel {
    my $self = shift;
    my @reductions = $self->files_in_prel_with_extension ('drd');
    my @clusters = $self->files_in_prel_with_extension ('dcl');
    my @identifications = $self->files_in_prel_with_extension ('did');

    my @answer = ();
    push (@answer, @reductions);
    push (@answer, @clusters);
    push (@answer, @identifications);

    if (wantarray) {
	return @answer;
    } else {
	return join (' ', @answer);
    }
}

sub definientia_in_prel_of_kind {
    my $self = shift;
    my $kind = shift;

    my $prel_subdir = $self->get_prel_subdir ();

    my @answers = ();

    my @definientia = $self->files_in_prel_with_extension ('def');

    foreach my $definiens (@definientia) {
	my $definiens_path = "${prel_subdir}/${definiens}";
	if (ensure_readable_file ($definiens_path)) {
	    my $definiens_doc = eval { $xml_parser->parse_file ($definiens_path) };
	    if (defined $definiens_doc) {

		my $xpath = '/Definientia/Definiens[@constrkind = "' . (uc $kind) . '"]';

		if ($definiens_doc->exists ($xpath)) {
		    push (@answers, $definiens);
		}

	    } else {
		croak ('Error: the XML file at ', $definiens_doc, ' is not well-formed.');
	    }
	} else {
	    croak ('Error: the definiens file ', $definiens, ' does not exist at the expected location (', $definiens_path, ').');
	}
    }

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }
}

sub functor_definientia_in_prel {
    my $self = shift;
    return $self->definientia_in_prel_of_kind ('k');
}

sub mode_definientia_in_prel {
    my $self = shift;
    return $self->definientia_in_prel_of_kind ('m');
}

sub relation_definientia_in_prel {
    my $self = shift;
    return $self->definientia_in_prel_of_kind ('r');
}

sub attribute_definientia_in_prel {
    my $self = shift;
    return $self->definientia_in_prel_of_kind ('v');
}

sub clusters_in_prel_of_kind {
    my $self = shift;
    my $kind = shift;

    my $prel_subdir = $self->get_prel_subdir ();

    my @answers = ();

    my @clusters = $self->files_in_prel_with_extension ('dcl');

    foreach my $cluster (@clusters) {
	my $cluster_path = "${prel_subdir}/$cluster";
	if (ensure_readable_file ($cluster_path)) {
	    my $cluster_doc = eval { $xml_parser->parse_file ($cluster_path) };
	    if (defined $cluster_doc) {

		my $xpath = '/Registrations/' . (uc $kind) . 'Cluster';

		if ($cluster_doc->exists ($xpath)) {
		    push (@answers, $cluster);
		}

	    } else {
		croak ('Error: the XML file at ', $cluster_doc, ' is not well-formed.');
	    }
	} else {
	    croak ('Error: the cluster file ', $cluster, ' does not exist at the expected location (', $cluster_path, ').');
	}
    }

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }
}

sub conditional_clusters_in_prel {
    my $self = shift;
    return $self->clusters_in_prel_of_type ('c');
}

sub functorial_clusters_in_prel {
    my $self = shift;
    return $self->clusters_in_prel_of_type ('f');
}

sub existential_clusters_in_prel {
    my $self = shift;
    return $self->clusters_in_prel_of_type ('r');
}

sub clusters_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('dcl');
}

sub schemes_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('sch');
}

sub identifications_in_prel_of_kind {

    my $self = shift;
    my $kind = shift;

    my $prel_subdir = $self->get_prel_subdir ();

    my @dids = $self->files_in_prel_with_extension ('did');

    my @answers = ();

    foreach my $did (@dids) {

	my $did_path = "${prel_subdir}/$did";
	if (ensure_readable_file ($did_path)) {
	    my $did_doc = eval { $xml_parser->parse_file ($did_path) };
	    if (defined $did_doc) {

		my $xpath = '/IdentifyRegistrations/Identify[@constrkind = "' . (uc $kind) . '"]';
		if ($did_doc->exists ($xpath)) {
		    push (@answers, $did);
		}

	    } else {
		croak ('Error: the XML file at ', $did_doc, ' is not well-formed.');
	    }
	} else {
	    croak ('Error: the theorem file ', $did, ' does not exist at the expected location (', $did_path, ').');
	}
    }

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }

}

sub identifications_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('did');
}

sub reductions_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('drd');
}

sub constructors_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('dco');
}

sub definitions_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('def');
}

sub theorems_in_prel_of_kind {

    my $self = shift;
    my $kind = shift;

    my $prel_subdir = $self->get_prel_subdir ();

    my @thes = $self->files_in_prel_with_extension ('the');

    my @answers = ();

    foreach my $the (@thes) {

	my $the_path = "${prel_subdir}/$the";
	if (ensure_readable_file ($the_path)) {
	    my $the_doc = eval { $xml_parser->parse_file ($the_path) };
	    if (defined $the_doc) {

		my $xpath = '/Theorems/Theorem[@kind = "' . (uc $kind) . '"]';
		if ($the_doc->exists ($xpath)) {
		    push (@answers, $the);
		}

	    } else {
		croak ('Error: the XML file at ', $the_doc, ' is not well-formed.');
	    }
	} else {
	    croak ('Error: the theorem file ', $the, ' does not exist at the expected location (', $the_path, ').');
	}
    }

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }


}

sub constructors_in_prel_of_kind {

    my $self = shift;
    my $kind = shift;

    my $prel_subdir = $self->get_prel_subdir ();

    my @dcos = $self->files_in_prel_with_extension ('dco');

    my @answers = ();

    foreach my $dco (@dcos) {

	my $dco_path = "${prel_subdir}/$dco";
	if (ensure_readable_file ($dco_path)) {
	    my $dco_doc = eval { $xml_parser->parse_file ($dco_path) };
	    if (defined $dco_doc) {

		my $xpath = '/Constructors/Constructor[@kind = "' . (uc $kind) . '"]';
		my @nodes = $dco_doc->findnodes ($xpath);
		foreach my $node (@nodes) {
		    push (@answers, $dco);
		}

	    } else {
		croak ('Error: the XML file at ', $dco_doc, ' is not well-formed.');
	    }
	} else {
	    croak ('Error: the constructor file ', $dco, ' does not exist at the expected location (', $dco_path, ').');
	}
    }

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }


}

sub patterns_in_prel_of_kind {

    my $self = shift;
    my $kind = shift;

    my $prel_subdir = $self->get_prel_subdir ();

    my @dnos = $self->files_in_prel_with_extension ('dno');

    my @answers = ();

    foreach my $dno (@dnos) {

	my $dno_path = "${prel_subdir}/$dno";
	if (ensure_readable_file ($dno_path)) {
	    my $dno_doc = eval { $xml_parser->parse_file ($dno_path) };
	    if (defined $dno_doc) {

		my $xpath = '/Notations/Pattern[@kind = "' . (uc $kind) . '"]';
		if ($dno_doc->exists ($xpath)) {
		    push (@answers, $dno);
		}

	    } else {
		croak ('Error: the XML file at ', $dno_doc, ' is not well-formed.');
	    }
	} else {
	    croak ('Error: the constructor file ', $dno, ' does not exist at the expected location (', $dno_path, ').');
	}
    }

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }


}

sub deftheorems_in_prel {
    my $self = shift;
    return $self->theorems_in_prel_of_kind ('D');
}

sub theorems_in_prel {
    my $self = shift;
    return $self->theorems_in_prel_of_kind ('T');
}

sub theorems_and_deftheorems_in_prel {
    my $self = shift;
    my @theorems = $self->theorems_in_prel ();
    my @deftheorems = $self->deftheorems_in_prel ();

    my @answers = ();
    push (@answers, @theorems);
    push (@answers, @deftheorems);

    if (wantarray) {
	return @answers;
    } else {
	return join (' ', @answers);
    }
}

sub run_tool_in_local_database {
    my $self = shift;
    my $tool = shift;
    my $article = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $article_base = basename ($article, '.miz');

    my $location = $self->get_location ();
    my $article_path_in_text_dir = "text/${article_base}.miz";

    my $cwd = getcwd ();

    chdir $location
	or croak ('Error: unable to change directory to ', $location, ': ', $!);
    my $tool_results = Mizar::run_mizar_tool ($tool,
					      $article_path_in_text_dir,
					      \%parameters);
    chdir $cwd;

    return $tool_results;
}

sub accom {
    my $self = shift;
    my $article = shift;
    my $parameters_ref = shift;

    return $self->run_tool_in_local_database ('accom',
					      $article,
					      $parameters_ref);

}

sub verify {
    my $self = shift;
    my $article = shift;
    my $parameters_ref = shift;

    return $self->run_tool_in_local_database ('verifier',
					      $article,
					      $parameters_ref);

}

sub export {
    my $self = shift;
    my $article = shift;
    my $parameters_ref = shift;

    return $self->run_tool_in_local_database ('exporter',
					      $article,
					      $parameters_ref);

}

sub transfer {
    my $self = shift;
    my $article = shift;
    my $parameters_ref = shift;

    return $self->run_tool_in_local_database ('transfer',
					      $article,
					      $parameters_ref);

}

sub path_for_stylesheet {
    my $self = shift;
    my $sheet = shift;
    my $stylesheet_home = $self->get_stylesheet_home ();
    return "${stylesheet_home}/${sheet}.xsl";
}

sub path_for_script {
    my $self = shift;
    my $script = shift;
    my $script_home = $self->get_script_home ();
    return "${script_home}/${script}";
}

sub absolutize {
    my $self = shift;

    my $text_subdir = $self->get_text_subdir ();
    my $absrefs_stylesheet = $self->path_for_stylesheet ('addabsrefs');

    my @fragment_xmls = glob "${text_subdir}/*.xml";
    my $fragment_xmls_with_space = join (' ', @fragment_xmls);

    my @parallel_call = ('parallel',
			 'xsltproc',
			 '--output', '{.}.xml1',
			 $absrefs_stylesheet,
			 '{}',
			 ':::');
    foreach my $fragment_xml (@fragment_xmls) {
	push (@parallel_call, $fragment_xml);
    }

    return run (\@parallel_call,
		'>', '/dev/null',
		'2>', '/dev/null');
}

sub dependencies_of {
    my $self = shift;
    my $article_name = shift;

    # Check that there really is an article here with the given name

    my $text_subdir = $self->get_text_subdir ();
    my $stylesheet_home = $self->get_stylesheet_home ();
    my $script_home = $self->get_script_home ();

    my $article_miz = "${text_subdir}/${article_name}.miz";

    if (! ensure_readable_file ($article_miz)) {
	croak ('Error: there is no article by the name \'', $article_miz, '\' under ', $self->get_location (), '.');
    }

    my $article = Article->new (path => $article_miz,
			        stylesheet_home => $stylesheet_home,
			        script_home => $script_home);

    my @deps = $article->needed_items ();

    if (wantarray) {
	return @deps;
    } else {
	\@deps;
    }
}

sub minimize_articles {
    my $self = shift;
    my $articles_ref = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @article_names = defined $articles_ref ? @{$articles_ref} : ();

    my $location = $self->get_location ();
    my $text_dir = $self->get_text_subdir ();
    my $now = time;

    my @article_paths = map { "${text_dir}/${_}.miz" } @article_names;

    if (scalar @article_paths == 0) {
	return 1; # nothing to do
    } else {

	# Sanity check: all these paths exist

	foreach my $path (@article_paths) {
	    if (! ensure_readable_file ($path)) {
		croak ('Error: there is no file at ', $path, '.');
	    }
	}

	# Build a call to GNU parallel
	my $minimizer_script = $self->path_for_script ('minimal.pl');

	my @parallel_call = ('parallel');

	if (defined $parameters{'jobs'} && $parameters{'jobs'}) {
	    my $jobs = $parameters{'jobs'};
	    push (@parallel_call, '--jobs', $jobs);
	}

	my $logfile = "${location}/minimize-${now}.log";
	push (@parallel_call, '--joblog', $logfile);

	push (@parallel_call, $minimizer_script);

	if (defined $parameters{'checker-only'} && $parameters{'checker-only'}) {
	    push (@parallel_call, '--checker-only');
	}

	if (defined $parameters{'fast-theorems-and-schemes'}) {
	    push (@parallel_call, '--fast-theorems', '--fast-schemes');
	}

	if (defined $parameters{'randomize'} && $parameters{'randomize'}) {
	    push (@parallel_call, '--randomize');
	}

	push (@parallel_call, '{}', ':::');
	foreach my $path (@article_paths) {
	    push (@parallel_call, $path);
	}

	my $parallel_out = '';
	my $parallel_err = '';

	my $h = start (\@parallel_call,
		       '>', \$parallel_out);
#		       '2>', \$parallel_err);

	$h->finish ();

	# DEBUG
	# carp ('Done minimizing articles; here was the error output:', "\n", $parallel_err);

	my $parallel_exit_code = $h->result (0);

	if ($parallel_exit_code != 0) {
	    croak ('Error: parallel did not exit cleanly when minimizing some articles; the exit code was ', $parallel_exit_code, '.  Here is the standard error: (warning: it may be garbled because of the parallel computation):', "\n", $parallel_err);
	} else {
	    return 1;
	}

    }

}

sub minimize {
    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @article_names = $self->articles ();

    return $self->minimize_articles (\@article_names, \%parameters);
}

1;
__END__
