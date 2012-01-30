package LocalDatabase;

use Moose;
use File::Basename qw(basename dirname);
use File::Spec;
use Carp qw(croak);
use Cwd;
use IPC::Run qw(run);
use XML::LibXML;

# Our stuff
use Utils qw(ensure_directory ensure_readable_file);

has 'location' => (
    is => 'ro',
    reader => 'get_location',
);

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
		if ($dco_doc->exists ($xpath)) {
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

sub absolutize {
    my $self = shift;

    my $text_subdir = $self->get_text_subdir ();

    my @fragment_xmls = glob "${text_subdir}/*.xml";
    my $fragment_xmls_with_space = join (' ', @fragment_xmls);

    my @parallel_call = ('parallel',
			 'xsltproc',
			 '--output', '{.}.xml1',
			 Mizar::path_for_stylesheet ('addabsrefs'),
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

    my $article_miz = "${text_subdir}/${article_name}.miz";

    if (! ensure_readable_file ($article_miz)) {
	croak ('Error: there is no article by the name \'', $article_miz, '\' under ', $self->get_location (), '.');
    }

    my $article = Article->new (path => $article_miz);

    my @deps = $article->needed_items ();

    if (wantarray) {
	return @deps;
    } else {
	\@deps;
    }
}

1;
__END__
