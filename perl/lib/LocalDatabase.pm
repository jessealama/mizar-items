package LocalDatabase;

use Moose;
use File::Basename qw(basename dirname);
use File::Spec;
use Carp qw(croak);

# Our stuff
use Utils qw(ensure_directory);

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

sub schemes_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('sch');
}

sub constructors_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('dco');
}

sub definitions_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('def');
}

sub theorems_in_prel {
    my $self = shift;
    return $self->files_in_prel_with_extension ('the');
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

1;
__END__
