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

1;
__END__
