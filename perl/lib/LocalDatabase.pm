package LocalDatabase;

use Moose;
use File::Basename qw(basename dirname);
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

    my $path_dirname = dirname ($path);

    if ( ensure_directory ($path)) {

	# DEBUG
	warn 'Directory ', $path, ' already exists; ensuring needed subdirectories exist...';

	# Make sure that the required subdirectories exist
	foreach my $dir ('dict', 'prel', 'text') {
	    my $subdir = "${path_dirname}/${dir}";
	    if (! ensure_directory ($subdir)) {
		mkdir $subdir
		    or croak ('Error: unable to make the required ', $dir, ' subdirectory under ', $path, '.');
	    }
	}
    } else {

	warn 'Directory ', $path, ' does not exist; ensure it and its needed subdirectories exist...';

	# Make the local db directory if it doesn't yet exist
	mkdir $path
	    or croak ('Error: unable to make a directory at ', $path, '.');
	foreach my $subdir_name ('dict', 'prel', 'text') {
	    my $subdir = "${path}/${subdir_name}";

	    # DEBUG
	    warn 'Making subdir ', $subdir;

	    mkdir $subdir
		or croak ('Error: unable to make the \'', $subdir_name, '\' subdirectory of ', $path, '.', "\n");
	}
    }
}

1;
__END__
