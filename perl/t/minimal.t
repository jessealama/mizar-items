use Test::More qw(no_plan);
use strict;

# Try to load our module
BEGIN {
    use_ok( 'Mizar', qw(accom verifier))
	or BAIL_OUT ('Unable to load the Mizar module; no point in continuing.');
}

# Try to load the Utils module
BEGIN {
    use_ok( 'Utils', qw(ensure_executable))
	or BAIL_OUT ('Unable to load the Mizar module; no point in continuing.');
}

# Ensure that we can run stuff
BEGIN {
    use_ok ('IPC::Run', qw(run));
}

# Ensure that we can find where we are
BEGIN {
    use_ok ('FindBin');
}

# Ensure that we can save articles to a temporary directory
BEGIN {
    use_ok ('File::Temp', qw(tempdir tempfile))
	or BAIL_OUT ('We need to create temporary directores and files using the File::Temp module.  If that module cannot be loaded, there is no point in continuing.');
}

my $bin_directory = "$FindBin::Bin/../bin";

ok (-d $bin_directory, 'do we know where to find the minimization script?');

my $minimal_script = "${bin_directory}/minimal.pl";

ok (ensure_executable ($minimal_script), 'is the minimization script executable?')
    or BAIL_OUT ('The article minimization script could not be found at the expected location (', $minimal_script, '; there is no point in continuing.');

my $empty_article = <<"END_EMPTY_ARTICLE";
environ begin
END_EMPTY_ARTICLE

my $dir_for_empty_article = File::Temp->newdir ();

ok (defined $dir_for_empty_article,
    'we made a temporary directory')
    or BAIL_OUT ('We cannot create a temporary file in which to save a Mizar article; no point in continuing.');

ok (defined $dir_for_empty_article, 'we have a directory for the shortest article')
    or BAIL_OUT ('Unable to create temporary directory.');

my $shortest_miz_path = "${dir_for_empty_article}/shortest.miz";

diag ('shortest article path: ', $shortest_miz_path);

ok ((open (my $miz_fh, '>', $shortest_miz_path)),
    'open output filehandle for saving the shortest miz');
ok ((print {$miz_fh} ($empty_article)),
    'print shortest miz to the output filehandle');
ok ((close $miz_fh),
    'close the output filehandle');

diag ('shortest article path: ', $shortest_miz_path);

ok (accom ($shortest_miz_path), 'empty article can be accomodated')
    or BAIL_OUT ('Unable to accommodate minimal article (??)');
ok (verifier ($shortest_miz_path), 'empty article is verifiable')
    or BAIL_OUT ('Unable to verify minimal article (??)');

my @minimal_call = ($minimal_script, $shortest_miz_path);
ok (run (\@minimal_call, '>', '/dev/null'),
    'minimize the shortest possible Mizar article');


### Local Variables: ***
### mode:perl ***
### End: ***
