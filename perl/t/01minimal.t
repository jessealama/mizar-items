use Test::More tests => 10;

# Try to load our module
BEGIN {
    use_ok( 'Mizar')
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

ok (my $empty_article_fh = File::Temp->new (), 'can we make a temporary file?')
    or BAIL_OUT ('We cannot create a temporary file in which to save a Mizar article; no point in continuing.');

ok (print {$empty_article_fh} ($empty_article),
    'write the shortest possible Mizar article to disk')
    or BAIL_OUT ('If we cannot save an article to disk, we cannot proceed.');

my @minimal_call = ($minimal_script, $empty_article_fh->filename);
ok (run (\@minimal_call, '>', '/dev/null'),
    'minimize the shortest possible Mizar article');


### Local Variables: ***
### mode:perl ***
### End: ***
