use Test::More qw(no_plan);

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

# Try to load the Utils module
BEGIN {
    use_ok( 'File::Temp', qw(tempdir) )
	or BAIL_OUT ('Unable to load File::Temp; no point in continuing.');
}

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

ok ((open (my $miz_fh, '>', $shortest_miz_path)),
    'open output filehandle for saving the shortest miz');
ok ((print {$miz_fh} ($empty_article)),
    'print shortest miz to the output filehandle');
ok ((close $miz_fh),
    'close the output filehandle');

ok (! (verifier ($shortest_miz_path)),
	'shortest mizar article is unverifiable before being accommodated');
ok ((accom ($shortest_miz_path)), 'empty article can be accomodated');
ok ((verifier ($shortest_miz_path)), 'empty article is verifiable');
ok ((verifier ($shortest_miz_path)), 'empty article is reverifiable');

ok (unlink ($shortest_miz_path), 'delete shortest article');
ok (! verifier ($shortest_miz_path), 'unable to verify a non-existent miz');


### Local Variables: ***
### mode:perl ***
### End: ***
