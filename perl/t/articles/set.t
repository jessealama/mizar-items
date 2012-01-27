use Test::More tests => 15;

use warnings;
use strict;

BEGIN {
    use_ok ('Utils', qw(write_string_to_file ensure_readable_file));
}

BEGIN {
    use_ok ('Article');
}

BEGIN {
    use_ok ('File::Temp', qw(tempdir));
}

BEGIN {
    use_ok ('File::Copy', qw(copy));
}

ok (my $dir = tempdir (),
    'make a temporary directory');
my $source_article_path = "$FindBin::Bin/set.miz";
ok (ensure_readable_file ($source_article_path),
    'test article exists at the expected location');
ok (copy ($source_article_path, $dir),
    'copy set.miz to the temporary directory');

my $article_copy_in_tempdir = "${dir}/set.miz";

ok (my $article = Article->new (path => $article_copy_in_tempdir),
    'construct an Article object for the empty article')
    or BAIL_OUT ('Unable to make an Article object for the test article.');
ok (! $article->is_verifiable (),
    'empty article is not verifiable before accom is called');
ok ($article->accom (),
    'empty article is accom\'able');
ok ($article->is_verifiable (),
    'empty article is verifiable after accom is called');

my @constructors = $article->needed_constructors ();

ok (scalar @constructors > 0,
    'before minimization, the empty article does depend on at least one constructor');

diag ('Needed constructors: ', join ("\n", @constructors));

$article->minimize ();

my @needed = $article->needed_items ();

diag ('Needed items after minimization: ', join ("\n", @needed));

is (scalar @needed, 2,
    'empty article needs exactly 2 item');

is (scalar (grep { /hidden:mconstructor:1/ } @needed), 1,
    'one needed item is the set constructor');

is (scalar (grep { /hidden:mpattern:1/ } @needed), 1,
    'another needed item is the set pattern');

### Local Variables: ***
### mode:perl ***
### End: ***
