use Test::More tests => 12;

use warnings;
use strict;
use Regexp::DefaultFlags;

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
my $source_article_path = "$FindBin::Bin/xboole_0.miz";
ok (ensure_readable_file ($source_article_path),
    'test article exists at the expected location');
ok (copy ($source_article_path, $dir),
    'copy set.miz to the temporary directory');

my $article_copy_in_tempdir = "${dir}/xboole_0.miz";

ok (my $article = Article->new (path => $article_copy_in_tempdir),
    'construct an Article object for the empty article')
    or BAIL_OUT ('Unable to make an Article object for the test article.');
ok (! $article->is_verifiable (),
    'article is not verifiable before accom is called');
ok ($article->accom (),
    'article is accom\'able');
ok ($article->is_verifiable (),
    'article is verifiable after accom is called');

$article->minimize ();

my @needed = $article->needed_items ();

diag ('Needed items after minimization: ', "\n", join ("\n", @needed));

my @self_deps = grep { /\A xboole_0 / } @needed;

diag ('Self-dependencies:', "\n", scalar @self_deps == 0 ? '(none)' : join ("\n", @self_deps));

is (scalar @self_deps, 0,
    'no item of xboole_0 depends on itself');

### Local Variables: ***
### mode:perl ***
### End: ***
