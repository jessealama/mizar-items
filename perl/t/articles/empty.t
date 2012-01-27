use Test::More tests => 14;

use warnings;
use strict;
use File::Temp qw(tempdir);

######################################################################
## Setting up
######################################################################

BEGIN {
    use_ok ('Utils', qw(write_string_to_file));
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

BEGIN {
    use_ok ('FindBin');
}

ok (-e "$FindBin::Bin/empty.miz",
    'empty article exists in the expected location');

ok (my $dir = tempdir (),
    'make a temporary directory');
ok (copy ("$FindBin::Bin/empty.miz", $dir),
    'copy empty.miz to the temporary directory');

my $path_for_empty_article = "${dir}/empty.miz";

ok (my $empty_article = Article->new (path => $path_for_empty_article),
    'construct an Article object for the empty article');
ok ($empty_article->accom (),
    'empty article is accom\'able');
ok ($empty_article->verify (),
    'empty article is verifiable after accom is called');

######################################################################
## Test dependencies
######################################################################

my @constructors = $empty_article->needed_constructors ();

ok (scalar @constructors > 0,
    'before minimization, the empty article depends on at least one constructor');

diag ('Needed constructors: ', join ("\n", @constructors));

my @properties_needed_by_minimal = $empty_article->minimize_properties ();

is (scalar @properties_needed_by_minimal, 0,
    'empty article needs 0 constructor properties');

######################################################################
## Minimize
######################################################################

$empty_article->minimize ();

my @needed_items = $empty_article->needed_items ();

diag ('Needed items after minimization: ', join (' ', @needed_items));

is (scalar @needed_items, 0,
    'after minimization, the empty article depends on nothing at all');

### Local Variables: ***
### mode:perl ***
### End: ***
