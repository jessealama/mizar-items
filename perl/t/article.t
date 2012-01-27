use Test::More tests => 4;

use warnings;
use strict;
use File::Temp qw(tempdir);

BEGIN {
    use_ok ('Utils', qw(write_string_to_file));
}

BEGIN {
    use_ok ('Article');
}

ok (! defined eval { Article->new () },
    'an Article must have a path');

ok (! defined eval { Article->new (path => '/') },
    'an article must have a readable file for its path attribute');

### Local Variables: ***
### mode:perl ***
### End: ***
