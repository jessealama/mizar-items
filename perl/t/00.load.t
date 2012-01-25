# -*- perl-mode; -*-

use FindBin;
use lib "$FindBin::Bin/../lib";

use Test::More tests => 1;

BEGIN {
use_ok( 'Mizar' );
}

diag( "Testing Mizar $Mizar::VERSION" );
