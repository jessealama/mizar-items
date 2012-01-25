use Test::More tests => 3;

# Try to load Utils.pm
BEGIN {
    use_ok( 'Utils', qw(ensure_directory ensure_executable) );
}

ok (ensure_directory ('/tmp'));
ok (ensure_executable ('/bin/ls'));

### Local Variables: ***
### mode:perl ***
### End: ***
