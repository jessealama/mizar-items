#!/usr/bin/perl

use warnings;
use strict;
use Getopt::Long;
use Pod::Usage;
use File::Basename qw(basename dirname);
use Carp qw(croak);
use XML::LibXML;
use File::Copy qw(copy move);

sub ensure_directory {
  my $dir = shift;
  if (! -e $dir) {
    die 'Error: the required directory ', $dir, ' does not exist.';
  }

  if (! -d $dir) {
    die 'Error: the required directory ', $dir, ' does not exist (as a directory).';
  }
  return 1;
}

sub ensure_readable_file {
  my $file = shift;

  if (! -e $file) {
    croak ('Error: ', $file, ' does not exist.');
  }
  if (! -f $file) {
    croak ('Error: ', $file, ' is not a file.');
  }

  if (! -r $file) {
    croak ('Error: ', $file, ' is unreadable.');
  }

  return 1;
}

my $help = 0;
my $man = 0;
my $verbose = 0;
my $checker_only = 0;
my $debug = 0;
my $stylesheet_home = '/Users/alama/sources/mizar/xsl4mizar/items';
my $script_home = '/Users/alama/sources/mizar/xsl4mizar/items';

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'checker-only' => \$checker_only,
	   'debug' => \$debug,
	   'stylesheet-home=s' => \$stylesheet_home)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) unless (scalar @ARGV == 1);

my $article = $ARGV[0];

my $article_basename = basename ($article, '.miz');
my $article_dirname = dirname ($article);
my $article_miz = "${article_dirname}/${article_basename}.miz";
my $article_miz_full = File::Spec->rel2abs ($article_miz);
my $article_err = "${article_dirname}/${article_basename}.err";
my $article_atr = "${article_dirname}/${article_basename}.atr";

ensure_readable_file ($article_atr);
ensure_directory ($stylesheet_home);
ensure_directory ($script_home);

my %stylesheet_paths =
  ('properties-of-constructor' => "${stylesheet_home}/properties-of-constructor.xsl",
   'strip-property' => "${stylesheet_home}/strip-property.xsl");

sub path_of_stylesheet {
  my $stylesheet_name = shift;
  if (defined $stylesheet_paths{$stylesheet_name}) {
    return $stylesheet_paths{$stylesheet_name};
  } else {
    croak ('Error: we were asked to look up the path for the ', $stylesheet_name, ' stylesheet, but that is unknown.');
  }
}

foreach my $stylesheet (keys %stylesheet_paths) {
  my $stylesheet_path = path_of_stylesheet ($stylesheet);
  if (! -e $stylesheet_path) {
    croak ('Error: we expected to find the ', $stylesheet, ' stylesheet at', "\n", "\n", '  ', $stylesheet_path, "\n", "\n", 'but it could not be found there.');
  }
  if (! -r $stylesheet_path) {
    croak ('Error: the ', $stylesheet, ' stylesheet at', "\n", "\n", '  ', $stylesheet_path, "\n", "\n", 'is unreadable.');
  }
}

my $properties_of_constructor_stylesheet
  = path_of_stylesheet ('properties-of-constructor');
my $strip_property_stylesheet
  = path_of_stylesheet ('strip-property');

my $inferred_constructors_script = "${script_home}/inferred-constructors.pl";

if (! -e $inferred_constructors_script) {
  croak ('Error: the inferred-constructors script does not exist at the expected location', "\n", "\n", '  ', $inferred_constructors_script, "\n");
}

if (! -f $inferred_constructors_script) {
  croak ('Error: the inferred-constructors script does not exist at the expected location', "\n", "\n", '  ', $inferred_constructors_script, "\n");
}

if (! -r $inferred_constructors_script) {
  croak ('Error: the inferred-constructors script at', "\n", "\n", '  ', $inferred_constructors_script, 'is unreadable.',"\n");
}

if (! -x $inferred_constructors_script) {
  croak ('Error: the inferred-constructors script at', "\n", "\n", '  ', $inferred_constructors_script, 'is not executable.',"\n");
}

my @inferred_constructors = `$inferred_constructors_script --stylesheet-home=${stylesheet_home} $article_miz_full`;
chomp @inferred_constructors;

# Make a copy of the atr
my $article_atr_orig = "${article_dirname}/${article_basename}.atr.orig";
copy ($article_atr, $article_atr_orig)
  or croak ('Error: unable to save a copy of the original .atr file at ', $article_atr, '.');

# Dump all properties of all constructors that are not inferred
my $inferred_constructors_token_list = join (',', @inferred_constructors);
$inferred_constructors_token_list = ',' . $inferred_constructors_token_list . ',';
$inferred_constructors_token_list =~ s/constructor//g;

# DEBUG
warn 'The inferred constructors token list is: ', "\n", $inferred_constructors_token_list;

my $delete_properties_stylesheet = "${stylesheet_home}/delete-properties.xsl";

my $article_atr_tmp = "${article_dirname}/${article_basename}.atr.tmp";
my $xsltproc_delete_status = system ("xsltproc --stringparam constructors '$inferred_constructors_token_list' $delete_properties_stylesheet $article_atr > $article_atr_tmp");
my $xsltproc_delete_exit_code = $xsltproc_delete_status >> 8;
if ($xsltproc_delete_exit_code != 0) {
  croak ('Error: xsltproc did not exit cleanly applying the delete-constructors stylesheet to ', $article_atr, '.', "\n");
}
move ($article_atr_tmp, $article_atr)
  or croak ('Error: unable to rename ', $article_atr_tmp, ' to ', $article_atr, '.', "\n");

# Paranoia: check that the article is still verifiable with the diminished atr file
my $verifier_check_call = undef;
if ($checker_only) {
  $verifier_check_call = "verifier -c -q -s -l $article_miz > /dev/null 2> /dev/null";
} else {
  $verifier_check_call = "verifier -q -s -l $article_miz > /dev/null 2> /dev/null";
}
my $verifier_check_status = system ($verifier_check_call);
my $verifier_check_exit_code = $verifier_check_status >> 8;
if ($verifier_check_exit_code != 0 || -s $article_err) {
  croak ('Error: we are unable to verify ', $article_basename, ' with its trimmed .atr file.');
}

my $xml_parser = XML::LibXML->new (suppress_warnings => 0,
				   suppress_errors => 0);

my $atr_doc = undef;

if (! defined eval { $atr_doc = $xml_parser->parse_file ($article_atr) } ) {
  croak ('Error: the .atr file for ', $article_basename, ' at', "\n", "\n", '  ', $article_atr, "\n", "\n", 'is not a well-formed XML file.', "\n");
}

if ($debug) {
  print STDERR ('We are about to treat ', scalar @inferred_constructors, '.', "\n");
}

my %needed_constructor_properties = ();
my %unneeded_constructor_properties = ();

# Let's do it

foreach my $constructor (@inferred_constructors) {
  if ($constructor =~ /\A ([a-z0-9_]+) : (.) constructor : ([0-9]+) \z/x) {
    (my $aid, my $kind, my $nr) = ($1, $2, $3);
    my @properties = `xsltproc --stringparam 'kind' '${kind}' --stringparam 'nr' '${nr}' --stringparam 'aid' '${aid}' $properties_of_constructor_stylesheet $article_atr`;
    if ($debug) {
      print STDERR ($constructor, ' has ', scalar @properties, ' properties.', "\n");
    }
    chomp @properties;
    foreach my $property (@properties) {
      warn 'Considering property ', $property, ' of constructor ', $constructor;
      my $property_lc = lc $property;
      my $xsltproc_status = system ("xsltproc --stringparam 'kind' '${kind}' --stringparam 'nr' '${nr}' --stringparam 'aid' '${aid}' --stringparam 'property' '${property}' $strip_property_stylesheet $article_atr > $article_atr_tmp");
      my $xsltproc_exit_code = $xsltproc_status >> 8;
      if ($xsltproc_exit_code != 0) {
	croak ('Error: xsltproc did not exit clearly when applying the strip-property stylesheet to ', $article_atr, '.  Its exit code was ', $xsltproc_exit_code, '.');
      }
      copy ($article_atr_tmp, $article_atr);

      my $verifier_call;
      if ($checker_only) {
	$verifier_call = "verifier -c -q -s -l $article_miz > /dev/null 2>&1"
      } else {
	$verifier_call = "verifier -q -s -l $article_miz > /dev/null 2>&1"
      }

      my $verifier_status = system ($verifier_call);
      my $verifier_exit_code = $verifier_status >> 8;
      if ($verifier_exit_code == 0 && -z $article_err) {
	if ($verbose) {
	  print 'We can remove property ', $property, ' of ', $constructor, "\n";
	}
	$unneeded_constructor_properties{"${constructor}[${property_lc}]"} = 0;
	copy ($article_atr, $article_atr_orig)
	   or croak ('Error: we were unable to update the .atr for ', $article_basename, ' to reflect its independence from the property ', $property, ' of constructor ', $constructor, '.', "\n");
      } else {
	$needed_constructor_properties{"${constructor}[${property_lc}]"} = 0;
	if ($verbose) {
	  print 'We cannot remove property ', $property, ' of ', $constructor, "\n";
	}
	if ($debug) {
	  if (-z $article_err) {
	    print STDERR ('(empty .err file.)', "\n");
	  } else {
	    print STDERR ('Contents of the .err file:', "\n");
	    print STDERR ('======================================================================', "\n");
	    system ("cat $article_err");
	    print STDERR ('======================================================================', "\n");
	  }
	}
	copy ($article_atr_orig, $article_atr)
	  or croak ('Error: we are unable to restore the original article .atr from ', $article_atr_orig, '.', "\n");
      }
    }
  } else {
    croak ('Error: unable to make sense of the constructor \'', $constructor, '\'.', "\n");
  }
}

# Restore the state of the article's environment
my $verifier_call;
if ($checker_only) {
  $verifier_call = "verifier -c -q -s -l $article_miz > /dev/null 2>&1"
} else {
  $verifier_call = "verifier -q -s -l $article_miz > /dev/null 2>&1"
}
my $verifier_status = system ($verifier_call);
my $verifier_exit_code = $verifier_status >> 8;
if ($verifier_exit_code != 0 || -s $article_err) {
  croak ('Error: after minimizing properties for ', $article_basename, ', we failed to verify it.');
}

foreach my $constructor_property (keys %needed_constructor_properties) {
  print $constructor_property, "\n";
}

__END__

=pod

=head1 minimize-properties.pl

minimize-properties.pl - Minimize the properties of imported constructors of a Mizar article

=head1 SYNOPSIS

minimize-properties.pl [options] mizar-article

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing.

=back

=head1 DESCRIPTION

B<minimize-properties.pl> will try to sequentially remove all
properties from all needed constructors of a Mizar article.

B<minimize-properties.pl> works only with all B<needed> constructors,
a list of which is inferred by sniffing through the XML and the
environment of the artice (apart from the .atr, of course).  It does
not try to remove properties from all constructors appearing in the
.atr file for the article.  Doing so would be sound, but quite
needless, especially for large Mizar articles that import much from
the library, since the vast majority of imported constructors (and
hence their properties) are not really needed for the article to be
successful.

For each needed constructor, we compute the list of its properties.
We then sequentially try removing each one.  If we can, then we remove
it from the .atr and continue.  If we cannot, we leave it.

=head1 BUGS AND LIMITATIONS

The simple sequential algorithm used to see which constructor
properties are removable is unsound: if the properties were removed in
a different order, we might get different results.  I think there may
be something suspicious with the relation:

=over 8

  P is the set of constructor properties is needed for the article A

=back

This program gives one sense to this relation (P is ordered in a
certain way, and we reeatedly veify A under a smaller and smaller set
of constuctor properties), but others are arguably also possible.  One
could implement the notion

=over 8

  the set P of constructor properties is such that no element of it
  can be removed from A while preserving A's verifiability

=back

That there could be different answers given depending on the ordering
is intuitively established as follows.  Suppose the content of the
Mizar article A is a simple disjunction of two properties of binary
relations, reflexivity and symmetry.  Either property could be removed
from A, but we cannot remove both.  Thus, depending on the order in
which we treat the proeprties, we would give different answers: the
result of treating symmetry first and then reflexivity would be that
reflexivity is needed; the result of treating reflexivity first and
then symmetry would be that symmetry is needed.  This seems to be
wrong.  Note, though, that the alternative approach above, in which we
simply remove exactly one propertt, test verifiability, and then
restore it, also leads to an incorrect result: that method would say
that neither property is needed.

The example uses a contrived "disjunctive" theorem.  I don't know
whether there might be other examples.  Nor do I know how often, or
whether, this kind of phenomenon occurs 'in the wild' of the MML.  One
could detect it by using the two different methods on all items of the
MML and seeing whether they give different results.

=head1 SEE ALSO

=over 8

=item L<http://mizar.org/>

=back

=head1 AUTHOR

Jesse Alama <jesse.alama@gmail.com>

=head1 LICENSE AND COPYRIGHT

This source is offered under the terms of
L<the GNU GPL version 3|http://www.gnu.org/licenses/gpl-3.0.en.html>.

=cut
