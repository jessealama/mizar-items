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
  ('strip-property' => "${stylesheet_home}/strip-property.xsl",
   'delete-properties' => "${stylesheet_home}/delete-properties.xsl",
   'delete-abstractness' => "${stylesheet_home}/delete-abstractness.xsl",
   'constructors-with-abstractness' => "${stylesheet_home}/constructors-with-abstractness.xsl");

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

my $strip_property_stylesheet = $stylesheet_paths{'strip-property'};
my $delete_properties_stylesheet = $stylesheet_paths{'delete-properties'};
my $delete_abstractness_stylesheet = $stylesheet_paths{'delete-abstractness'};
my $constructors_with_abstractness_stylesheet = $stylesheet_paths{'constructors-with-abstractness'};

my @abstractful_constructors = `xsltproc $constructors_with_abstractness_stylesheet $article_atr`;
chomp @abstractful_constructors;

my $article_atr_orig = "${article_dirname}/${article_basename}.atr.orig";

copy ($article_atr, $article_atr_orig)
  or croak ('Error: unable to save a copy of the original .atr file for ', $article_basename, '.', "\n");

my $article_atr_tmp = "${article_dirname}/${article_basename}.atr.tmp";

my %removable = ();
my %unremovable = ();

foreach my $constructor (@abstractful_constructors) {

  if ($constructor =~ /\A ([a-z0-9_]+) : (.) constructor : ([0-9]+) \z/x)  {
    (my $aid, my $kind, my $nr) = ($1, $2, $3);

    my $xsltproc_delete_status = system ("xsltproc --stringparam aid '${aid}' --stringparam nr '${nr}' --stringparam kind '${kind}' --stringparam property 'abstractness' $strip_property_stylesheet $article_atr > $article_atr_tmp");
    my $xsltproc_delete_exit_code = $xsltproc_delete_status >> 8;
    if ($xsltproc_delete_exit_code != 0) {
      croak ('Error: xsltproc did not exit cleanly applying the delete-constructors stylesheet to ', $article_atr, '.', "\n");
    }
    move ($article_atr_tmp, $article_atr)
      or croak ('Error: unable to rename ', $article_atr_tmp, ' to ', $article_atr, '.', "\n");

    # Is the article is still verifiable when we dump abstractness from this constructor?
    my $verifier_check_call = undef;
    if ($checker_only) {
      $verifier_check_call = "verifier -c -q -s -l $article_miz > /dev/null 2> /dev/null";
    } else {
      $verifier_check_call = "verifier -q -s -l $article_miz > /dev/null 2> /dev/null";
    }
    my $verifier_check_status = system ($verifier_check_call);
    my $verifier_check_exit_code = $verifier_check_status >> 8;
    if ($verifier_check_exit_code != 0 || ! -z $article_err) {
      $unremovable{$constructor} = 0;
    } else {
      $removable{$constructor} = 0;
    }

    copy ($article_atr_orig, $article_atr);
  } else {
    croak ('Error: unable to make sense of the constructor \'', $constructor, '\'.', "\n");
  }
}

# We now know which abstractness principles can be dumped,
# individually.  Let's dump all of them at once.

my $removable_constructors = join (",", keys %removable);
$removable_constructors =~ s/constructor//g;
my $removable_token_list = ",${removable_constructors},";

# DEBUG
warn 'Removable constructors token list: ', $removable_token_list;

my $xsltproc_delete_status = system ("xsltproc --stringparam constructors ',${removable_token_list},' $delete_abstractness_stylesheet $article_atr > $article_atr_tmp");
my $xsltproc_delete_exit_code = $xsltproc_delete_status >> 8;
if ($xsltproc_delete_exit_code != 0) {
  croak ('Error: xsltproc did not exit cleanly applying the delete-abstractness stylesheet to ', $article_atr, '.', "\n");
}
copy ($article_atr_tmp, $article_atr)
  or croak ('Error: unable to rename ', $article_atr_tmp, ' to ', $article_atr, '.', "\n");

# Is the article is still verifiable when we dump abstractness from this constructor?
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

foreach my $constructor (keys %unremovable) {
  print $constructor, "\n";
}

__END__
