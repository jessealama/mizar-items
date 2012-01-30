#!/usr/bin/perl

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Copy qw(copy move);
use File::Copy::Recursive qw(dircopy dirmove);
use Getopt::Long;
use Pod::Usage;
use File::Temp qw(tempdir);
use Carp qw(croak);

use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';
use Utils qw(ensure_directory ensure_readable_file ensure_executable);
use LocalDatabase;

my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $paranoid = 0;
my $minimize_whole_article = 0;
my $script_home = '/Users/alama/sources/mizar/mizar-items/bin';
my $stylesheet_home = '/Users/alama/sources/mizar-items/xsl';
my $nice = 0;
my $num_jobs = undef;
my $workdir = undef;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'debug' => \$debug,
           'minimize-whole-article' => \$minimize_whole_article,
	   'script-home=s' => \$script_home,
	   'stylesheet-home=s' => \$stylesheet_home,
	   'nice' => \$nice,
	   'paranoid' => \$paranoid,
	   'jobs=i' => \$num_jobs,
	   'workdir=s' => \$workdir)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

if (defined $num_jobs) {
  if ($num_jobs < 1) {
    pod2usage(1);
  }
}

if (scalar @ARGV != 1) {
    pod2usage (1);
}

if (defined $workdir) {
  ensure_directory ($workdir)
      or croak ('Error: the work directory does not exist.');
}

my $article_dir = $ARGV[0];
my $article_text_dir = "${article_dir}/text";

ensure_directory ($article_dir)
    or croak ('Error: the supplied article directory ', $article_dir, ' does not exist.');
ensure_directory ($article_text_dir)
    or croak ('Ouch');

my @miz_candidates = `find $article_dir -maxdepth 1 -mindepth 1 -type f -name "*.miz"`;
chomp @miz_candidates;

if (scalar @miz_candidates == 0) {
  print 'Error: we did not find any .miz files under ', $article_dir, '.', "\n";
  exit 1;
}

if (scalar @miz_candidates > 1) {
  print 'Error: we found multiple .miz files under $article_dir; which one should we use?', "\n";
  exit 1;
}

my $itemized_article_miz = $miz_candidates[0];
my $itemized_article_basename = basename ($itemized_article_miz, '.miz');

ensure_readable_file ($itemized_article_miz);
ensure_directory ($script_home);

my $minimize_script = "${script_home}/minimal.pl";
my $minimize_internally_script = "${script_home}/minimize-internally.pl";

ensure_executable ($minimize_script)
    or croak ('Error: the minimize script could not be found at the expected location (', $minimize_script, ').');
ensure_executable ($minimize_internally_script)
    or croak ('Error: the minimize-internally script could not be found at the expected location (', $minimize_internally_script, ').');

ensure_directory ($stylesheet_home);

my $prefer_environment_stylesheet = "${stylesheet_home}/prefer-environment.xsl";

ensure_readable_file ($prefer_environment_stylesheet);

my $real_workdir = undef;
if (defined $workdir) {
  $real_workdir = tempdir ("minimization-${itemized_article_basename}-XXXX",
			   CLEANUP => 1,
			   DIR => $workdir);
  if ($verbose == 1) {
    print 'Copying the itemized article directory', "\n", "\n", '  ', $article_dir, "\n", "\n", 'to the temporary directory', "\n", "\n", '  ', $real_workdir, "\n";
  }
  dircopy ($article_dir, $real_workdir)
    or (print 'Error: something went wrong copying', "\n", "\n", '  ', $article_dir, "\n", "\n", 'to', "\n", "\n", '  ', $real_workdir, "\n", "\n", $!, "\n" && exit 1);
} else {
  $real_workdir = File::Spec->rel2abs ($article_dir);
}

my $real_text_dir = "${real_workdir}/text";

my @fragments = `find $real_text_dir -type f -name "ckb*.miz" | grep 'ckb[1-9][0-9]*.miz'`;
chomp @fragments;

if (scalar @fragments == 0) {
  print 'Error: we found 0 fragments under ', $real_text_dir, '.', "\n";
  exit 1;
}

my $real_itemized_article_miz = "${real_workdir}/${itemized_article_basename}.miz";

my $parallel_call = undef;

if ($nice == 1) {
  if (defined $num_jobs) {
    if ($paranoid == 1) {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs $num_jobs nice ${minimize_script} --fast-theorems --fast-schemes --paranoid --verbose {}";
    } else {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs $num_jobs nice ${minimize_script} --fast-theorems --fast-schemes --verbose {}";
    }
  } else {
    if ($paranoid == 1) {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs +0 nice ${minimize_script} --paranoid --verbose --fast-theorems --fast-schemes  {}";
    } else {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs +0 nice ${minimize_script} --fast-theorems --fast-schemes  --verbose {}";
    }
  }
} else {
  if (defined $num_jobs) {
    if ($paranoid == 1) {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs $num_jobs ${minimize_script} --fast-theorems --fast-schemes  --paranoid --verbose {}";
    } else {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs $num_jobs ${minimize_script} --fast-theorems --fast-schemes  --verbose {}";
    }
  } else {
    if ($paranoid == 1) {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs +0 ${minimize_script} --paranoid --verbose --fast-theorems --fast-schemes {}";
    } else {
      $parallel_call = "find ${real_text_dir} -name 'ckb*.miz' | grep 'ckb[1-9][0-9]*.miz' | parallel --jobs +0 ${minimize_script} --verbose --fast-theorems --fast-schemes  {}";
    }
  }
}

chdir $real_workdir
  or croak ('Error: cannot change directory to ', $real_workdir, '.', "\n");

my $parallel_minimize_status = system ($parallel_call);
my $parallel_minimize_exit_code = $parallel_minimize_status >> 8;

if ($parallel_minimize_exit_code != 0) {
  croak ('Error: parallel did not exit cleanly when minimizing the fragments of ', $itemized_article_basename, ' under ', $real_text_dir, '.', "\n", 'Its exit code was ', $parallel_minimize_exit_code, '.', "\n");
}

# Now check for conditions and constructor properties

opendir my $text_dir_fh, $real_text_dir
    or croak ('Error: unable to open the directory ', $real_text_dir, '.');
my @ps_and_cs
    = grep { -f "${real_text_dir}/$_"
		 && / ckb[0-9]+[a-z]{2}.miz \z/x } readdir $text_dir_fh;
closedir $text_dir_fh;
my @properties_and_conditions = map { "${real_text_dir}/$_" } @ps_and_cs;

if ($debug) {
  print {*STDERR} 'There are ', scalar @properties_and_conditions, ' properties/conditions:', join ("\n", @properties_and_conditions), "\n";

}

# First, internally minimize

my $parallel_minimize_internally_status
    = system ('parallel '
	      . $minimize_internally_script
	      . ' ::: ' . join (' ', @properties_and_conditions));
my $parallel_minimize_internally_exit_code
    = $parallel_minimize_internally_status >> 8;
if ($parallel_minimize_internally_exit_code != 0) {
    croak ('Error: parallel died internally minimizing the properties and conditions articles for ', $itemized_article_basename, '.');
}

# Now minimize the envionment of the property and correctness condition guys

my $parallel_minimize_ps_and_cs_status
    = system ('parallel '
	      . $minimize_script
	      . ' --checker-only '
	      . ' ::: ' . join (' ', @properties_and_conditions));
my $parallel_minimize_ps_and_cs_exit_code
    = $parallel_minimize_ps_and_cs_status >> 8;
if ($parallel_minimize_ps_and_cs_exit_code != 0) {
    croak ('Error: parallel died minimizing the properties and conditions articles for ', $itemized_article_basename, '.');
}

if ($paranoid == 1) {
  my @bad_guys = `find ${real_text_dir} -name 'ckb*.err' ! -empty -exec basename {} .err ';' | sed -e 's/ckb//' | sort --numeric-sort`;
  chomp @bad_guys;
  if (scalar @bad_guys > 0) {
    print 'Error: some fragments of ', $itemized_article_basename, ' are not verifiable!  The failed fragments are:', "\n";
    foreach my $bad_guy (@bad_guys) {
      print '* ', $bad_guy, "\n";
    }
    exit 1;
  }
  if ($verbose == 1) {
    print 'Paranoia: all minimized fragments are still verifiable.', "\n";
  }
}

if ($verbose == 1) {
  print 'Done.', "\n";
}

if (defined $workdir) {
  if ($verbose == 1) {
    print 'Moving the newly minimized directory back from', "\n", "\n", '  ', $real_workdir, "\n", "\n", 'to', "\n", "\n", '  ', $article_dir, "\n";
  }
  dirmove ($real_workdir, $article_dir)
    or (print 'Error: something went wrong moving the newly minimized directory back from', "\n", "\n", '  ', $real_workdir, "\n", "\n", 'to', "\n", "\n", '  ', $article_dir, "\n" && exit 1);
}

exit 0;

__END__

=pod

=head1 minimize-itemized-article.pl

minimize-itemized-article.pl - Minimize the fragments of an "itemized" mizar article

=head1 SYNOPSIS

minimize-itemized-article.pl [options] itemized-mizar-article-directory

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what environment file we're minimizing, and for each environment
file, say how many environment "items" are present there and how many
we really need.

=item B<--minimize-whole-article>

Before minimizing the environment of each of the items from an
article, minimize the whole article and use its minimal environment as
a basis for each of the article's item's environment.

Such minimization may an expensive up-front cost, but it can save time
in the long run, depending on the size of the original article's
environment and how expensive it is to verify it.

=item B<--paranoid>

Call the verifier on each of the itemized articles.

=item B<--script-home=DIR>

The directory in which we will look for any needed scripts.

=item B<--stylesheet-home=DIR>

The directory in which we will look for any needed stylesheets.

=item B<--nice>

Use nice when minimizing the original article's fragments.

=item B<--jobs=NUM-JOBS>

Run NUM-JOBS fragment minimization jobs in parallel.  By default, all
available processors will be used.

=item B<--workdir=DIR>

Do the minimization in DIR (e.g., a ramdisk).  This means that before
anything else, the directory to be minimized will be copied to DIR.
Upon normal completion, the original direcory will be deleted and the
contents of the newly minimized directory will be moved into the
original directory.  If something goes wrong during the minimization,
the original directory will not be deleted, and the itemized article
subdirectory of DIR will be deleted.

=back

=head1 DESCRIPTION

B<minimize-itemized-article.pl> will construct, for each fragment of
an itemized mizar article, the smallest environment with respect to
which the given article is verifiable.

=cut
