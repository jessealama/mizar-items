#!/usr/bin/perl

# Check first that our environment is sensible

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw(tempfile);
use File::Basename qw(basename dirname);
use File::Spec;
use XML::LibXML;
use Cwd qw(cwd);
use File::Copy qw(copy move);
use Carp qw(croak);
use IPC::Cmd qw(can_run);

use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';
use Utils qw(ensure_directory ensure_readable_file extension strip_extension);
use Mizar;
use Article;
use LocalDatabase;
use Xsltproc qw(apply_stylesheet);

my $paranoid = 0;
my $stylesheet_home = '/Users/alama/sources/mizar/mizar-items';
my $verbose = 0;
my $debug = 0;
my $man = 0;
my $help = 0;
my $target_directory = undef;

GetOptions('help|?' => \$help,
           'man' => \$man,
           'verbose'  => \$verbose,
	   'debug' => \$debug,
	   'paranoid' => \$paranoid,
	   'stylesheet-home=s' => \$stylesheet_home,
	   'target-directory=s' => \$target_directory)
  or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;
pod2usage(1) unless (scalar @ARGV == 1);

# Look for the required programs

Mizar::ensure_sensible_mizar_environment ();

if (! can_run ('xsltproc')) {
    print 'Error: xsltproc appears to be unavailable.';
    exit 1;
}

if (! can_run ('parallel')) {
    print 'Error: GNU parallel appears to be unavailable.';
    exit 1;
}

my $article_path = $ARGV[0];
my $article_basename = basename ($article_path, '.miz');
my $article_dirname = dirname ($article_path);
my $article_miz = "${article_dirname}/${article_basename}.miz";

if (! ensure_readable_file ($article_miz)) {
    croak ('Error: ', $article_miz, ' does not exist (as a file) or is not readable.');
}

my $article = Article->new (path => $article_miz);

Mizar::set_stylesheet_home ($stylesheet_home);

my $split_stylesheet = Mizar::path_for_stylesheet ('split');
my $itemize_stylesheet = Mizar::path_for_stylesheet ('itemize');
my $wsm_stylesheet = Mizar::path_for_stylesheet ('wsm');
my $extend_evl_stylesheet = Mizar::path_for_stylesheet ('extend-evl');
my $conditions_and_properties_stylesheet = Mizar::path_for_stylesheet ('conditions-and-properties');
my $trim_properties_and_conditions_stylesheet = Mizar::path_for_stylesheet ('trim-properties-and-conditions');

if (defined $target_directory) {
    if (-e $target_directory) {
	croak ('Error: the supplied target directory, ', $target_directory, ' already exists.  Please move it out of the way.', "\n");
    }
} else {
    my $cwd = cwd ();
    $target_directory = "${cwd}/${article_basename}";
    if (-e $target_directory) {
	croak ('Error: since the target-directory option was not used, we are to save our wok in \'', $article_basename, '\'; but there is already a directory by that name in the current working directory.  Please move it out of the way.', "\n");
    }
}

# Populate the directory with what we'll eventually need

my $local_db = LocalDatabase->new (location => $target_directory);

my $target_text_subdir = $local_db->get_text_subdir ();
my $target_prel_subdir = $local_db->get_prel_subdir ();

# Copy the article miz to the new subdirectory

my $article_in_target_dir = $article->copy (File::Spec->catdir ($target_directory));

my $article_evl_in_target_dir = $article_in_target_dir->file_with_extension ('evl');

# Transform the new miz
print 'Rewriting the text of ', $article_basename, ': ';

$article_in_target_dir->msmify ()
    or croak ('Error: ', $article_basename, ' cannot be MSMified.');

print 'done.', "\n";

my $article_wsx_in_target_dir
    = $article_in_target_dir->file_with_extension ('wsx');
my $article_split_wsx_in_target_dir
    = $article_in_target_dir->file_with_extension ('wsx.split');
my $article_itemized_wsx_in_target_dir
    = $article_in_target_dir->file_with_extension ('wsx.split.itemized');

ensure_readable_file ($article_wsx_in_target_dir);

print 'Split ', $article_basename, ': ';
apply_stylesheet ($split_stylesheet,
		  $article_wsx_in_target_dir,
		  $article_split_wsx_in_target_dir)
    or
    croak ('Error: xsltproc did not exit cleanly when applying the split stylesheet at ', $split_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");

print 'done.', "\n";

print 'Itemize ', $article_basename, ': ';
apply_stylesheet ($itemize_stylesheet,
		  $article_split_wsx_in_target_dir,
		  $article_itemized_wsx_in_target_dir)
    or croak ('Error: xsltproc did not exit cleanly when applying the itemize stylesheet at ', $itemize_stylesheet, ' to ', $article_split_wsx_in_target_dir, '.', "\n");

print 'done.', "\n";

# Load the article's environment

ensure_readable_file ($article_evl_in_target_dir);

my $xml_parser = XML::LibXML->new (suppress_warnings => 1,
				   suppress_errors => 1);

my @notations = $article_in_target_dir->notations ();
my @registrations = $article_in_target_dir->registrations ();
my @definitions = $article_in_target_dir->definitions ();
my @theorems = $article_in_target_dir->theorems ();
my @schemes = $article_in_target_dir->schemes ();
my @constructors = $article_in_target_dir->constructors ();

# Now print the items

my $itemized_article_doc = undef;

unless ($itemized_article_doc = eval { $xml_parser->parse_file ($article_itemized_wsx_in_target_dir) } ) {
  croak ('Error: the XML in ', $article_itemized_wsx_in_target_dir, ' is not well-formed.', "\n");
}

sub list_as_token_string {
  my @lst = @{shift ()};
  my $val = '';
  my $num_elements = scalar @lst;
  for (my $i = 0; $i < $num_elements; $i++) {
    $val .= $lst[$i];
    $val .= ',';
  }
  return $val;
}

sub fragment_number {
  my $fragment_path = shift;
  if ($fragment_path =~ m{ \A ckb ([0-9]+) ( $ | [.] ) }x) {
    my $fragment_number = $1;
    return $fragment_number;
  } else {
    croak ('Error: we could not extract the fragment number from the path \'', $fragment_path, '\'.', "\n");
  }
}

my @fragments = $itemized_article_doc->findnodes ('/Fragments/Text-Proper');

if ($verbose && scalar @fragments == 0) {
  print 'Warning: there are 0 Fragment elements in the itemized wsx file for ', $article_basename, ' at ', $article_itemized_wsx_in_target_dir, '.', "\n";
}

# Our upper bound: 999.  This is because we may potentially add
# 2-letter suffixes to the names of our fragments.  We use 'ckb' as
# the prefix, followed by a number and maybe a two-letter constructor
# property/correctness condition code, thus, 'ckb50' or 'ckb50ab'.  If
# there are more than 999 fragments, then we could potentially use
# names like 'ckb1000ab', which is too long for a Mizar article name
# (these have to be at most 8 characters long).
if (scalar @fragments > 999) {
  croak ('Error: because of limitations in Mizar, we cannot itemize articles with more than 999 fragments.');
}

# Separate the XML for the fragments into separate files

foreach my $i (1 .. scalar @fragments) {
  my $fragment = $fragments[$i - 1];
  my $fragment_doc = XML::LibXML::Document->createDocument ();
  $fragment->setAttribute ('original-article', $article_basename);
  $fragment->setAttribute ('fragment-number', $i);
  $fragment_doc->setDocumentElement ($fragment);
  my $fragment_path = "${target_directory}/fragment-${i}.wsx";
  $fragment_doc->toFile ($fragment_path);
}

print 'Generating ', scalar @fragments, ' Mizar fragments from ', $article_basename, ': ';

sub uc_mizar_name {
    my $name = shift;
    return uc strip_extension ($name);
}

foreach my $i (1 .. scalar @fragments) {

    print '.';

    my $fragment = $fragments[$i - 1];

    my $fragment_path = "${target_directory}/fragment-${i}.wsx";
    my $fragment_evl = "${target_directory}/fragment-${i}.evl";
    my $fragment_miz = "${target_text_subdir}/ckb${i}.miz";
    my $fragment_xml = "${target_text_subdir}/ckb${i}.xml";
    my $fragment_xml_orig = "${target_text_subdir}/ckb${i}.xml.orig";
    my $fragment_xml_exported = "${target_text_subdir}/ckb${i}.xml.exported";

    # Extend the evl of the initial article by inspecting the contents
    # of the prel subdirectory
    my @new_notations
	= map { uc_mizar_name ($_) } $local_db->notations_in_prel ();
    my @new_registrations
	= map { uc_mizar_name ($_) } $local_db->registrations_in_prel ();
    my @new_definitions
	= map { uc_mizar_name ($_) } $local_db->definitions_in_prel ();
    my @new_theorems
	= map { uc_mizar_name ($_) } $local_db->theorems_in_prel ();
    my @new_schemes
	= map { uc_mizar_name ($_) } $local_db->schemes_in_prel ();
    my @new_constructors =
	map { uc_mizar_name ($_) } $local_db->constructors_in_prel ();

    apply_stylesheet ($extend_evl_stylesheet,
		      $article_evl_in_target_dir,
		      $fragment_evl,
		      {
			  'notations' => list_as_token_string (\@new_notations),
			  'definitions' => list_as_token_string (\@new_definitions),
			  'theorems' => list_as_token_string (\@new_theorems),
			  'registrations' => list_as_token_string (\@new_registrations),
			  'constructors' => list_as_token_string (\@new_constructors),
			  'schemes' => list_as_token_string (\@new_schemes),
		      })
	or croak ('Error: xsltproc did not exit cleanly when applying the extend-evl stylesheet to ', $article_evl_in_target_dir, '.', "\n");

    # Now render the fragment's XML as a mizar article
    apply_stylesheet ($wsm_stylesheet,
		      $fragment_path,
		      $fragment_miz,
		      { 'evl' => $fragment_evl })
	or croak ('Error: xsltproc did not exit cleanly when applying the WSM stylesheet to ', $fragment_path, '.', "\n");

    # Now export and transfer the fragment

    my $fragment_basename = "ckb${i}";
    $local_db->accom ($fragment_basename)
	or croak ('Warning: accom did not exit cleanly when applied to fragment number ', $i, ' of ', $article_basename, '.');

    if ($local_db->verify ($fragment_basename)) {
	copy ($fragment_xml, $fragment_xml_orig)
	    or croak ('Error: unable to save a copy of ', $fragment_xml, ' to ', $fragment_xml_orig, ': ', $!);
	if ($local_db->export ($fragment_basename)) {
	    $local_db->transfer ($fragment_basename)
		or carp ('Warning: fragment number ', $i, ' of ', $article_basename, ' is verifiable and exportable, but there was a problem with the call to transfer.');
	} else {
	    carp ('Warning: fragment number ', $i, ' of ', $article_basename, ' is verifiable but not exportable.');
	}
    } else {
	carp ('Warning: fragment number ', $i, ' of ', $article_basename, ' is not verifiable.');
    }
}

print 'done.', "\n";

# Now extract all correctness conditions and properties

print 'Extracting patterns, constructors, properties, and correctness conditions from definitions of ', $article_basename, ': ';

my %conditions_and_properties_shortcuts
  = ('existence' => 'ex',
     'uniqueness' => 'un',
     'coherence' => 'ch',
     'correctness' => 'cr',
     'abstractness' => 'ab',
     'reflexivity' => 're',
     'irreflexivity' => 'ir',
     'symmetry' => 'sy',
     'asymmetry' => 'as',
     'connectedness' => 'cn',
     'involutiveness' => 'in',
     'projectivity' => 'pr',
     'idempotence' => 'id',
     'commutativity' => 'cm',
     'compatibility' => 'cp',
     'sethood' => 'se',
     'gpattern' => 'gp',
     'jpattern' => 'jp',
     'kpattern' => 'kp',
     'lpattern' => 'lp',
     'gconstructor' => 'gc',
     'jconstructor' => 'jc',
     'kconstructor' => 'kc',
     'lconstructor' => 'lc',
     'rpattern' => 'rp',
     'rconstructor' => 'rc',
     'mpattern' => 'mp',
     'mconstructor' => 'mc',
     'uconstructor' => 'uc',
     'vconstructor' => 'vc',
     'upattern' => 'up',
     'vpattern' => 'vp',
     'deftheorem' => 'dt',
     'kdefiniens' => 'kf',
     'mdefiniens' => 'mf',
     'rdefiniens' => 'rf',
     'vdefiniens' => 'vf');

sub copy_fragment_to_new_prefix {
  my $fragment_basename = shift;
  my $new_prefix = shift;
  my @old_files = glob "${target_text_subdir}/${fragment_basename}.*";

  foreach my $file (@old_files) {
    my $extension = extension ($file);
    my $new_path = "${target_text_subdir}/${new_prefix}.${extension}";
    copy ($file, $new_path)
      or croak ('Error: unable to copy ', $file, ' to ', $new_path, '.', "\n");
  }

  return 1;
}

my @fragment_files = $local_db->articles ();

sub write_properties_and_conditions {

    foreach my $fragment (@fragment_files) {

	print '.';

	my $fragment_number = fragment_number ($fragment);
	my $fragment_xml = "${target_text_subdir}/${fragment}.xml";

	my @correctness_conditions_and_properties
	    = apply_stylesheet ($conditions_and_properties_stylesheet,
				$fragment_xml);

	if ($debug) {
	    print {*STDERR} 'Fragment ', $fragment, ' contains ', scalar @correctness_conditions_and_properties, ' correctness conditions and properties:', "\n", join ("\n", @correctness_conditions_and_properties), "\n";
	}

	foreach my $cc_or_p (@correctness_conditions_and_properties) {
	    my $cc_or_p_code = $conditions_and_properties_shortcuts{$cc_or_p};

	    if (! defined $conditions_and_properties_shortcuts{$cc_or_p}) {
		croak ('Error: we are unable to find the short form of the correctness condition/constructor property \'', $cc_or_p, '\'.', "\n");
	    }

	    my $new_prefix = "ckb${fragment_number}${cc_or_p_code}";
	    copy_fragment_to_new_prefix ($fragment, $new_prefix);

	    my $new_miz = "${target_text_subdir}/${new_prefix}.miz";
	    my $new_err = "${target_text_subdir}/${new_prefix}.err";
	    my $new_xml = "${target_text_subdir}/${new_prefix}.xml";
	    my $new_xml_tmp = "${target_text_subdir}/${new_prefix}.xml.tmp";
	    ensure_readable_file ($new_miz);
	    ensure_readable_file ($new_err);
	    ensure_readable_file ($new_xml);

	    my $new_xml_orig = "${target_text_subdir}/${new_prefix}.xml.orig";

	    # Save a copy of the old XML
	    copy ($new_xml, $new_xml_orig)
		or croak ('Error: unable to save a copy of ', $new_xml, ' to ', $new_xml_orig, '.', "\n");

	    my $xsltproc_trim_status = system ("xsltproc --stringparam target-condition-or-property '${cc_or_p}' $trim_properties_and_conditions_stylesheet $new_xml > $new_xml_tmp");
	    my $xsltproc_trim_exit_code = $xsltproc_trim_status >> 8;
	    if ($xsltproc_trim_exit_code != 0) {
		croak ('Error: something went wrong trimming the property ', $cc_or_p, ' from ', $new_xml, '.', "\n");
	    }

	    move ($new_xml_tmp, $new_xml)
		or croak ('Error: error moving the temporary XML generated by strippping ', $cc_or_p, ' from fragment ', $fragment_number, '.', "\n");

	    if ($paranoid) {
		if ($verbose) {
		    print 'Checking that the result of trimming ', $cc_or_p, ' from fragment ', $fragment_number, ' is verifiable...';
		}
		my $verifier_status = system ("verifier -c -l -q -s $new_miz > /dev/null 2> /dev/null");
		my $verifier_exit_code = $verifier_status >> 8;
		if ($verifier_exit_code == 0 && -z $new_err) {
		    if ($verbose) {
			print 'OK.', "\n";
		    }
		} else {
		    croak ('Error: verifier rejected the trimming of ', $cc_or_p, ' from fragment ', $fragment_number, '; see ', $new_err, ' for details.', "\n");
		}
	    }

	    # Now that we've trimmed the XML, minimize to throw away any
	    # spurious toplevel stuff that we don't really need.
	}
    }
}

write_properties_and_conditions ();

print 'done.', "\n";

print 'Absolutizing the fragment XMLs...';

$local_db->absolutize ();

print 'done.', "\n";

__END__

=pod

=encoding utf8

=head1 NAME

itemize.pl - Divide a mizar article into fragments

=head1 USAGE

itemize.pl [options] mizar-article

=head1 REQUIRED ARGUMENTS

A Mizar article, as a path, must be supplied.

=head1 CONFIGURATION

This program requires that a working Mizar installation is available.
We require these Mizar programs:

=over 8

=item verifier

=item accom

=item msplit

=item mglue

=item exporter

=item transfer

=item wsmparser

=item msmprocessor

=back

The first six are a part of the standard distribution, but wsmparser
and msmprocessor are (as of 2012-01-03) not part of the standard
release.  Please contact the author or the mizar-forum mailing list
(L<mizar-forum@mizar.uwb.edu.pl|mailto:mizar-forum@mizar.uwb.edu.pl>)
to obtain suitable versions of wsmparser and msmprocessor.

=head1 ENVIRONMENT

The MIZFILES environment variable needs to be set to a sensible value.

=head1 DEPENDENCIES

=over 8

=item B<Perl dependencies>

=over 8

=item Cwd

=item File::Basename

=item File::Copy qw(copy);

=item Getopt::Long

=item Pod::Usage

=item XML::LibXML

=back

=item B<XSL dependencies>

This package requires that xsltproc be available.  These stylesheets,
in addition, are required:

=over 8

=item F<split.xsl>

=item F<itemize.xsl>

=item F<wsm.xsl>

=item F<extend-evl.xsl>

=back

If you do not have these stylesheets, see
L<the github page for this program and related Mizar code|https://github.com/jessealama/xsl4mizar/tree/master/items/>
to obtain the latest versions of them.  Use the --stylesheet-home option to
specify the directory in which to look for these needed stylesheets.

=back

=head1 OPTIONS

=over 8

=item B<--help>

Prints a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--verbose>

Say what we're doing.

=item B<--paranoid>

After itemizing, verify all of the article's fragments.

=item B<--target-directory=DIR>

Save our work in the specified directory.  It is an error if the
specified directory already exists.

=item B<--stylesheet-home=DIR>

The directory in which we will look for any needed stylesheets.

=back

=head1 DESCRIPTION

B<itemize.pl> will divide a mizar article into fragments.  The
fragments will be stored in the directory indicated by the
--target-directory option.  If that option is not supplied, we will
attempt to create, in the working directory in which this script is
called, a directory whose name is the basename of the supplied
article.  It is an error if this directory, or the directory supplied
by the --target-directory option, already exists.

=head1 EXIT STATUS

0 if everything went OK, 1 if any error occurred.  No other exit codes
are used.

=head1 DIAGNOSTICS

Before doing anything, we inspect the MIZFILES environment variable,
and that certain needed Mizar programs are available.  If MIZFILES is
not set, or set to a strange value, we will terminate.  If any of the
needed Mizar programs is not found, we will terminate.

We also check whether the xsltproc XSLT processor is available.  If it
is unavailable, we cannot proceed.

This program, when run with no options on a Mizar article that can be
entirely itemized, will generate no output.  Before itemizing the
article, it is copied to a new directory (whose creation can fail).

The article is then rewritten using various transformations by the
xsltproc XLST processor.  If any of these transformations fails, this
program will terminate and one will learn that some step (which
involves the Mizar accom, verifier, msplit, mglue, wsmparser, and
msmprocessor programs, as well as a handful of XSL stylesheets) has
failed.  At present we do not pass along whatever error messages
xsltproc generated, nor do we explain any Mizar error files.  If a
Mizar program fails, you can see for yourself how it failed by
consulting the .err file corresponding to the failing .err file; see
also the file F<mizar.msg> under the MIZFILES environment variable.

=head1 BUGS AND LIMITATIONS

Sending a signal to this program when it is writing out the fragments
of the initial article and running accom/verifier/exporter/transfer
should probably kill the whole process.  Instead, it kills
accom/verifier/exporter/transfer, and itemization continues in an
incoherent state.

There are some opportunities for parallelization of the itemization
process, but at the moment we are not exploiting these.

=head1 SEE ALSO

=over 8

=item F<itemized-article-dependencies.pl>

=item L<http://mizar.org/>

=back

=head1 INCOMPATIBILITIES

None known.

=head1 AUTHOR

Jesse Alama <jesse.alama@gmail.com>

=head1 LICENSE AND COPYRIGHT

This source is offered under the terms of
L<the GNU GPL version 3|http://www.gnu.org/licenses/gpl-3.0.en.html>.

=cut
