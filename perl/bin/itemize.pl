#!/usr/bin/perl

# Check first that our environment is sensible

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw(tempfile);
use File::Basename qw(basename dirname);
use XML::LibXML;
use Cwd qw(cwd);
use File::Copy qw(copy move);
use Carp qw(croak);

use lib '/Users/alama/sources/mizar/mizar-items/perl/lib';
use Utils qw(ensure_directory ensure_readable_file);
use Mizar;

my $paranoid = 0;
my $stylesheet_home = '/Users/alama/sources/mizar/xsl4mizar/items';
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

unless (defined $ENV{"MIZFILES"}) {
  croak ('Error: the MIZFILES environment variable is not set.');
}

my $mizfiles = $ENV{"MIZFILES"};

ensure_directory ($mizfiles);

# Look for the required programs

my @mizar_programs = ('verifier',
		      'accom',
		      'exporter',
		      'transfer',
		      'msmprocessor',
		      'wsmparser',
		      'msplit',
		      'mglue',
		      'xsltproc');

foreach my $program (@mizar_programs) {
  my $which_status = system ("which $program > /dev/null 2>&1");
  my $which_exit_code = $which_status >> 8;
  if ($which_exit_code != 0) {
    croak ('Error: the required program ', $program, ' cannot be found (or is not executable)', "\n");
  }
}

my $article = $ARGV[0];
my $article_basename = basename ($article, '.miz');
my $article_dirname = dirname ($article);
my $article_sans_extension = "${article_dirname}/${article_basename}";
my $article_miz = "${article_dirname}/${article_basename}.miz";
my $article_err = "${article_dirname}/${article_basename}.err";
my $article_evl = "${article_dirname}/${article_basename}.evl";

ensure_readable_file ($article_miz);
ensure_directory ($stylesheet_home);

my $absrefs_stylesheet = Mizar::path_for_stylesheet ('addabsrefs');
my $rewrite_aid_stylesheet = Mizar::path_for_stylesheet ('rewrite-aid');
my $split_stylesheet = Mizar::path_for_stylesheet ('split');
my $itemize_stylesheet = Mizar::path_for_stylesheet ('itemize');
my $wsm_stylesheet = Mizar::path_for_stylesheet ('wsm');
my $extend_evl_stylesheet = Mizar::path_for_stylesheet ('extend-evl');
my $conditions_and_properties_stylesheet = Mizar::path_for_stylesheet ('conditions-and-properties');
my $trim_properties_and_conditions_stylesheet = Mizar::path_for_stylesheet ('trim-properties-and-conditions');

if (defined $target_directory) {
  if (-e $target_directory) {
    croak ('Error: the supplied target directory, ', $target_directory, ' already exists.  Please move it out of the way.', "\n");
  } else {
    mkdir $target_directory
      or croak ('Error: unable to make the directory \'', $target_directory, '\'.', "\n");
  }
} else {
  my $cwd = cwd ();
  $target_directory = "${cwd}/${article_basename}";
  if (-e $target_directory) {
    croak ('Error: since the target-directory option was not used, we are to save our wok in \'', $article_basename, '\'; but there is already a directory by that name in the current working directory.  Please move it out of the way.', "\n");
  }
  mkdir $target_directory
    or croak ('Error: unable to make the directory \'', $article_basename, '\' in the current working directory.', "\n");
}

# Populate the directory with what we'll eventually need

foreach my $subdir_basename ('dict', 'prel', 'text') {
  my $subdir = "${target_directory}/${subdir_basename}";
  if (-e $subdir) {
    croak ('Error: there is already a \'', $subdir_basename, '\' subdirectory of the target directory ', $target_directory, '.', "\n");
  }
  mkdir $subdir
    or croak ('Error: unable to make the \'', $subdir_basename, '\' subdirectory of ', $target_directory, '.', "\n");
}

my $target_dict_subdir = "${target_directory}/dict";
my $target_text_subdir = "${target_directory}/text";
my $target_prel_subdir = "${target_directory}/prel";

# Copy the article miz to the new subdirectory

my $article_miz_in_target_dir = "${target_directory}/${article_basename}.miz";
my $article_miz_orig_in_target_dir = "${target_directory}/${article_basename}.miz.orig";
my $article_err_in_target_dir = "${target_directory}/${article_basename}.err";

copy ($article_miz, $article_miz_in_target_dir)
  or croak ('Error: unable to copy the article at ', $article_miz, ' to ', $article_miz_in_target_dir, '.', "\n");
copy ($article_miz, $article_miz_orig_in_target_dir)
  or croak ('Error: unable to copy the article at ', $article_miz, ' to ', $article_miz_orig_in_target_dir, '.', "\n");

# Transform the new miz

sub run_mizar_tool {
  my $tool = shift;
  my $article_file = shift;
  my $article_base = basename ($article_file, '.miz');
  my $article_err_file = "${article_base}.err";
  my $tool_status = system ("$tool -l -q $article_file > /dev/null 2>&1");
  my $tool_exit_code = $tool_status >> 8;
  if ($tool_exit_code != 0 && -s $article_err_file) {
    if ($verbose) {
      print 'Error: the ', $tool, ' Mizar tool did not exit cleanly when applied to ', $article_file, ' (or the .err file is non-empty).', "\n";
    }
    return 0;
  }
  return 1;
}

my $article_evl_in_target_dir = "${target_directory}/${article_basename}.evl";
my $article_msm_in_target_dir = "${target_directory}/${article_basename}.msm";
my $article_tpr_in_target_dir = "${target_directory}/${article_basename}.tpr";

print 'Rewriting the text of ', $article_basename, ': ';

my $accom_ok = run_mizar_tool ('accom', $article_miz_in_target_dir);
if ($accom_ok == 0) {
  croak ('Error: the initial article did not could not be accom\'d.');
}

print '.';

my $verifier_ok = run_mizar_tool ('verifier', $article_miz_in_target_dir);
if ($verifier_ok == 0) {
  croak ('Error: the initial article could not be verified.');
}

print '.';

my $wsmparser_ok = run_mizar_tool ('wsmparser', $article_miz_in_target_dir);
if ($wsmparser_ok == 0) {
  croak ('Error: wsmparser failed on the initial article.');
}

print '.';

my $msmprocessor_ok = run_mizar_tool ('msmprocessor', $article_miz_in_target_dir);
if ($msmprocessor_ok == 0) {
  croak ('Error: msmprocessor failed on the initial article.');
}

print '.';

my $msplit_ok = run_mizar_tool ('msplit', $article_miz_in_target_dir);
if ($msplit_ok == 0) {
  croak ('Error: msplit failed on the initial article.');
}

print '.';

copy ($article_msm_in_target_dir, $article_tpr_in_target_dir)
  or croak ('Error: we are unable to copy ', $article_msm_in_target_dir, ' to ', $article_tpr_in_target_dir, '.', "\n");

print '.';

my $mglue_ok = run_mizar_tool ('mglue', $article_miz_in_target_dir);
if ($mglue_ok == 0) {
  croak ('Error: mglue failed on the initial article.');
}

print '.';

$wsmparser_ok = run_mizar_tool ('wsmparser', $article_miz_in_target_dir);
if ($wsmparser_ok == 0) {
  croak ('Error: wsmparser failed on the WSMified article.');
}

print 'done.', "\n";

my $article_wsx_in_target_dir = "${target_directory}/${article_basename}.wsx";
my $article_split_wsx_in_target_dir = "${article_wsx_in_target_dir}.split";
my $article_itemized_wsx_in_target_dir = "${article_split_wsx_in_target_dir}.itemized";

ensure_readable_file ($article_wsx_in_target_dir);

print 'Split ', $article_basename, ': ';
my $xsltproc_split_status = system ("xsltproc --output $article_split_wsx_in_target_dir $split_stylesheet $article_wsx_in_target_dir 2>/dev/null");

my $xsltproc_split_exit_code = $xsltproc_split_status >> 8;

if ($xsltproc_split_exit_code != 0) {
  croak ('Error: xsltproc did not exit cleanly when applying the split stylesheet at ', $split_stylesheet, ' to ', $article_wsx_in_target_dir, '.', "\n");
}

ensure_readable_file ($article_split_wsx_in_target_dir);

print 'done.', "\n";

print 'Itemize ', $article_basename, ': ';
my $xsltproc_itemize_status = system ("xsltproc --output $article_itemized_wsx_in_target_dir $itemize_stylesheet $article_split_wsx_in_target_dir 2>/dev/null");

my $xsltproc_itemize_exit_code = $xsltproc_itemize_status >> 8;
if ($xsltproc_itemize_exit_code != 0) {
  croak ('Error: xsltproc did not exit cleanly when applying the itemize stylesheet at ', $itemize_stylesheet, ' to ', $article_split_wsx_in_target_dir, '.', "\n");
}

print 'done.', "\n";

# Load the article's environment

ensure_readable_file ($article_evl_in_target_dir);

my $xml_parser = XML::LibXML->new (suppress_warnings => 1,
				   suppress_errors => 1);

my $article_evl_doc = undef;
unless ($article_evl_doc = eval { $xml_parser->parse_file ($article_evl_in_target_dir)  } ) {
  croak ('Error: ', $article_evl_in_target_dir, ' is not well-formed XML.', "\n");
}

sub ident_name {
  my $ident_node = shift;
  return ($ident_node->getAttribute ('name'));
}

my @notations_nodes
  = $article_evl_doc->findnodes ('Environ/Directive[@name = "Notations"]/Ident[@name]');
my @notations = map { ident_name($_) } @notations_nodes;
my @registrations_nodes
  = $article_evl_doc->findnodes ('Environ/Directive[@name = "Registrations"]/Ident[@name]');
my @registrations = map { ident_name($_) } @registrations_nodes;
my @definitions_nodes
  = $article_evl_doc->findnodes ('Environ/Directive[@name = "Definitions"]/Ident[@name]');
my @definitions = map { ident_name($_) } @definitions_nodes;
my @theorems_nodes
  = $article_evl_doc->findnodes ('Environ/Directive[@name = "Theorems"]/Ident[@name]');
my @theorems = map { ident_name($_) } @theorems_nodes;
my @schemes_nodes
  = $article_evl_doc->findnodes ('Environ/Directive[@name = "Schemes"]/Ident[@name]');
my @schemes = map { ident_name($_) } @schemes_nodes;
my @constructors_nodes
  = $article_evl_doc->findnodes ('Environ/Directive[@name = "Constructors"]/Ident[@name]');
my @constructors = map { ident_name($_) } @constructors_nodes;

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

chdir $target_directory
  or croak ('Error: unable to change directory to ', $target_directory, '.', "\n");

print 'Generating ', scalar @fragments, ' Mizar fragments from ', $article_basename, ': ';

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
  opendir (PREL_DIR, $target_prel_subdir)
    or croak ('Error: unable to open the directory at ', $target_prel_subdir, '.', "\n");
  my @prel_files = readdir (PREL_DIR);
  closedir PREL_DIR
    or croak ('Error: unable to close the directory filehandle for ', $target_prel_subdir, '.', "\n");

  my @new_notations = ();
  my @new_registrations = ();
  my @new_definitions = ();
  my @new_theorems = ();
  my @new_schemes = ();
  my @new_constructors = ();

  foreach my $prel_file (@prel_files) {
    my $prel_path = "${target_prel_subdir}/${prel_file}";
    if (-f $prel_path) {
      my $fragment_number = fragment_number ($prel_file);
      my $fragment_article_name_uc = 'CKB' . $fragment_number;
      if ($prel_file =~ / [.]dno \z /x) {
	push (@new_notations, $fragment_article_name_uc);
      }
      if ($prel_file =~ / [.]drd \z /x) {
	push (@new_registrations, $fragment_article_name_uc);
      }
      if ($prel_file =~ / [.]dcl \z /x) {
	push (@new_registrations, $fragment_article_name_uc)
      }
      if ($prel_file =~ / [.]eid \z /x) {
	push (@new_registrations, $fragment_article_name_uc)
      }
      if ($prel_file =~ / [.]did \z /x) {
	push (@new_registrations, $fragment_article_name_uc)
      }
      if ($prel_file =~ / [.]sch \z /x) {
	push (@new_schemes, $fragment_article_name_uc)
      }
      if ($prel_file =~ / [.]dco \z /x) {
	push (@new_constructors, $fragment_article_name_uc)
      }
      if ($prel_file =~ / [.]def \z /x) {
	push (@new_definitions, $fragment_article_name_uc)
      }
      if ($prel_file =~ / [.]the \z /x) {
	push (@new_theorems, $fragment_article_name_uc)
      }
    }
  }

  my $all_notations_token_string = list_as_token_string (\@new_notations);
  my $all_definitions_token_string = list_as_token_string (\@new_definitions);
  my $all_theorems_token_string = list_as_token_string (\@new_theorems);
  my $all_registrations_token_string = list_as_token_string (\@new_registrations);
  my $all_constructors_token_string = list_as_token_string (\@new_constructors);
  my $all_schemes_token_string = list_as_token_string (\@new_schemes);

  my $xsltproc_extend_evl_status
    = system ("xsltproc --output $fragment_evl --stringparam notations '$all_notations_token_string' --stringparam definitions '$all_definitions_token_string' --stringparam theorems '$all_theorems_token_string' --stringparam registrations '$all_registrations_token_string' --stringparam constructors '$all_constructors_token_string' --stringparam schemes '$all_schemes_token_string' $extend_evl_stylesheet $article_evl_in_target_dir 2>/dev/null");
  my $xsltproc_extend_evl_exit_code = $xsltproc_extend_evl_status >> 8;
  if ($xsltproc_extend_evl_exit_code != 0) {
    croak ('Error: xsltproc did not exit cleanly when applying the extend-evl stylesheet to ', $article_evl_in_target_dir, '.', "\n");
  }

  # Now render the fragment's XML as a mizar article
  my $xsltproc_wsm_status
    = system ("xsltproc --output $fragment_miz --stringparam evl '$fragment_evl' $wsm_stylesheet $fragment_path 2>/dev/null");
  my $xsltproc_wsm_exit_code = $xsltproc_wsm_status >> 8;
  if ($xsltproc_wsm_exit_code != 0) {
    croak ('Error: xsltproc did not exit cleanly when applying the WSM stylesheet to ', $fragment_path, '.', "\n");
  }

  # Now export and transfer the fragment

  if (run_mizar_tool ('accom', $fragment_miz)) {
    if (run_mizar_tool ('verifier', $fragment_miz)) {
      # Save a copy of the XML -- exporter can modify it!
      copy ($fragment_xml, $fragment_xml_orig)
	or croak ('Error: unable to save a copy of ', $fragment_xml, ' to ', $fragment_xml_orig, '.', "\n");
      if (run_mizar_tool ('exporter', $fragment_miz)) {
	copy ($fragment_xml, $fragment_xml_exported)
	  or croak ('Error: unable to save a copy of the exported XML ', $fragment_xml, ' to ', $fragment_xml_exported, '.', "\n");
	if (run_mizar_tool ('transfer', $fragment_miz)) {
	  # nothing to do
	} else {
	  if ($verbose) {
	    print 'Warning: fragment number ', $i, ' of ', $article_basename, ' is not transferrable.', "\n";
	  }
	}
      } else {
	if ($verbose) {
	  print 'Warning: fragment number ', $i, ' of ', $article_basename, ' is not expotable.', "\n";
	}
      }
      copy ($fragment_xml_orig, $fragment_xml)
	or croak ('Error: unable to restore the pre-exporter version of the XML at ', $fragment_xml_orig, ' to ', $fragment_xml, '.', "\n");
    } else {
      if ($verbose) {
	print 'Warning: verifier did not exit cleanly when applied to fragment number ', $i, ' of ', $article_basename, '.', "\n";
      }
    }
  } else {
    if ($verbose) {
      print 'Warning: accom did not exit cleanly when applied to fragment number ', $i, ' of ', $article_basename, '.', "\n";
    }
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

sub extension {
  my $path = shift;
  if ($path =~ /[.]([^.]+)$/) {
    return $1;
  } else {
    croak ('Error: the path \'', $path, '\' does not have an extension.');
  }
}

sub copy_fragment_to_new_prefix {
  my $fragment_basename = shift;
  my $new_prefix = shift;
  my @old_files = glob "${target_text_subdir}/${fragment_basename}.*";
  # if ($debug) {
  #   print {*STDERR} 'Files matching the name \'', $fragment_basename, '\':';
  #   foreach my $file (@old_files) {
  #     print {*STDERR} $file, "\n";
  #    }
  # }
  foreach my $file (@old_files) {
    my $extension = extension ($file);
    my $new_path = "${target_text_subdir}/${new_prefix}.${extension}";
    copy ($file, $new_path)
      or croak ('Error: unable to copy ', $file, ' to ', $new_path, '.', "\n");
  }
  return 1;
}

my @fragment_files = glob "${target_text_subdir}/ckb[0-9]*.miz";
chomp @fragment_files;
foreach my $fragment (@fragment_files) {

  print '.';

  my $fragment_basename = basename ($fragment, '.miz');
  my $fragment_number = fragment_number ($fragment_basename);
  my $fragment_xml = "${target_text_subdir}/${fragment_basename}.xml";

  ensure_readable_file ($fragment_xml);

  my @correctness_conditions_and_properties
      = apply_stylesheet ($conditions_and_properties_stylesheet,
			  $fragment_xml);

  if ($debug) {
    print {*STDERR} 'Fragment ', $fragment_basename, ' contains ', scalar @correctness_conditions_and_properties, ' correctness conditions and properties:', "\n", join ("\n", @correctness_conditions_and_properties), "\n";
  }

  foreach my $cc_or_p (@correctness_conditions_and_properties) {
    my $cc_or_p_code = $conditions_and_properties_shortcuts{$cc_or_p};

    if (! defined $conditions_and_properties_shortcuts{$cc_or_p}) {
      croak ('Error: we are unable to find the short form of the correctness condition/constructor property \'', $cc_or_p, '\'.', "\n");
    }

    my $new_prefix = "ckb${fragment_number}${cc_or_p_code}";
    copy_fragment_to_new_prefix ($fragment_basename, $new_prefix);

    my $new_miz = "${target_text_subdir}/${new_prefix}.miz";
    my $new_err = "${target_text_subdir}/${new_prefix}.err";
    my $new_xml = "${target_text_subdir}/${new_prefix}.xml";
    my $new_xml_tmp = "${target_text_subdir}/${new_prefix}.xml.tmp";
    ensure_readable_file ($new_miz);
    ensure_readable_file ($new_err);
    ensure_readable_file ($new_xml);

    # # We need to rewrite the aid of the article; otherwise the external
    # # dependencies stylesheet will sniff through the original whole article
    # my $xsltproc_rewrite_aid_status = system ("xsltproc --stringparam new-aid '${new_prefix}' $rewrite_aid_stylesheet $new_xml > $new_xml_tmp 2> /dev/null");
    # my $xsltproc_rewrite_aid_exit_code = $xsltproc_rewrite_aid_status >> 8;
    # if ($xsltproc_rewrite_aid_status != 0) {
    #   if ($verbose) {
    # 	print "\n";
    #   }
    #   croak ('Error: xsltproc did not exit cleanly applying aid-rewriting stylesheet to ', $new_xml, '.');
    # }
    # move ($new_xml_tmp, $new_xml)
    #   or croak ('Error: unable to rename ', $new_xml_tmp, ' to ', $new_xml, '.');

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

print 'done.', "\n";

print 'Absolutizing the fragment XMLs...';

my @fragment_xmls = glob "${target_text_subdir}/ckb[0-9]*.xml";
my $fragment_xmls_with_space = join (' ', @fragment_xmls);
my $parallel_status = system ("parallel 'xsltproc --output {.}.xml1 ${absrefs_stylesheet} {} 2> /dev/null' ::: ${fragment_xmls_with_space}");
my $parallel_exit_code = $parallel_status >> 8;
if ($parallel_exit_code != 0) {
    croak ('Error: parallel did not exit cleanly when absolutizing the fragment XMLs for ', $article_basename, '; its exit code was ', $parallel_exit_code, '.', "\n");
}

# Need to rewrite -- again!  addabsrefs can smash my precious bogus aid's

# my @fragment_abs_xmls = glob "${target_text_subdir}/ckb[0-9]*.xml1";
# foreach my $fragment_abs_xml (@fragment_abs_xmls) {
#     my $fragment_basename = basename ($fragment_abs_xml, '.xml1');
#     my $fragment_tmp = "${fragment_abs_xml}2";
#     system ("xsltproc --stringparam new-aid '${fragment_basename}' $rewrite_aid_stylesheet $fragment_abs_xml > $fragment_tmp");
#     move ($fragment_tmp, $fragment_abs_xml);
# }

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
