#!/usr/bin/perl -w

use Getopt::Euclid; # load this first to set up our command-line parser

use Cwd qw / getcwd /;
use File::Temp qw / tempdir /;
use File::Spec::Functions qw / catfile catdir /;
use File::Copy qw / copy move /;
use File::Path qw / remove_tree /;
use XML::LibXML;
use Fatal qw / open /;
use List::MoreUtils qw / all /;

######################################################################
### Process the command line
###
###
### We are using Getopt::Euclid; see the documentation section at the
### end of this file to see what command-line options are available.
######################################################################

### --verbose

## Process --verbose first, because if it is set, then we want to
## print stuff out as we process the rest of the command-line
## arguments.
my $be_verbose = defined $ARGV{'--verbose'} ? 1 : 0;

### --debug
my $debug = defined $ARGV{'--debug'} ? 1 : 0;

## --mizfiles next, because some of the other options use this value.

my $mizfiles = defined $ARGV{'--mizfiles'} ? $ARGV{'--mizfiles'}
                                           : $ENV{'MIZFILES'};

# Sanity check
unless (defined $mizfiles) {
  die 'Error: The --mizfiles option was not used, so we looked for MIZFILES in he current environment;\nbut that is likewise unset, so we cannot process any mizar texts';
}

### --result-dir

# First, extract a value.  The default is to use the current directory.
my $result_dir = $ARGV{'--result-dir'};
unless (defined $result_dir) {
  $result_dir = getcwd ();
}

# Ensure that the value is sensible, which in this case means: it
# exists, it's a directory, and it's writable
unless (-e $result_dir) {
  die "Error: The given result directory\n\n  $result_dir\n\ndoes not exist!";
}
unless (-d $result_dir) {
  die "Error: The given result directory\n\n$result_dir\n\nis not actually a directory!";
}
unless (-w $result_dir) {
  die "Error: The given result directory\n\n$result_dir\n\nis not writable!";
}

if ($be_verbose) {
  print "Setting the result directory to '$result_dir'\n";
}

### --with-stylesheets-in

# First, extract or assign a value.
my $stylesheet_dir = $ARGV{'--with-stylesheets-in'};
if ($be_verbose) {
  print "Looking for stylesheets in directory '$stylesheet_dir'\n";
}

# Now ensure that the value is sensible, which in this case means that
# the path refers to an existing, readable directory and that that
# directory contains all the relevant stylesheets (and that these are
# all readable)
unless (-e $stylesheet_dir) {
  die "Error: the stylesheet directory '$stylesheet_dir' does not exist!";
}
unless (-d $stylesheet_dir) {
  die "Error: the stylesheet directory '$stylesheet_dir' is not actually a directory!";
}
unless (-r $stylesheet_dir) {
  die "Error: the stylesheet directory '$stylesheet_dir' is not readable!";
}

# We've established that the stylesheet directory is reachable.  Now
# check that all the required stylesheets are reachable in that
# directory.
my @stylesheets = ('addabsrefs');
foreach my $stylesheet (@stylesheets) {
  my $stylesheet_xsl = "$stylesheet.xsl";
  my $stylesheet_path = catfile ($stylesheet_dir, $stylesheet_xsl);
  unless (-e $stylesheet_path) {
    die "The required stylesheet $stylesheet_xsl does not exist in $stylesheet_dir";
  }
  unless (-r $stylesheet_path) {
    die "The required stylesheet $stylesheet_xsl, under $stylesheet_dir, is not readable";
  }
}

### ARTICLE

# First, extract a value of the single required ARTICLE
# argument.
my $article_name = $ARGV{'<ARTICLE>'};
unless (defined $article_name) { # weird: my typo or bug in Getopt::Euclid
  die 'Error: The mandatory ARTICLE argument was somehow omitted!';
}

# Strip the final ".miz", if there is one
my $article_name_len = length $article_name;
if ($article_name =~ /\.miz$/) {
  $article_name = substr $article_name, 0, $article_name_len - 4;
}

if ($be_verbose) {
  print "Working with article '$article_name'\n";
}

### --article-source-dir

# First, extract a value
my $article_source_dir = $ARGV{'--article-source-dir'};
if (defined $article_source_dir) {
  if ($be_verbose) {
    print "Setting the article source directory to '$article_source_dir', as requested\n";
  }
} else {
  $article_source_dir = "$mizfiles/mml";
  if ($be_verbose) {
    print "Setting the article source directory to '$article_source_dir' (which is the default)\n";
  }
}

# Now ensure that this value for is sensible, which in this case
# means: it exists, it's directory, and it's readable.
unless (-e $article_source_dir) {
  die "Error: The given article source directory\n\n  $article_source_dir\n\ndoes not exist!";
}
unless (-d $article_source_dir) {
  die "Error: The given article source directory\n\n$article_source_dir\n\nis not actually a directory!";
}
unless (-r $article_source_dir) {
  die "Error: The given article source directory\n\n$article_source_dir\n\nis not readable!";
}

# Some common extensions we'll be using, and their paths
my $article_miz = $article_name . '.miz';
my $article_err = $article_name . '.err'; # for error checking with the mizar tools
my $article_tmp = $article_name . '.$-$';
my $article_evl = $article_name . '.evl';
my $article_miz_path = catfile ($article_source_dir, $article_miz);
my $article_err_path = catfile ($article_source_dir, $article_err);
my $article_tmp_path = catfile ($article_source_dir, $article_tmp);
my $article_evl_path = catfile ($article_source_dir, $article_evl);

# More sanity checks: the mizar file exists and is readable
unless (-e $article_miz_path) {
  die "Error: No file named\n\n  $article_miz\n\nunder the source directory\n\n  $article_source_dir";
}
unless (-r $article_miz_path) {
  die "Error: The file\n\n  $article_miz\n\under the source directory\n\n  $article_source_dir\n\nis not readable";
}

### --no-cleanup
my $cleanup_afterward = 1;
if (defined $ARGV{'--no-cleanup'}) {
  $cleanup_afterward = 0;
}

### --with-verifier
my $verifier;
if (defined $ARGV{'--with-verifier'}) {
  $verifier = $ARGV{'--with-verifier'};
} else {
  my $which_verifier = `which "verifier"`;
  unless ($? == 0) {
    die 'Unable to find the verifier!  which died, somehow';
  }
  chomp $which_verifier;
  $verifier = $which_verifier;
}

# Sanity check: the verifier needs to exist as a file and be executable
unless (-e $verifier) {
  die "There is no verifier binary at '$verifier'!";
}
if (-d $verifier) {
  die "The path '$verifier' to the verifier isn't a file, but a directory!";
}
unless (-x $verifier) {
  die "The verifier at '$verifier' is not executable!";
}

### --with-makeenv
my $makeenv;
if (defined $ARGV{'--with-makeenv'}) {
  $makeenv = $ARGV{'--with-makeenv'};
} else {
  my $which_makeenv = `which "makeenv"`;
  unless ($? == 0) {
    die 'Unable to find the makeenv!  which died, somehow';
  }
  chomp $which_makeenv;
  $makeenv = $which_makeenv;
}

# Sanity check: the makeenv needs to exist as a file and be executable
unless (-e $makeenv) {
  die "There is no makeenv binary at '$makeenv'!";
}
if (-d $makeenv) {
  die "The path '$makeenv' to the makeenv isn't a file, but a directory!";
}
unless (-x $makeenv) {
  die "The makeenv at '$makeenv' is not executable!";
}

######################################################################
### End command-line processing.
###
### Now we can start doing something.
######################################################################

######################################################################
### Prepare result directories:
###
### 1. The work directory, where sed, dellink, JA1, etc., will be run.
###
### 2. The local article database.
######################################################################

### 1. Prepare the work directory.

# First, create it.
my $workdir = tempdir (CLEANUP => $cleanup_afterward)
  or die 'Error: Unable to create a working directory!';

if ($be_verbose) {
  print "Setting the work directory to $workdir\n";
}

# Now copy the specified mizar article to the work directory
my $article_in_workdir = catfile ($workdir, $article_miz);
copy ($article_miz_path, $article_in_workdir)
  or die "Error: Unable to copy article ($article_miz) to work directory ($article_in_workdir):\n\n$!";

### 2. Prepare the result directory

## But first check whether it already exists.  If it does, stop; we
## don't want to potentially overwrite anything.
my $local_db_in_workdir = catdir ($workdir, $article_name);
my $local_db_in_resultdir = catdir ($result_dir, $article_name);

if (-x $local_db_in_resultdir) {
  die "Error: there is already a directory called '$article_name' in the result directory ($result_dir); refusing to overwrite its contents";
}

if ($be_verbose) {
  print "Article fragments will be stored in '$local_db_in_workdir'\n";
}

mkdir $local_db_in_workdir
  or die "Error: Unable to make the local database directory: $!";
my @local_db_subdirs = ('dict', 'prel', 'text');
my $article_text_dir = catdir ($local_db_in_workdir, 'text');
my $article_prel_dir = catdir ($local_db_in_workdir, 'prel');

foreach my $local_db_in_workdir_subdir (@local_db_subdirs) {
  my $local_db_in_workdir_path = catfile ($local_db_in_workdir, $local_db_in_workdir_subdir);
  mkdir $local_db_in_workdir_path
    or die "Error: Unable to make local database subdirectory $local_db_in_workdir_subdir: $!";
}

######################################################################
### Prepare article for itemization:
###
### 1. Run the accomodator (needed for JA1)
###
### 2. Run JA1, edtfile, overwrite the non-JA1'd .miz, dellink,
###    edtfile, overwrite the non-dellink'd .miz, and load the result
###
### 3. Verify (and generate article XML)
###
### 4. Generate the absolute reference version of the generated XML
###
### 5. Load the .idx file
######################################################################

### 1. Run the accomodator
chdir $workdir;
system ("accom -q -s -l $article_miz > /dev/null 2> /dev/null") == 0
  or die "Error: Something went wrong when calling the accomodator on $article_name: the error was\n\n$!";
if (-s $article_err) {
  die "Error: although the accomodator returned successfully, it nonetheless generated a non-empty error file";
}


### 2. Run JA1, edtfile, overwrite the non-JA1'd .miz, and load it

# JA1
system ("JA1 -q -s -l $article_miz > /dev/null 2> /dev/null") == 0
  or die "Error: Something went wrong when calling JA1 on $article_name: the error was\n\n$!";
if (-s $article_err) {
  die "Error: although the JA1 tool returned successfully, it nonetheless generated a non-empty error file";
}

# edtfile
system ("edtfile $article_name > /dev/null 2> /dev/null") == 0
  or die "Error: Something went wrong during the call to edtfile on $article_name:\n\n  $!";
if (-s $article_err) {
  die "Error: although the edtfile tool returned successfully, it nonetheless generated a non-empty error file";
}

# sanity check
unless (-e $article_tmp) {
  die "Error: the edtfile tool did not generate the expected file '$article_tmp'";
}
unless (-r $article_tmp) {
  die "Error: the file generated by the edtfile tool, '$article_tmp', is not readable";
}

# rename
move ($article_tmp, $article_miz) == 1
  or die "Error: unable to rename the temporary file\n\n  $article_tmp\n\nto\n\n  $article_miz\n\nin the work directory\n\n  $workdir .\n\nThe error was\n\n  $!";

# dellink
system ("dellink -q -s -l $article_miz > /dev/null 2> /dev/null") == 0
  or die "Error: Something went wrong when calling dellink on $article_name: the error was\n\n$!";
if (-s $article_err) {
  die "Error: although the dellink tool returned successfully, it nonetheless generated a non-empty error file";
}

# edtfile
system ("edtfile $article_name > /dev/null 2> /dev/null") == 0
  or die "Error: Something went wrong during the call to edtfile on $article_name:\n\n  $!";
if (-s $article_err) {
  die "Error: although the edtfile tool returned successfully, it nonetheless generated a non-empty error file";
}

# sanity check
unless (-e $article_tmp) {
  die "Error: the edtfile tool did not generate the expected file '$article_tmp'";
}
unless (-r $article_tmp) {
  die "Error: the file generated by the edtfile tool, '$article_tmp', is not readable";
}

# load
my @article_lines = ();

open my $miz, '<', $article_in_workdir # we already 'know' this is readable
  or die "Couldn't open an input file handle for $article_miz_path!";
while (defined (my $line = <$miz>)) {
  chomp $line;
  push (@article_lines, $line);
}
close $miz
  or die "Couldn't close the input file handle for $article_miz_path!";

my $num_article_lines = scalar @article_lines;

### 3. Verify (and generate article XML)
system ("$verifier -s -q -l $article_miz > /dev/null 2> /dev/null") == 0
  or die "Error: something went wrong verifying $article_miz: the error was\n\n$!";
unless (-z $article_err) {
  die "Error: although the verifier at $verifier returned successfully, it nonetheless generated a non-empty error file";
}

### 4. Generate the absolute reference version of the generated XML
my $absrefs_stylesheet = catfile ($stylesheet_dir, 'addabsrefs.xsl');
my $article_xml = $article_name . '.xml';
my $article_xml_absrefs = $article_name . '.xml1';
unless (-e $absrefs_stylesheet) {
  die "The absolute reference stylesheet could not be found under $stylesheet_dir!";
}
unless (-r $absrefs_stylesheet) {
  die "The absolute reference styesheet under $stylesheet_dir is not readable.";
}
chdir $workdir;
system ("xsltproc $absrefs_stylesheet $article_xml 2> /dev/null > $article_xml_absrefs") == 0 or
  die "Something went wrong when creating the absolute reference XML: the error was\n\n$!";

### 5. Load the idx file
my $article_idx = $article_name . '.idx';
my %idx_table = ();

# sanity
unless (-e $article_idx) {
  die "IDX file doesn't exist in the working directory ($workdir)!";
}
unless (-r $article_idx) {
  die "IDX file in the working directory ($workdir) is not readable!";
}

my $idx_parser = XML::LibXML->new();
my $idx_doc = $idx_parser->parse_file ($article_idx);
my $symbol_xpath_query 
  = 'Symbols/Symbol'; # this might need to change in the future
my @symbols = $idx_doc->findnodes ($symbol_xpath_query);
foreach my $symbol (@symbols) {
  my $vid = $symbol->findvalue ('@nr');
  my $name = $symbol->findvalue ('@name');
  $idx_table{$vid} = $name;
}

######################################################################
### We're done setting up the work directory; now we can use the
### intermediate files we just generated to split up the given article
### into its constituent items.
######################################################################

sub fetch_directive {
  my $directive = shift;

  chdir $workdir;
  system ("envget -l $article_miz > /dev/null 2> /dev/null") == 0
    or die "envget died working on $article_miz in $workdir!";

  # This is the way things should be, but envget doesn't behave as expected!

  # unless ($? == 0) {
  #   die ("Something went wrong when calling envget on $article_base.\nThe error was\n\n  $!");
  # }

  # But let's check for an error file:
  unless (-z $article_err) {
    die "Error: envget generated a non-empty error file while fetching the value of the directive $directive";
  }

  # cheap approach: take advantage of the fact the the Directives in the
  # EVL file all begin at the beginning of the line
  my $evl_directive = "sed -n -e '/^<Directive name=\"$directive\"/,/^<\\/Directive/p' $article_evl";
  # another cheap trick like the one above
  my $select_identifiers = 'grep "^<Ident name="';

  # now delete all the padding
  my $name_equals_field = 'cut -f 2 -d \' \'';
  my $name_right_hand_side = 'cut -f 2 -d \'=\'';
  my $de_double_quote = 'sed -e \'s/"//g\'';

  my $big_pipe = "$evl_directive | $select_identifiers | $name_equals_field | $name_right_hand_side | $de_double_quote";

  my @directive_items = `$big_pipe`;
  chomp (@directive_items);
  @directive_items = grep (!/^HIDDEN$/, @directive_items);
  return @directive_items;
}

# article environment
my @vocabularies = fetch_directive ('Vocabularies');
my @notations = fetch_directive ('Notations');
my @constructors = fetch_directive ('Constructors');
my @registrations = fetch_directive ('Registrations');
my @requirements = fetch_directive ('Requirements');
my @definitions = fetch_directive ('Definitions');
my @theorems = fetch_directive ('Theorems');
my @schemes = fetch_directive ('Schemes');

my @mml_lar = ();

# ensure that we can read mml.lar
my $mml_lar_path = catfile ($mizfiles, 'mml.lar');
unless (-e $mml_lar_path) {
  die "The file mml.lar doesn't exist under $mizfiles!";
}
unless (-r $mml_lar_path) {
  die "The file mml.lar under $mizfiles is not readable!";
}

sub read_mml_lar {
  open my $mmllar, '<', $mml_lar_path
    or die "mml.lar cannot be opened: $!";
  while (<$mmllar>) {
    chomp;
    push (@mml_lar, $_);
  }
  close ($mmllar)
    or die "Can't close read-only filehandle for mml.lar: $!";
  return;
}

read_mml_lar ();

sub export_item {
  my ($number, $begin_line, $text) = @_;

  # copy the article environment
  my @vocabularies = @vocabularies;
  my @notations = @notations;
  my @constructors = @constructors;
  my @registrations = @registrations;
  my @requirements = @requirements;
  my @definitions = @definitions;
  my @theorems = @theorems;
  my @schemes = @schemes;

  # pad the copied environment with references to ALL earlier items
  # special case: vocabularies, requirements; don't pad these (we will
  # get errors from the mizar tools otherwise).
  @earlier_items = map { "ITEM$_" } 1 .. $number - 1;
  push (@notations, @earlier_items);
  push (@constructors, @earlier_items);
  push (@registrations, @earlier_items);
  push (@definitions, @earlier_items);
  push (@theorems, @earlier_items);
  push (@schemes, @earlier_items);

  # now let's print this thing
  my $item_path = catfile ($article_text_dir, "item$number.miz");
  open my $item_miz, '>', $item_path
    or die "Unable to open an output filehandle at $item_path:\n\n  $!";

  print {$item_miz} 'environ', "\n";

  print {$item_miz}
    'vocabularies ', join (', ', @vocabularies), ';', "\n"
      unless (@vocabularies == 0);

  print {$item_miz}
    'notations ', join (', ', @notations), ';', "\n" 
      unless (@notations == 0);

  print {$item_miz}
    'constructors ', join (', ', @constructors), ';', "\n"
      unless (@constructors == 0);

  print {$item_miz}
    'registrations ', join (', ', @registrations), ';', "\n"
      unless (@registrations == 0);

  print {$item_miz}
    'requirements ', join (', ', @requirements), ';', "\n"
      unless (@requirements == 0);

  print {$item_miz}
    'definitions ', join (', ', @definitions), ';', "\n"
      unless (@definitions == 0);

  print {$item_miz}
    'theorems ', join (', ', @theorems), ';', "\n"
      unless (@theorems == 0);

  print {$item_miz}
    'schemes ', join (', ', @schemes), ';', "\n"
      unless (@schemes == 0);

  print {$item_miz} "\n";
  print {$item_miz} 'begin';
  print {$item_miz} "\n";

  # reservations
  my @reservations = @{reservations_before_line ($begin_line)};
  foreach my $reservation (@reservations) {
    print {$item_miz} 'reserve ', $reservation, "\n";
  }

  # the item proper
  print {$item_miz} $text;

  print {$item_miz} "\n"; # hygenic end-of-file newline

  close $item_miz
    or die "Unable to close the filehandle for the path $item_path";
}

sub miz_xml {
  my $parser = XML::LibXML->new();
  return ($parser->parse_file($article_xml_absrefs));
}

my %reservation_table = ();

sub init_reservation_table {
  foreach my $line_num (0 .. $num_article_lines - 1) {
    my $miz_line = $article_lines[$line_num];
    if ($miz_line =~ m/^reserve[ ]+[^ ]|[ ]+reserve[ ][^ ]/g) {
#                                                          ^ 'g' here; see below
      my $after_reserve = pos $miz_line;
      my $line_up_to_match = substr $miz_line, 0, $after_reserve;
      unless ($line_up_to_match =~ m/::/) {
#                                      ^ no 'g' in match here...
	if ($miz_line =~ m/\G[^;]+;/g) {
#                          ^ ...so '\G' here refers to the penultimate match
	  my $semicolon = pos $miz_line;
	  my $reserve
	    = substr $miz_line,
                     $after_reserve - 1,
#                                   ^ - 1 for the previous non-whitespace char
		     $semicolon - $after_reserve + 1;
#                                                ^ + 1 for the semicolon
	  if ($debug) {
	    warn "Computed reservation $reserve";
	  }

	  $reservation_table{$line_num} = $reserve;
	}
      }
    }
  }
  return;
}

sub print_reservation_table {
  foreach my $key (keys (%reservation_table)) {
    print "$key:\n";
    print ($reservation_table{$key});
    print ("\n");
  }
  return;
}

sub reservations_before_line {
  my $line = shift;
  my @reservations = ();
  foreach my $key (sort {$a <=> $b} keys %reservation_table) {
    if ($key < $line) {
      push (@reservations, $reservation_table{$key});
    }
  }
  return \@reservations;
}

sub from_keyword_to_position {
  my $keyword = shift;
  my $line = shift;
  my $col = shift;
  # find the maximal line and column in @article_lines before column
  # $col of line $line that starts with $keyword.

  # first, check whether the current line already contains the keyword.
  my $first_line = $article_lines[$line -1]; # count lines from 1
  my $first_line_length = length $first_line;

  # sanity
  if ($col > $first_line_length) {
    die "We cannot inspect column $col of line $line, because there aren't that many columns in the line!";
  }

  my $truncated_first_line = substr $first_line, 0, $col - 1; # from 1, not 0

  my $target_line_num = $line;
  my $pos;

  my $target_line = undef;

  until (defined $target_line) {
    $target_line_num--;

    my $current_line;
    if ($target_line_num == $line - 1) { # look at the truncated first line
      $current_line = $truncated_first_line;
    } else {
      $current_line = $article_lines[$target_line_num];
    }

    my $match_pos;
    if ($current_line =~ m/^$keyword|[ ]$keyword$|[ ]$keyword[ ]/g) {
      # check this this occurrence of $keyword isn't commented out
      my $match_pos = pos $current_line;
      my $truncated_current_line = substr $current_line, 0, $match_pos;
      if ($truncated_current_line !~ m/::/) {
	$target_line = $current_line;
      }
    }
  }

  # we have found our line; now find the LAST use of $keyword
  while ($target_line =~ m/^$keyword|[ ]$keyword$|[ ]$keyword[ ]/g) {
    $pos = pos $target_line;
  }

  $target_line_num++; # off by one?!

  return ($target_line_num, $pos);
}

sub theorem_before_position {
  my $end_line = shift;
  my $end_col = shift;
  my ($begin_line,$begin_col)
    = from_keyword_to_position ('theorem', $end_line, $end_col);

  if ($debug) {
    warn "For this theorem, we started at line $end_line and column $end_col";
    warn "For this theorem, the begin_line is $begin_line and the begin_col is $begin_col";
  }

  my $theorem = extract_region ($begin_line,$begin_col,$end_line,$end_col);

  if ($debug) {
    warn "Just extracted theorem: $theorem";
  }

  return $theorem;
}

sub extract_region {
  my $beg_line = shift;
  my $beg_col = shift;
  my $end_line = shift;
  my $end_col = shift;

  my @buffer
    = @{extract_region_as_array ($beg_line, $beg_col, $end_line, $end_col)};

  return join ("\n", @buffer);
}

sub extract_region_as_array {
  my $beg_line = shift;
  my $beg_col = shift;
  my $end_line = shift;
  my $end_col = shift;

  # sanity checking
  if ($beg_line < 0) {
    die "Cannot extract a line with a negative line number\n(we were asked for the region starting from line $beg_line)";
  }
  if ($end_line > $num_article_lines) {
    die "Cannot extract a line beyond the end of the file\n(there are $num_article_lines total, but we were asked for the region up to line $end_line)";
  }

  my @buffer = ();

  # get the first line
  my $first_line_full
    = $article_lines[$beg_line -1]; # count lines from 1
  my $first_line_length = length $first_line_full;
  if ($beg_col <= $first_line_length) {
    if ($beg_line == $end_line) {
      push (@buffer, substr $first_line_full, $beg_col, $end_col - $beg_col + 1);
    } else {
      push (@buffer, substr $first_line_full, $beg_col); # count from 1
    }
  } else {
    die "Cannot extract text from beyond the end of the line\n(the line of the file that was first requested,\n\n  $first_line_full\n\nhas length $first_line_length, but we were asked to start at column $beg_col)";
  }

  # get intermediate lines between $beg_line and $end_line
  foreach my $i ($beg_line .. $end_line - 2) {
    push (@buffer, $article_lines[$i]);
  }

  if ($beg_line != $end_line) {
    # get the last line
    my $last_line_full = $article_lines[$end_line -1]; # count lines from 1
    my $last_line_length = length $last_line_full;
    if ($end_col < $last_line_length) {
      push (@buffer, substr $last_line_full, 0, $end_col + 1); # cols from 1
    } else {
      die "Cannot extract text from beyond the end of the line\n(the last line of the requested region has length $last_line_length, but we were asked to extract up to column $end_col)";
    }
  }

  return \@buffer;
}

sub instruction_greater_than {
  my @instruction_a = @{$a};
  my @instruction_b = @{$b};
  my $line_a = $instruction_a[1];
  my $line_b = $instruction_b[1];
  my $col_a = $instruction_a[2];
  my $col_b = $instruction_b[2];
  if ($line_a < $line_b) {
    return 1;
  } elsif ($line_a == $line_b) {
    if ($col_a < $col_b) {
      return 1
    } elsif ($col_a == $col_b) {
      return 0;
    } else {
      return -1;
    }
  } else {
    return -1;
  }
}

sub extract_article_region_replacing_schemes_and_definitions_and_theorems {
  my $item_kind = shift;
  my $label = shift;
  my $bl = shift;
  my $bc = shift;
  my $el = shift;
  my $ec = shift;
  my @schemes = @{shift ()};
  my @definitions = @{shift ()};
  my @theorems = @{shift ()};

  my @instructions = ();

  foreach my $scheme_info_ref (@schemes) {
    my @scheme_info = @{$scheme_info_ref};
    my @instruction = ('scheme');
    push (@instruction, @scheme_info);
    push (@instructions, \@instruction);
  }
  foreach my $definition_info_ref (@definitions) {
    my @definition_info = @{$definition_info_ref};
    my @instruction = ('definition');
    push (@instruction, @definition_info);
    push (@instructions, \@instruction);
  }
  foreach my $theorem_info_ref (@theorems) {
    my @theorem_info = @{$theorem_info_ref};
    my @instruction = ('theorem');
    push (@instruction, @theorem_info);
    push (@instructions, \@instruction);
  }

  # sort the instructions
  my @sorted_instructions
    = sort instruction_greater_than @instructions; # apply in REVERSE order

  if ($item_kind eq 'canceled') {
    return ''; # the pretext is already 'canceled;'
  }

  # do it
  my @buffer = @{extract_region_as_array ($bl, $bc, $el, $ec)};

  if ($debug) {
    print "Instructions:\n";
    foreach my $instruction_ref (@sorted_instructions) {
      my @instruction = @{$instruction_ref};
      my $instruction_type = $instruction[0];
      my $instr_line_num = $instruction[1];
      my $instr_col_num = $instruction[2];
      my $instr_label = $instruction[3];
      my $instr_item_num = $instruction[4];
      print "($instruction_type $instr_line_num $instr_col_num $instr_label $instr_item_num) in the region ($bl,$bc,$el,$ec)\n";
    }
  }

  foreach my $instruction_ref (@sorted_instructions) {
    my @instruction = @{$instruction_ref};

    # instructions are of two kinds: scheme and theorem instructions,
    # and definition instructions.  Scheme and theorem instructions look
    # like
    #
    # (scheme <line> <column> <label> <item-number>)
    #
    # and
    #
    # (theorem <line> <column> <label> <item-number>) .
    #
    # In both of these kinds of instructions, <line> and <column> refer
    # to the place in the source article where the scheme/theorem
    # reference begins.  Thus, <line> and <column> point
    #
    # blah by Th1,XBOOLE_0:def 4;
    #         ^
    #         here
    #
    # <label> is a string representing the label of the toplevel
    # article-internal item to be replaced.  <item-number> refers to
    # the absolute item number.  Thus, continuing the example above,
    # if the sole instruction were
    #
    # (theorem 1 8 'Th1' 3)
    #
    # then
    #
    # blah by Th1,XBOOLE_0:def 4;
    #
    # would get transformed to
    #
    # blah by ITEM3:1,XBOOLE_0:def 4;
    #
    # Definition instructions have an extra piece of information:
    #
    # (definition <line> <column> <label> <item-number> <relative-item-number>
    #
    # <line>, <column>, <label>, and <item-number> mean the same thing
    # as they do for scheme and theorem instructions. The extra bit
    # <relative-item-number> refers to the number of the definition
    # within <item-number> (which is a definition).  "Definition" here
    # just means "definition block", which can have multiple
    # definitions, hence the need for the further
    # <relative-item-number> information.
    #
    # This function takes the region of the article delimited by BL, BC,
    # EL, and EC and applies the instruction.  It returns a reference to
    # the modfied article text.

    my $instruction_type = $instruction[0];
    my $instr_line_num = $instruction[1];
    my $instr_col_num = $instruction[2];
    my $instr_label = $instruction[3];
    my $instr_item_num = $instruction[4];

    if ($debug) {
      print "Applying instruction ($instruction_type $instr_line_num $instr_col_num $instr_label $instr_item_num) in the region ($bl,$bc,$el,$ec)\n";
      print "The buffer looks like this:\n";
      foreach (@buffer) {
	print "$_\n";
      }
    }

    # sanity checks
    if ($instr_line_num < 1) {
      die "The given editing instruction requests that line $instr_line_num be modified, but that's impossible (we start counting line numbers at 1)!";
    }
    if ($instr_line_num > $num_article_lines) {
      die "The given editing instruction requests that line $instr_line_num be modified, but there aren't that many lines in the article!";
    }
    if ($instr_col_num < 0) {
      die "The given editing instruction requests that column $instr_col_num be inspected, but that's negative!";
    }

    # distinguish between editing instuctions that ask us to modify
    # the first of the region.  From the first line we have (in
    # general) stripped off some initial substring, which (in general)
    # renders the column information of the editing instruction
    # incoherent, so that it needs to be adjusted.
    if ($instr_line_num == $bl) {
      $instr_col_num -= $bc;
    }

    if ($debug) {
      warn "instruction column number is now $instr_col_num";
    }

    # now adjust the instruction's line numbers.  We need to do this
    # because the information from which the editing instruction was
    # generated makes sense relative to the whole file, but we are
    # editing only a snippet of the file here.  Thus, if the editing
    # instruction says to modify line 1000 in a certain way, and we
    # are considering an item that spans lines 990 to 1010, we dealing
    # only with those 21 lines; 990 gets mapped to 0, and 1010 gets
    # mapped to 20, so line "1000" needs to get mapped to line 10.

    if ($debug) {
      warn "We are asked to edit line $instr_line_num...";
    }

    $instr_line_num -= $bl;

    if ($debug) {
      warn "...which just got adjusted to $instr_line_num";
    }

    my $line = $buffer[$instr_line_num];
    unless (defined $line) {
      die "There is no line number $instr_line_num in the current buffer\n\n@buffer\n\n!";
    }
    my $line_length = length $line;

    # more sanity checking
    if ($instr_col_num > $line_length) {
      die "The given editing instruction requests that column $instr_col_num be inspected, but there aren't that many columns in the line\n\n$line\n\nthat we're supposed to look at!";
    }

    # weird special case: the editing instruction says to go to the
    # END of the line.  See line 100
    # of xbool_0.miz for an example of what can go wrong.  What we
    # need to do when we detect this kind of case is adust $line so
    # that it is the first line that contains something not commented
    # out.
    #
    # this is a case where what I'm doing crucially depends on how the
    # mizar parser keeps track of whitespace.  Annoying.
    if ($instr_col_num == $line_length) {

      if ($debug) {
	warn "This is the weird whitespace case!";
      }

      # we need to specially treat multiple equations
      if ($instruction_type eq 'scheme') { # fragile, incorrect
	$instr_line_num++;
	$line = $buffer[$instr_line_num];
	while ($line =~ /^[ ]*::/) {
	  $instr_line_num++;
	  $line = $buffer[$instr_line_num];
	}	
	# we've found the right line; now go to the right column
	$line =~ m/^[ ]*[^ ]/g;
	$instr_col_num = (pos $line) - 2; # back up 2 because of pos
      } else {

	if ($debug) {
	  print "yes, but it is a multiple equation case\n";
	}

      }

      if ($debug) {
	warn "Done dealing with the whitepsace case: the current line is\n\n$line\n\nand the current column is $instr_col_num"
      }

    }

    my $label_length = length $instr_label;

    # weird special case! thanks Josef :-p
    my $offset = $instruction_type eq 'scheme' ? $instr_col_num + 1
                                               : $instr_col_num - $label_length;

    my $before_line = $line;
    if ($instruction_type eq 'scheme') {
      $line =~ s/^(.{$offset})$instr_label(.*)$/$1ITEM$instr_item_num:sch 1$2/;
    } elsif ($instruction_type eq 'theorem') {
      $line =~ s/^(.{$offset})$instr_label(.*)$/$1ITEM$instr_item_num:1$2/;
    } elsif ($instruction_type eq 'definition') {
      my $instr_relative_item_number = $instruction[5];
      $line =~ s/^(.{$offset})$instr_label(.*)$/$1ITEM$instr_item_num:def $instr_relative_item_number$2/;
    } else {
      die "Unknown instruction type $instruction_type";
    }

    if ($debug) {
      print "line after the instuction:\n\n$line\n";
    }

    if ($before_line eq $line) {
      die "We were supposed to do an editig operation, but NOTHING HAPPENED!";
    }

    $buffer[$instr_line_num] = $line;
  }

  return join ("\n", @buffer);
}

my %vid_to_theorem_num = ();
my %theorem_num_to_vid = ();
my %vid_to_diffuse_lemma_num = ();
my %diffuse_lemmas_to_vid = ();
my %vid_to_scheme_num = ();
my %scheme_num_to_vid = ();

sub line_and_column {
  my $node = shift;

  my ($line,$col);

  if ($node->exists ('@line')) {
    $line = $node->findvalue ('@line');
  } else {
    die ("Node lacks a line attribute");
  }

  if ($node->exists ('@col')) {
    $col = $node->findvalue ('@col');
  } else {
    die ("Node lacks a col attribute");
  }

  return ($line,$col);
}

sub pretext_from_item_type_and_beginning {
  my $node = shift;
  my $begin_line = shift;
  my $begin_col = shift;
  my $item_node = shift;

  my $item_type = $node->nodeName;

  if ($debug) {
    warn "Looking for pretext starting from $begin_line and $begin_col";
  }

  my $pretext;
  if ($item_type eq 'JustifiedTheorem') {
    # $pretext = theorem_before_position ($begin_line, $begin_col);
    if ($node->exists ('SkippedProof')) {
      $pretext = 'canceled;'; # in this case, the pretext is the whole text
    } else {
      $pretext = 'theorem ';
    }
  } elsif ($item_type eq 'Proposition') {
    my $vid = $item_node->findvalue ('@vid');

    if ($debug) {
      warn ("unexported toplevel theorem with vid $vid...");
    }

    my $prop_label = $idx_table{$vid};

    if ($debug) {
      warn ("unexported toplevel theorem has label $prop_label...");
      print "this unexported toplevel theorem begins at ($begin_line,$begin_col)\n";
    }

    my ($lemma_begin_line,$lemma_begin_col)
      = from_keyword_to_position ($prop_label, $begin_line, $begin_col);
    $lemma_begin_col++; # because of the colon after the keyword

    if ($debug) {
      print "this unexported toplevel theorem actually starts at ($lemma_begin_line,$lemma_begin_col)\n";
    }

    my $theorem = extract_region ($lemma_begin_line, $lemma_begin_col,
				  $begin_line, $begin_col - 1);
    $pretext = "theorem $theorem";
  } elsif ($item_type eq 'SchemeBlock') {
    $pretext = 'scheme ';
  } elsif ($item_type eq 'NotationBlock') {
    $pretext = "notation ";
  } elsif ($item_type eq 'DefinitionBlock') {
    $pretext = "definition ";
  } elsif ($item_type eq 'RegistrationBlock') {
    $pretext = "registration ";
  } else {
    $pretext = '';
  }
  return $pretext;
}

my %scheme_num_to_abs_num = ();
my %definition_vid_to_absnum = ();
my %definition_vid_to_thmnum = ();
my %theorem_nr_to_absnum = ();
my %theorem_vid_to_absnum = ();

sub is_exported_deftheorem {
  my $deftheorem_node = shift;
  my $node_name = $deftheorem_node->nodeName ();
  unless ($node_name eq 'DefTheorem') {
    die ('This is not even a DefTheorem node!');
  }
  my ($prop_node) = $deftheorem_node->findnodes ('Proposition');
  unless (defined $prop_node) {
    die "Weird: a DefTheorem node lacks a Proposition child element";
  }
  return $prop_node->exists ('@vid');
}

sub load_deftheorems {
  my $doc = miz_xml ();

  my $last_deftheorem_xpath_query
    = '//Article/DefTheorem[name(following-sibling::*) != "DefTheorem"]';

  my @final_deftheorem_nodes = $doc->findnodes ($last_deftheorem_xpath_query);
  
  foreach my $i (1 .. scalar (@final_deftheorem_nodes)) {
    my $final_deftheorem_node = $final_deftheorem_nodes[$i-1];
    
    # find the first DefTheorem after this definitionblock
    # DefinitionBlock that give rise to exported theorems
    my $current_node = $final_deftheorem_node;
    while ($current_node->nodeName () eq 'DefTheorem') {
      $current_node = $current_node->previousNonBlankSibling ();
    }
    
    # now that we know how many exported DefTheorems this
    # DefinitionBlock gave rise to, we need to harvest the vid's of
    # the Proposition child elements of such DefTheorems; these are
    # the vid's that we'll need later.
    
    # reuse $current_node from the previous loop -- it is the last
    # DefTheorem node generated by the DefinitionBlock.  We need to go
    # *forward* now to ensure that items getting the same label (i.e.,
    # the same vid) are mapped to their *last* (re)definition.
    $current_node = $current_node->nextNonBlankSibling ();
    my $num_exported_theorems = 1;
    while ((defined $current_node) && $current_node->nodeName () eq 'DefTheorem') {
      if (is_exported_deftheorem ($current_node)) {
	my ($prop_node) = $current_node->findnodes ('Proposition');
	my $vid = $prop_node->findvalue ('@vid');
	$definition_vid_to_thmnum{$vid} = $num_exported_theorems;

	if ($debug) {
	  warn ("We just assigned vid $vid to exported thereom number $num_exported_theorems of this definition block (we don't know yet what the absolute number of this definitionblock is)");
	}

      }
      $current_node = $current_node->nextNonBlankSibling ();
      $num_exported_theorems++;
    }
  }

  return;
}

my @nodes = ();

my %node_processors
  = (
     'JustifiedTheorem[not(SkippedProof)]' => \&process_justifiedtheorem,
     'Proposition' => \&process_toplevel_proposition,
     'DefinitionBlock' => \&process_definitionblock,
     'SchemeBlock' => \&process_schemeblock,
     'RegistrationBlock' => \&process_registrationblock,
     'NotationBlock' => \&process_notationblock,
    );

my @handled_node_types = keys %node_processors;

# I wish I knew how to deal with these :-<
my @unhandled_node_types = ('DefFunc',
			    'Defpred',
			    'Set',
			    'Consider',
			    'Reconsider',
			    'Proposition[not(@vid)]');

sub load_items {
  my $doc = miz_xml ();
  # check for unhandled nodes; die quickly

  my @toplevel_unhandled_item_xpaths
    = map { "Article/$_" } @unhandled_node_types;
  my $unhandled_query = join (' | ', @toplevel_unhandled_item_xpaths);

  if ($doc->exists ($unhandled_query)) {
    warn "There's an unhandled node type in this article; sorry";
    exit 2;
  }

  my @toplevel_item_xpaths = map { "Article/$_" } @handled_node_types;
  my $query = join (' | ', @toplevel_item_xpaths);
  @nodes = $doc->findnodes ($query);

  if ($debug) {
    print 'we found ', scalar @nodes, ' nodes', "\n";
  }

  return;
}

sub process_justifiedtheorem {}
sub process_toplevel_proposition {}

sub process_definitionblock {
  # register definitions, making sure to count the ones that
  # generate DefTheorems
  my @local_definition_nodes = $node->findnodes ('.//Definition');
  foreach my $local_definition_node (@local_definition_nodes) {
    my $vid = $local_definition_node->findvalue ('@vid');
    # search for the Definiens following this node, if any
    my $next = $node->nextNonBlankSibling ();
    $definition_vid_to_absnum{$vid} = $i;
  }
}

{
  my $scheme_num = 0;
  sub process_schemeblock {
    # register a scheme, if necessary
    $scheme_num++;
    $scheme_num_to_abs_num{$scheme_num} = $i;
    my $vid = $node->findvalue ('@vid');
    unless (defined $vid) {
      die "SchemeBlock node lacks a vid!";
    }
    $scheme_num_to_vid{$scheme_num} = $vid;
  }
}

sub process_registrationblock {}
sub process_notationblock {}

load_items ();
load_deftheorems ();
init_reservation_table ();

sub itemize {

  my $scheme_num = 0;
  foreach my $i (1 .. scalar (@nodes)) {
    my $node = $nodes[$i-1];
    my $node_name = $node->nodeName;

    # register a scheme, if necessary
    if ($node_name eq 'SchemeBlock') {
      $scheme_num++;
      $scheme_num_to_abs_num{$scheme_num} = $i;
      my $vid = $node->findvalue ('@vid');
      unless (defined $vid) {
	die "SchemeBlock node lacks a vid!";
      }
      $scheme_num_to_vid{$scheme_num} = $vid;
    }

    # register definitions, making sure to count the ones that
    # generate DefTheorems
    if ($node_name eq 'DefinitionBlock') {
      my @local_definition_nodes = $node->findnodes ('.//Definition');
      foreach my $local_definition_node (@local_definition_nodes) {
	my $vid = $local_definition_node->findvalue ('@vid');
	# search for the Definiens following this node, if any
	my $next = $node->nextNonBlankSibling ();
	$definition_vid_to_absnum{$vid} = $i;
      }
    }

    if ($node_name eq 'Defpred') {
      warn "Node $i is a global defpred statement; we don't know how to handle these yet.";
      exit 2;
    }

    if ($node_name eq 'Deffunc') {
      warn "Node $i is a global deffunc statement; we don't know how to handle these yet.";
      exit 2;
    }

    if ($node_name eq 'Reconsider') {
      warn "Node $i is a global reconsider statement; we don't know how to handle these yet.";
      exit 2;
    }

    if ($node_name eq 'Set') {
      warn "Node $i is a global reconsider statement; we don't know how to handle these yet.";
      exit 2;
    }

    # deal with Definiens elements corresponding to Definition
    # elements that we've already seen -- record their relative ordering
    if ($node_name eq 'DefTheorem') {
      my ($prop_node) = $node->findnodes ('Proposition');
      unless (defined $prop_node) {
	die "Weird: a DefTheorem node (item $i) lacks a Proposition child element";
      }
      if ($prop_node->exists ('@vid')) {
	my $vid = $prop_node->findvalue ('@vid');
	# ensure that we really have seen this before
	unless (defined $definition_vid_to_absnum{$vid}) {
	  die "DefTheorem/Proposition with vid = $vid has not been previously registered!";
	}
      }
    }

    # register theorems that get referred to later in the article
    if ($node_name eq 'JustifiedTheorem' or $node_name eq 'Proposition') {
      my $proposition_node;
      if ($node_name eq 'JustifiedTheorem') {
	($proposition_node) = $node->findnodes ('Proposition[position()=1]');
      } else {
	$proposition_node = $node;
      }

      if (defined $proposition_node) {
	if ($proposition_node->exists ('@nr') && $proposition_node->exists ('@vid')) {
	  my $nr = $proposition_node->findvalue ('@nr');
	  my $vid = $proposition_node->findvalue ('@vid');

	  if ($debug) {
	    warn ("we found a theorem that gets referred to later! its nr is $nr and its vid is $vid");
	  }

	  $theorem_nr_to_absnum{$nr} = $i;
	  $theorem_vid_to_absnum{$vid} = $i;	
	}
      } else {
	die "Weird: a JustiiedTheorem without a Proposition child element? Why?";
      }
    }

    # find the beginning
    my ($begin_line, $begin_col);
    if ($node_name eq 'DefinitionBlock' ||
	$node_name eq 'SchemeBlock' ||
        $node_name eq 'RegistrationBlock' ||
	$node_name eq 'NotationBlock') {

      ($begin_line,$begin_col) = line_and_column ($node);

    } elsif ($node_name eq 'Proposition') {
      my $vid = $node->findvalue ('@vid');
      unless (defined $vid) {
	die "ouch!";
      }

      if ($debug) {
	warn ("unexported toplevel theorem with vid $vid...");
      }

      my $prop_label = $idx_table{$vid};
      ($begin_line, $begin_col) = line_and_column ($node);
      # weird: this might not be accurate!
      # ($begin_line, $begin_col)
      #  	= from_keyword_to_position ($prop_label, $begin_line, $begin_col);
    } else { # JustifiedTheorem
      my ($theorem_proposition) = $node->findnodes ('Proposition[position()=1]');
      unless (defined ($theorem_proposition)) {
	die ("Weird: node $i, a JustifiedTheorem, lacks a Proposition child element");
      }
      ($begin_line, $begin_col) = line_and_column ($theorem_proposition);
      # weird: this might not be accurate!
      ($begin_line, $begin_col)
       	= from_keyword_to_position ('theorem', $begin_line, $begin_col);
    }

    # now find the end, if there is such a thing in the text
    unless ($node_name eq 'DefTheorem') {
      my ($end_line, $end_col);
      my $last_endposition_child;

      # we need to look at its proof
      if ($node_name eq 'Proposition') {
	my $next = $node->nextNonBlankSibling ();
	unless (defined ($next)) {
	  die ("Weird: node $i, a Proposition, is not followed by a sibling!");
	}
	my $next_name = $next->nodeName ();
	if ($next_name eq 'Proof') {
	  ($last_endposition_child)
	    = $next->findnodes ('EndPosition[position()=last()]');
	  # die ("Weird: the next sibling of node $i, a Proposition, is not a Proof element! It is a $next_name element, somehow");
	} elsif ($next_name eq 'By'
		 || $next_name eq 'From') {
	  my ($last_ref) = $next->findnodes ('Ref[position()=last()]');
	  if (defined ($last_ref)) {
	    $last_endposition_child = $last_ref;
	  } else {
	    # die ("Weird: node $i, a Proposition, is immediately justified, but the justification lacks a Ref child element!");
	    $last_endposition_child = $next;
	  }
	}
      } elsif ($node_name eq 'JustifiedTheorem') {
	my ($proof) = $node->findnodes ('Proof');
	my ($by_or_from) = $node->findnodes ('By | From');
	if (defined ($proof)) {
	  ($last_endposition_child)
	    = $proof->findnodes ('EndPosition[position()=last()]');
	} elsif (defined ($by_or_from)) {
	  my ($last_ref) = $by_or_from->findnodes ('Ref[position()=last()]');
	  if (defined ($last_ref)) {
	    $last_endposition_child = $last_ref;
	  } else {
	    $last_endposition_child = $by_or_from;
	    # die ("Node $i, a JustifiedTheorem, is immediately justified, but no statements are mentioned after the by/from keyword!");
	  }
	} else {
	  # this is the case of cancelled theorems
	  if ($node->exists ('SkippedProof')) { # toplevel skipped proof
	    my ($prop_node) = $node->findnodes ('Proposition');
	    $last_endposition_child = $prop_node;
	  } else {
	    die ("Node $i, a JustifiedTheorem, lacks a Proof as well as a SkippedProof, nor is it immediately justified by a By or From statement");
	  }
	}
      } else {
	($last_endposition_child)
	  = $node->findnodes ('EndPosition[position()=last()]');
      }

      unless (defined ($last_endposition_child)) {
	die ("Weird: node $i (a $node_name) lacks an EndPosition child element");
      }
      ($end_line,$end_col) = line_and_column ($last_endposition_child);

      # kludge: EndPosition information for Schemes differs from all
      # other elements: it is off by one: it includes the final
      # semicolon of the "end;", whereas other elements end at "end".
      if ($node_name eq 'SchemeBlock') {
	$end_col--;
      }

      if ($debug) {
	print "the region of interest is ($begin_line,$begin_col)-($end_line,$end_col)\n";
      }

      # look into the node to find references that might need to be
      # rewritten.  First, distinguish between unexported toplevel
      # theorems and the rest; for the former, the references to be
      # gathered are *not* contained within the $node, but rather in its
      # following sibling.
      my $ref_containing_node;
      if ($node_name eq 'Proposition') {
	my $next = $node->nextNonBlankSibling ();
	my $next_name = $next->nodeName ();
	if ($next_name eq 'Proof') {
	  $ref_containing_node = $next;
	} else {
	  $ref_containing_node = $node; # there's no following proof; there's no need to rewrite references
	}
      } else {
	$ref_containing_node = $node;
      }

      # gather all local schemes
      my @local_schemes = ();
      my @local_scheme_nodes = $ref_containing_node->findnodes ('.//From');

      if ($debug) {
	warn ("this node has " . scalar (@local_scheme_nodes) . " local scheme nodes");
      }

      foreach my $local_scheme_node (@local_scheme_nodes) {
	my $articlenr = $local_scheme_node->findvalue ('@articlenr');

	if ($debug) {
	  warn ("articlenr of this From node is $articlenr");
	}

	if ($articlenr == 0) {
	  my $local_scheme_line = $local_scheme_node->findvalue ('@line');
	  my $local_scheme_col = $local_scheme_node->findvalue ('@col');
	  my $local_scheme_sch_num = $local_scheme_node->findvalue ('@absnr');
	  my $local_scheme_abs_num = $scheme_num_to_abs_num{$local_scheme_sch_num};
	  my $scheme_vid = $scheme_num_to_vid{$local_scheme_sch_num};
	  my $scheme_label = $idx_table{$scheme_vid};

	  if ($debug) {
	    warn "scheme vid is $scheme_vid; its label is $scheme_label";
	  }

	  my @local_scheme_info = ($local_scheme_line, $local_scheme_col, $scheme_label, $local_scheme_abs_num);
	  push (@local_schemes, \@local_scheme_info);

	  if ($debug) {
	    warn ("we found a scheme use starting at line $local_scheme_line and column $local_scheme_col, scheme $local_scheme_sch_num in the article, which is item number $local_scheme_abs_num");
	  }

	}
      }

      my @local_definitions = ();
      my @local_ref_nodes = $ref_containing_node->findnodes ('.//Ref');

      if ($debug) {
	warn ("this node has " . scalar (@local_ref_nodes) . " Ref elements");
      }

      foreach my $ref_node (@local_ref_nodes) {
	if ($ref_node->exists ('@aid')) {

	  if ($debug) {
	    warn ("This ref node points to something outside the current article");
	  }

	} else {
	  if ($debug) {
	    warn ("This ref node points to something in the current article");
	  }

	  my $vid = $ref_node->findvalue ('@vid');
	  my $absnum = $definition_vid_to_absnum{$vid};
	  my $thm_num = $definition_vid_to_thmnum{$vid};
	  if (defined ($absnum) && defined ($thm_num)) {

	    if ($debug) {
	      warn ("this article-internal ref points to absolute item $absnum and theorem $thm_num of whatever definitionblock introduced it");
	    }

	    my $def_label = $idx_table{$vid};

	    if ($debug) {
	      warn "it's vid is $vid; its label is $def_label";
	    }

	    my $line = $ref_node->findvalue ('@line');
	    my $col = $ref_node->findvalue ('@col');
	    my @local_definition_info = ($line,$col,$def_label,$absnum,$thm_num);
	    push (@local_definitions, \@local_definition_info);
	  }
	}
      }

      my @local_theorems = ();
      @local_ref_nodes = $ref_containing_node->findnodes ('.//Ref');

      if ($debug) {
	warn ("searching for theorem references; this node has " . scalar (@local_ref_nodes) . " Ref elements");
      }

      foreach my $ref_node (@local_ref_nodes) {
	if ($ref_node->exists ('@aid')) {

	  if ($debug) {
	    warn ("This ref node points to something outside the current article");
	  }

	} else {

	  if ($debug) {
	    warn ("This ref node points to something in the current article");
	  }

	  my $nr = $ref_node->findvalue ('@nr');
	  my $vid = $ref_node->findvalue ('@vid');
	  my $theorem_nr_absnum = $theorem_nr_to_absnum{$nr};
	  my $theorem_vid_absnum = $theorem_vid_to_absnum{$vid};
	  if (defined ($theorem_nr_absnum) && defined ($theorem_vid_absnum)) { # this Ref points to an article-local theorem

	    if ($debug) {
	      warn ("this article-internal ref points to theorem_nr_absnum $theorem_nr_absnum and theorem_vid_absnum $theorem_vid_absnum");
	    }

	    if ($theorem_nr_absnum == $theorem_vid_absnum) { # sanity check
	      my $line = $ref_node->findvalue ('@line');
	      my $col = $ref_node->findvalue ('@col');
	      my $th_label = $idx_table{$vid};

	      if ($debug) {
		warn "the vid of this theorem is $vid; its label is $th_label";
	      }

	      my @local_theorem_info = ($line,$col,$th_label,$theorem_nr_absnum);
	      push (@local_theorems, \@local_theorem_info);
	    }
	  }
	}
      }

      my $pretext
	= pretext_from_item_type_and_beginning ($node,
						$begin_line,
						$begin_col,
						$node);

      if ($debug) {
	print "the pretext is '$pretext'\n";
      }

      # check for whether we're dealing with one of those annoying unexported toplevel theorems, for which we need to know its label
      my $label;
      if ($node_name eq 'Proposition') {
	my $vid = $node->findvalue ('@vid');

	if ($debug) {
	  warn ("unexported toplevel theorem with vid $vid...");
	}

	$label = $idx_table{$vid};
	unless (defined $label) {
	  warn "Don't know how to handle toplevel unexported theorems without labels!";
	  exit 2;
	}

	if ($debug) {
	  warn ("unexported toplevel theorem has label $label...");
	}

      } else {
	$label = '';
      }

      my $node_keyword;
      if ($node_name eq 'JustifiedTheorem') {
	if ($node->exists ('SkippedProof')) {
	  $node_keyword = 'canceled';
	} else {
	  $node_keyword = 'theorem';	
	}
      } elsif ($node_name eq 'Proposition') {
	$node_keyword = 'proposition';
      } elsif ($node_name eq 'SchemeBlock') {
	$node_keyword = 'scheme';
      } elsif ($node_name eq 'RegistrationBlock') {
	$node_keyword = 'registration';
      } elsif ($node_name eq 'DefinitionBlock') {
	$node_keyword = 'definition';
      } elsif ($node_name eq 'NotationBlock') {
	$node_keyword = 'notation';
      }

      my $text
	= extract_article_region_replacing_schemes_and_definitions_and_theorems ($node_keyword, $label, $begin_line, $begin_col, $end_line, $end_col, \@local_schemes, \@local_definitions, \@local_theorems);

      chomp $text;
      print ("Item $i: $node_name: ($begin_line,$begin_col)-($end_line,$end_col)\n");
      print ("======================================================================\n");
      print ("$pretext$text");
      print ("\n");
      print ("======================================================================\n");

      export_item ($i, $begin_line, "$pretext$text");
    }
  }

  return scalar @nodes;
}

my $num_items = itemize ();

######################################################################
### Incrementally verify and export each of the generated items
######################################################################

sub verify_item_with_number {
  my $item_number = shift;

  if ($be_verbose) {
    print "Verifying article fragment #$item_number\n";
  }

  my $miz = catfile ('text', "item$item_number.miz");
  my $err = catfile ('text', "item$item_number.err");

  # where we'll work
  chdir $local_db_in_workdir;

  # sanity check: article exists and is readable
  unless (-e $miz) {
    die "Error: the mizar article for item number $item_number does not exist under $article_text_dir!";
  }
  unless (-r $miz) {
    die "Error: the mizar article for item number $item_number under $article_text_dir is not readable!";
  }

  # accomodate
  system ("accom -q -s -l $miz > /dev/null 2> /dev/null") == 0
    or die "Error: Something went wrong when calling the accomodator on $miz under $article_text_dir: the error was\n\n$!";

  # sanity
  if (-e $err && -s $err) {
    die "Error: although the accomodator returned successfully when run on $miz under $article_text_dir,\\it nonetheless generated a non-empty error file";
  }

  # verify
  system ("$verifier -q -s -l $miz > /dev/null 2> /dev/null") == 0
    or die "Error: Something went wrong when calling the verifier at $verifier on $miz under $article_text_dir: the error was\n\n$!";

  # more sanity
  if (-e $err && -s $err) {
    die "Error: although the verifier at $verifier returned successfully when run on $miz under $article_text_dir,\\it nonetheless generated a non-empty error file";
  }

  return;
}

sub export_item_with_number {
  my $item_number = shift;

  if ($be_verbose) {
    print "Exporting article fragment #$item_number\n";
  }

  my $miz = catfile ('text', "item$item_number.miz");
  my $err = catfile ('text', "item$item_number.err");

  # where we'll work
  chdir $local_db_in_workdir;

  # sanity check: article exists and is readable
  unless (-e $miz) {
    die "Error: the mizar article for item number $item_number does not exist under $article_text_dir!";
  }
  unless (-r $miz) {
    die "Error: the mizar article for item number $item_number under $article_text_dir is not readable!";
  }

  # export
  system ("exporter -q -s -l $miz > /dev/null 2> /dev/null") == 0
    or die "Error: Something went wrong when exporting $miz under $article_text_dir: the error was\n\n$!";
  if (-e $err && -s $err) {
    die "Error: although the exporter terminated successfully after working on $miz (under $article_text_dir),\na non-empty error file was generated nonetheess!";
  }

  # transfer
  system ("transfer -q -s -l $miz > /dev/null 2> /dev/null") == 0
    or die "Error: Something went wrong when calling the transfer tool on $miz under $article_text_dir: the error was\n\n$!";
  if (-e $err && -s $err) {
    die "Error: although transfer terminated successfully after working on $miz (under $article_text_dir),\na non-empty error file was generated nonetheess!";
  }

  return;
}

sub trim_directive {
  my $directive_name = shift;
  my $extension_for_directive = shift;
  my @directive_contents = @{shift ()};
  my @trimmed = ();
  foreach my $directive_item (@directive_contents) {
    my $file_to_look_for = catfile ($article_prel_dir, "$directive_item.$extension_for_directive");
    if (grep (/^$directive_item$/i, @mml_lar)) {
      push (@trimmed, $directive_item);
    } elsif ($directive_item eq 'TARSKI') { # TARSKI is not listed in mml.lar
      push (@trimmed, $directive_item);
    } elsif (-e $file_to_look_for) {
      push (@trimmed, $directive_item);
    }
  }
  return \@trimmed;
}

sub trim_item_with_number {

  my $item_number = shift;

  if ($be_verbose) {
    print "Triming article fragment #$item_number\n";
  }

  my $miz = catfile ($article_text_dir, "item$item_number.miz");

  # sanity check: the article fragment exists and is readable
  unless (-e $miz) {
    die "Error: unable to trim article fragment $item_number because the corresponding .miz file doesn't exist under $article_text_dir!";
  }
  unless (-r $miz) {
    die "Error: unable to trim article fragment $item_number because the corresponding .miz file under $article_text_dir is not readable!";
  }

  # copy the environment
  my @notations = @notations;
  my @constructors = @constructors;
  my @registrations = @registrations;
  my @definitions = @definitions;
  my @theorems = @theorems;
  my @schemes = @schemes;

  my @earlier_item_numbers = ();
  foreach my $i (1 .. $item_number - 1) {
    push (@earlier_item_numbers, "ITEM$i");
  }

  # in addition to any directives that the original article uses,
  # conservatively start off by saying that the fragment depends on
  # ALL earlier fragments.  We'll cut that down later.
  push (@notations, @earlier_item_numbers);
  push (@constructors,@earlier_item_numbers);
  push (@registrations, @earlier_item_numbers);
  push (@definitions, @earlier_item_numbers);
  push (@theorems, @earlier_item_numbers);
  push (@schemes, @earlier_item_numbers);

  # Now that we've ballooned each of the directives, cut them down to
  # something more sensible.  Notice that we're not doing anything
  # with the vocabularies and requirements directives; these are
  # special cases.
  @notations = @{trim_directive ('notations', 'dno', \@notations)};
  @constructors = @{trim_directive ('constructors', 'dco', \@constructors)};
  @registrations = @{trim_directive ('registrations', 'dcl', \@registrations)};
  @definitions = @{trim_directive ('definitions', 'def', \@definitions)};
  @theorems = @{trim_directive ('theorems', 'the', \@theorems)};
  @schemes = @{trim_directive ('schemes', 'sch', \@schemes)};

  # we're going to overwrite the .miz with the trimmed environment
  open my $miz_in, '<', $miz
    or die "Coudn't open read-only filehandle for $miz: $!";
  unlink $miz;
  open my $miz_out, '>', $miz
    or die "Couldn' open write-only filehandle for $miz: $!";
  while (defined (my $line = <$miz_in>)) {
    chomp $line;
    if ($line =~ /^notations /) {
      print {$miz_out} ('notations ' . join (', ', @notations) . ";\n")
	unless scalar @notations == 0;
    } elsif ($line =~ /^constructors /) {
      print {$miz_out} ('constructors ' . join (', ', @constructors) . ";\n")
	unless scalar @constructors == 0;
    } elsif ($line =~ /^registrations /) {
      print {$miz_out} ('registrations ' . join (', ', @registrations) . ";\n")
	unless scalar @registrations == 0;
    } elsif ($line =~ /^definitions /) {
      print {$miz_out} ('definitions ' . join (', ', @definitions) . ";\n")
	unless scalar @definitions == 0;
    } elsif ($line =~ /^theorems /) {
      print {$miz_out} ('theorems ' . join (', ', @theorems) . ";\n")
	unless scalar @theorems == 0;
    } elsif ($line =~ /^schemes /) {
      print {$miz_out} ('schemes ' . join (', ', @schemes) . ";\n")
	unless scalar @schemes == 0;
    } elsif ($line =~ /^requirements /) { # special case
      print {$miz_out} ('requirements ' . join (', ', @requirements) . ";\n")
	unless scalar @requirements == 0;
    } else {
      print {$miz_out} ("$line\n");
    }
  }

  close $miz_in
    or die "Couldn't close input filehandle for $miz!";
  close $miz_out
    or die "Couldn't close output filehandle for $miz!";

  return;
}

foreach my $item_number (1 .. $num_items) {
  trim_item_with_number ($item_number);
  verify_item_with_number ($item_number);
  export_item_with_number ($item_number);
}


######################################################################
### Cleanup
######################################################################

sub cleanup {

  # move the local db in $workdir to $result_dir

  move ($local_db_in_workdir, $local_db_in_resultdir) == 1
    or die ("Something went wrong transferring our work from\n\n  $local_db_in_workdir\n\nto\n\n  $local_db_in_resultdir");

  if ($cleanup_afterward) {

    # trash the entire working directory
    remove_tree ($workdir);
    # according to File::Path, error handling for remove_tree is done
    # via the Carp module, and not through the return value (which just
    # counts the number of files and directories removed).  Deferring to
    # this modules method for error handling is not ideal, but I'm too
    # lazy to investigate how to take over error handling and reporting;
    # in any case, how to do that is described in the File::Path
    # documentation.

    # cleanup any non-.miz, non-.xml files in the recently-moved local
    # db

    # just use find
    my $text_subdir_of_local_db_in_resultdir
      = catdir ($local_db_in_resultdir, 'text');
    system ("find $text_subdir_of_local_db_in_resultdir -type f -and -not -name '*.miz' -and -not -name '*.xml' -exec rm {} ';'") == 0
      or die "find did not terminate properly deleting the non-.miz and non-.xml files in the text directory! The error was:\n\n  $!";
  } else {
    print "Not clearning up the work directory; all auxiliary files can be found in the directory\n\n  $local_db_in_resultdir\n\nfor your inspection.\n";
  }
}

sub trim_vocabularies_for_item {
  my $item_number = shift;

  my $item_miz_path = catfile ($workdir, 'text', "item$item_number.miz");
  my $item_evl_path = catfile ($workdir, 'text', "item$item_number.evl");
  my $item_err_path = catfile ($workdir, 'text', "item$item_number.err");

  # read the evl
  my $parser = XML::LibXML->new();
  my $evl_doc = $parser->parse_file($item_evl_path);

  my @idents
    = $evl_doc->findnodes ('Environ/Directive[name="Vocabularies"]/Ident');

  # forget about HIDDEN
  @idents = grep (!/^HIDDEN$/, @idents);

  # assume that everything in the vocabularies directive appears in one line
  my %col_to_article = ();
  foreach my $ident (@idents) {
    my $col = $ident->findvalue ('@col');
    my $name = $ident->findvalue ('@name');
    $col_to_article{$col} = $name;
  }

  # now run irrvoc
  chdir $workdir;
  system ("irrvoc text/item$item_number") == 0
    or die ("Something went wrong calling irrvoc on item $item_number under $workdir");

  # read .err file that irrvoc just generated
  open my $err_file, '<', $item_err_path
    or die "Unable to open the .err file for item $item_number under $workdir: $!";

  my @err_lines = ();
  while (defined (my $err_line = <$err_file>)) {
    chomp $err_line;
    push (@err_lines, $err_line);
  }

  close $err_file
    or die "Unable to close the input filehandle for $item_err_path! $!";

  # the lines look like this:
  #
  # 2 19 709
  #
  # <line> <column> <error type>
  #
  # We are going to assume that all the <line> values are equal (that
  # is, we are assuming that the contents of the vocabulary directive
  # in the generated article fragment is contained on a single line,
  # and we're going to ignore the error code: assuming that we got
  # this far, we know that our article fragment is a valid mizar
  # article, so we should always get the code 709, which means:
  # unsused vocabulary item.  That's just what we're looking for.

  my @unused_by_column = ();

  foreach my $err_line (@err_lines) {
    @line_column_errcode = split / /, $err_line;
    unless (@line_column_errcode == 3) {
      die "We found an error line that does not have exactly three fields: @line_column_errcode";
    }
    my $col = $line_column_errcode[1];
    push (@unused_by_column, $col);
  }

  # now we know the column numbers on the unique line that contains
  # the contents of this article fragment's vocabulary directive.
  # Let's dump these.
  foreach my $col_of_unused_voc_item (@unused_by_column) {
    delete $col_to_article{$col_of_unused_voc_item};
  }

  # the keys of %col_to_article now contains the columns of all used
  # vocabulary items.

  my @used_vocabulary_items = keys %col_to_article;

  if (scalar @used_vocabulary_items < scalar @idents) { # something was deleted
    rewrite_vocabulary_directive_of_item ($item_number, \@used_vocabulary_items);
  }

  return;

}

## return the number of printed
sub PrepareXml
{
    my ($filestem,$file_ext,$xmlelems,$removed,$xmlbeg,$xmlend) = @_;
    my $res = 0;
    open(XML1,">$filestem$file_ext");
    print XML1 $xmlbeg;
    foreach my $elemnr (0 .. scalar(@$xmlelems) -1)
    {
	if(! exists $removed->{$elemnr})
	{
	    print XML1 $xmlelems->[$elemnr];
	    $res++;
	}
    }
    print XML1 $xmlend;
    close(XML1);
    return $res;
}


sub TestXMLElems ($$$)
{
    my ($xml_elem,$file_ext,$filestem) = @_;
    # print $filestem, "\n";
    # print $file_ext, "\n";
    # print $makeenv, "\n";
    # print getcwd(), "\n";

    die "makeenv errors"
      unless system ("$makeenv -l $filestem > /dev/null 2> /dev/null") == 0;

    my $xml_contents;
    my $xitemfile = $filestem . $file_ext;
    if (-e $xitemfile) {
      {
	open(XML, $xitemfile);
	local $/; $xml_contents = <XML>;
	close(XML);
      }
    } else {
      print "nothing to trim", "\n";
      return;
    }

    my ($xmlbeg,$xmlnodes,$xmlend) = $xml_contents
      =~ m/(.*?)([<]$xml_elem\b.*[<]\/$xml_elem>)(.*)/s;

    if (!defined $xmlbeg) {
      return;
    }

    ## call Mizar parser to get the tp positions
    my @xmlelems = $xmlnodes
      =~ m/(<$xml_elem\b.*?<\/$xml_elem>)/sg; # this is a multiline match

    # sanity
    die "Verification errors"
      unless system ("$verifier -l -q $filestem > /dev/null 2>/dev/null") == 0;

    my %removed = (); ## indices of removed elements

    ## remove consecutive chunks of sqrt size and to retract to
    ## one-by-one if the chunk fails (not sure why better than
    ## logarithmic approach - perhaps simpler to write)
    my $total = scalar(@xmlelems);
    my $chunksize = 1 + int(sqrt($total));
    my $chunks = int($total / $chunksize);
    foreach my $chunk (0 .. $chunks)
    {
	foreach my $elem (0 .. $chunksize -1)
	{
	    $removed{$chunk * $chunksize + $elem} = 1;
	}
	PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
	if (system ("$verifier -l -q $filestem > /dev/null 2>/dev/null") != 0)
	{
	    foreach my $elem (0 .. $chunksize -1)
	    {
		delete $removed{$chunk * $chunksize + $elem};
	    }
	    my $found = 0; ## when 1, at least one was found necessary already from these
	    foreach my $elem (0 .. $chunksize -1)
	    {
		## if the first condition is unmet, we know the last
		## elem is culprit and don't have to test
		if(!(($elem == $chunksize -1) && ($found == 0)) && 
		   ($chunk * $chunksize + $elem <= $#xmlelems))
		{
		    $removed{$chunk * $chunksize + $elem} = 1;
		    PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
		    if (system ("$verifier -l -q $filestem > /dev/null 2>/dev/null") != 0)
		    {
			delete $removed{$chunk * $chunksize + $elem};
			$found = 1;
		    }
		}
	    }
	}
    }

	
    # foreach my $chunk (0 .. $#xmlelems)
    # {
    # 	$removed{$chunk} = 1;
    # 	PrepareXml($filestem,$file_ext,$xmlelems,$removed,$xmlbeg,$xmlend);
    # 	delete $removed{$chunk} if(system("$verifier -l -q $filestem") !=0);
    # }
    ## print the final form
    my $needed 
      = PrepareXml($filestem,$file_ext,\@xmlelems,\%removed,$xmlbeg,$xmlend);
    ## print stats
    print 'total ', $xml_elem, ': ', $total, "\n";
    print 'removed: ', $total - $needed, "\n";
    print 'needed: ', $needed, "\n";
}

my %item_to_extension =
  (
   'Vocabulary' => ['vcl'],
   'Definiens' => ['dfs'],
   'RCluster' => ['ecl'],
   'CCluster' => ['ecl'],
   'FCluster' => ['ecl'],
   'Scheme' => ['esh'],
   'Constructor' => ['atr', 'aco'], # what's the difference?
   'Theorem' => ['eth'],
   'Identify' => ['eid'],
   'Notation' => ['eno'],
  );

sub reduce_imported_items {
  my $item_number = shift;

  my $item_stem = catfile ($local_db_in_workdir, 'text', "item$item_number");
  foreach my $item_type (keys %item_to_extension) {
    my @extensions = @{$item_to_extension{$item_type}};
    foreach my $extension (@extensions) {
      TestXMLElems ($item_type, '.' . $extension, $item_stem);
    }
  }
}

foreach my $i (1 .. $num_items) {
  reduce_imported_items ($i);
}

cleanup ();

exit 0;

__END__

=head1 NAME

itemize – Decompose a mizar article into its constituent parts

=head1 VERSION

Alpha!

=head1 USAGE

  itemize.pl [options] ARTICLE

=head1 REQUIRED ARGUMENTS

=over

=item <ARTICLE>

ARTICLE should be the name of an article.  If ARTICLE ends with
".miz", then the part of the article before the ".miz" will be treated
as the name of the article.

ARTICLE will be looked for in the directory specified by the
--article-source-dir option.  If that option is unset, then the 'mml'
subdirectory of whatever is specified by the MIZFILES environment
variable will be used.

ARTICLE must be at most 1 but at most 8 characters long, all
alphanumeric (though the underscore character '_' is permitted)
excluding an optional ".miz" file extension.

=for Euclid:
     ARTICLE.type: /^[A-Za-z0-9_]{1,8}(\.miz)?/
     ARTICLE.type.error:   Article must be at most 8 characters long, all alphanumeric (or '_'); it may end in '.miz', and this is not counted in the limit of 8 characters.  You supplied 'ARTICLE'.

=back

=head1 OPTIONS

=over

=item --mizfiles=<DIRECTORY>

Sets the $MIZFILES environmental variable to DIRECTORY for Mizar
processing.  The default is its value in the current environment.

=item --with-verifier=<PATH-TO-VERIFIER>

Use the program located at PATH-TO-VERIFIER as the mizar verifier.  By
default, whatever 'verifier' is according to the current PATH
environment variable will be used.

=item --with-makeenv=<PATH-TO-MAKEENV>

Use the program located at PATH-TO-MAKEENV as the mizar makeenv
program.  By default, whatever 'makeenv' is according to the current
PATH environment variable will be used.

=item --article-source-dir=<DIRECTORY>

Take ARTICLE from DIRECTORY.  Both relative and absolute paths are
acceptable.

If this option is unset, then the MIZFILES environment variable will
be consulted, and the subdirectory 'mml' will be the source for
ARTICLE.

=item --result-dir=<RESULT-DIRECTORY>

Make a local mizar database for ARTICLE in RESULT-DIRECTORY, which
should be an absolute path.  The database will itself be a subdirecory
of RESULT-DIRECTORY called by the same name as ARTICLE; the database
itself will contain subdirectories 'prel' and 'text'.

RESULT-DIRECTORY, if unset, defaults to the current directory.  If
set, it should be a path; it can be either an absolute path (beginning
with a forward slash '/') or a relative path (i.e., anything that is
not an absolute path).  In either case, it is up to the user to ensure
that, if RESULT-DIRECTORY is set, that the directory exists and is
writable.

=for Euclid:

=item --with-stylesheets-in=<STYLESHEET-DIRECTORY>

The directory that contains the relevant stylesheets applied to the
article XML.  Both absolute and relative paths may be supplied.  If
this option is unset, then the current directory ('.') is used.

=for Euclid:
     STYLESHEET-DIRECTORY.default: '.'

=item --no-cleanup

Don't remove auxiliary, intermediate files generated for the sake of
decomposing the given article. (By default, all such files will be
deleted before terminating.)

=item --verbose

Indicate what's going on at notable points in the computation.

=item --debug

(For developers only.)  Print debugging information.  Warning: this
may generate a lot of confusing output.

=item --version

=item --usage

=item --help

=item --man

Print the usual program information.

=back

=head1 DESCRIPTION

This program divides a mizar article into its constituent pieces.

A directory called by the same name as ARTICLE will be created in the
directory specified by the --result-dir option.  The default is to use
the current directory.

Upon termination, the directory ARTICLE will be a mizar "working
directory" containing subdirectories "dict", "prel", and "text".
Inside the "text" subdirectory there will be as many new mizar
articles as there are items in ARTICLE.  The "dict" subdirectory will
likewise contain as vocabulary files as there are items in ARTICLE.
The "prel" subdirectory will contain the results of calling miz2prel
on each of the standalone articles.

=head1 DIAGNOSTICS

Always returns 0, if it terminates cleanly at all.

(Obviously, this is useless.  This will change in future versions as
the program matures.)

=head1 CONFIGURATION AND ENVIRONMENT

This program uses the MIZFILES environment variable.

=head1 DEPENDENCIES

=head2 PERL DEPENDENCIES

=head3 Non-standard modules

=over

=item File::Tempdir

=item File::Spec

=item Getopt::Euclid (>= 0.2.3)

=item File::Copy

=item File::Path

=back

=head2 NON-PERL DEPENDENCIES

=over

=item addabsrefs.xsl

XSL stylesheet by Josef Urban for computing, from the XML generated by
the mizar verifier, a version of the same thing with more information.

=back

=head1 INCOMPATIBILITIES

This program uses some unix trickery; it almost certainly doesn't work
on Windows.  Sorry.

=head1 BUGS AND LIMITATIONS

Breaking up article is very slow.  There are lots of opportunities for
optimization.

This module currently does not handle articles that have global set or
reconsider statements.  If any article with such features is detected,
further processing stops.

Please report problems to Jesse Alama (jesse.alama@gmail.com). Patches
are welcome.

=head1 AUTHORS

Jesse Alama (jesse.alama@gmail.com)

=head1 ACKNOWLEDGEMENTS

Thanks to Josef Urban, as always, for his mizar-ly support and advice
and to Karol Pąk for his essential JA and JA1 mizar tools.

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2010 Jesse Alama (jesse.alama@gmail.com). All rights reserved.

This module is free software; you can redistribute it and/or modify it
under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 TODO

=over

=item Dependency computation

trim_directive cuts down the contents of a specified directive by
looking for files in the local prel database that have a given suffix.
This approach is fairly fast, because it just depends on testing
existence of files.  This is perhaps the laziest workable approach; it
ensures only that the article fragmens make sense to the mizar
tools. We should plug in to Josef's code; it requires far more
computation than simply checking the existence of suitable files, but
that is the way we cut things down as far as possible.

=item Mizar module

A fair amount of this code deals with just running mizar tools,
checking their return values and existence of a non-empty .err file,
etc.  This kind of thing more naturally belongs in a separate mizar
module rather than here.

=item Unhandled items

=over

=item deffunc

From ABCMIZ_1 (of MML 4.150.1103):

deffunc F(set,set) =
{[varcl A, j] where A is Subset of $2, j is Element of NAT: A is finite};

=item defpred

From FINSEQ_1 (of MML 4.150.1103):

defpred P[set,set] means ex k st $1 = k & $2 = k+1;

=item consider

Consider this bad boy from AFVECT01 (of MML 4.150.1103):

consider AFV0 being WeakAffVect;
set X = the carrier of AFV0;
set XX = [:X,X:];
defpred P[set,set] means ex a,b,c,d being Element of X st $1=[a,b] & $2=[c,d]
& a,b '||' c,d;
consider P being Relation of XX,XX such that
Lm13: for x,y being set holds [x,y] in P iff x in XX & y in XX & P[x,y]
from RELSET_1:sch 1;

Hairy!

For a less hairy example, look at BHSP_1 (from MML 4.150.1103):

consider V0 being RealLinearSpace;

=item reconsider

From RAT_1 (of MML 4.150.1103):

then reconsider 09 = 0 as Element of REAL+ by ARYTM_2:2;

(The "then" links the reconsider with the previous statement, an
unexported toplevel theorem

  0 in omega;

with no proof.)

=item set

From ARYTM_2 (from MML 4.150.1103):

set IR = { A where A is Subset of RAT+: r in A implies (for s st s <=' r holds
s in A) & ex s st s in A & r < s}, RA = {{ s: s < t}: t <> {}};

item unlabeled unexported theorems

=back

The following 129 articles of MML 4.150.1103 contain, at the toplevel,
either reconsider, set, defpred, deffunc, or unlabeled unexported
theorems:

=over

=item abcmiz_1

=item abcmiz_a

=item afvect01

=item ami_4

=item ami_wstd

=item amistd_1

=item analmetr

=item arytm_0

=item arytm_1

=item arytm_2

=item arytm_3

=item axioms

=item bhsp_1

=item bhsp_2

=item bhsp_3

=item bhsp_4

=item binop_2

=item brouwer

=item card_1

=item card_2

=item card_lar

=item cardfin2

=item cfdiff_1

=item chain_1

=item classes1

=item classes2

=item closure2

=item clvect_1

=item clvect_2

=item clvect_3

=item collsp

=item commacat

=item comptrig

=item comseq_2

=item comseq_3

=item convex2

=item cqc_sim1

=item csspace

=item dtconstr

=item euclid_9

=item euler_1

=item fdiff_1

=item fib_num

=item fib_num4

=item filter_1

=item filter_2

=item finset_1

=item geomtrap

=item heyting1

=item heyting2

=item int_3

=item jgraph_2

=item jordan

=item jordan1a

=item jordan1c

=item jordan1d

=item jordan24

=item lattice3

=item limfunc1

=item measure7

=item metric_1

=item monoid_0

=item monoid_1

=item mssublat

=item msualg_1

=item nagata_2

=item normsp_0

=item normsp_1

=item numbers

=item ortsp_1

=item parsp_1

=item pcs_0

=item pdiff_1

=item polyalg1

=item polyeq_1

=item polyeq_2

=item pre_circ

=item pre_ff

=item prelamb

=item prvect_1

=item qc_lang3

=item qmax_1

=item quantal1

=item rat_1

=item rcomp_3

=item real_3

=item realset2

=item scm_comp

=item scm_halt

=item scmbsort

=item scmfsa6c

=item scmfsa7b

=item scmfsa8a

=item scmfsa8b

=item scmfsa9a

=item scmfsa_7

=item scmfsa_9

=item scmisort

=item scmp_gcd

=item scmpds_2

=item scmpds_6

=item scmpds_7

=item scmpds_8

=item scpinvar

=item scpisort

=item scpqsort

=item sfmastr1

=item sfmastr2

=item sin_cos

=item sin_cos6

=item sincos10

=item symsp_1

=item topalg_2

=item topalg_3

=item topalg_4

=item topalg_5

=item topgen_3

=item topreal2

=item topreala

=item toprealb

=item twoscomp

=item valued_2

=item vectsp_1

=item vectsp_2

=item xcmplx_0

=item xreal_0

=item xreal_1

=item zf_model

=item zfmodel1

=back

=back

=cut

# itemize.pl ends here
