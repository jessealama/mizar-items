#!/usr/bin/perl -w

use strict;
use XML::LibXML;
use File::Basename qw(basename);

unless (scalar @ARGV == 1) {
  print 'Usage: brutalize.pl ARTICLE', "\n";
  exit 1;
}

my $article = $ARGV[0];

my $article_basename = basename ($article, '.miz');
my $article_miz = "${article_basename}.miz";
my $article_xml = "${article_basename}.xml";
my $article_absolute_xml = "${article_basename}.xml1";

unless (-e $article_miz) {
  print 'Error: the .miz for ', $article_basename, ' does not exist.', "\n";
  exit 1;
}

unless (-r $article_miz) {
  print 'Error: the .miz for ', $article_basename, ' is unreadable.', "\n";
  exit 1;
}

my $absrefs_stylesheet = '/Users/alama/sources/mizar/xsl4mizar/addabsrefs.xsl';

unless (-e $absrefs_stylesheet) {
  print 'Error: the absolutizer stylesheet does not exist at the expected location (', $absrefs_stylesheet, ').', "\n";
  exit 1;
}

unless (-r $absrefs_stylesheet) {
  print 'Error: the absolutizer stylesheet at ', $absrefs_stylesheet, ' is unreadable.', "\n";
  exit 1;
}

if (! -e $article_absolute_xml) {

  if (! -e $article_xml) {
    print 'The XML for ', $article_basename, ' does not exist.  Generating...';
    my $analyzer_status = system ("verifier -a -q -l $article_miz > /dev/null 2>&1");
    my $analyzer_exit_code = $analyzer_status >> 8;
    if ($analyzer_exit_code != 0) {
      print "\n"; # flush
      print 'Error: something went wrong generating the XML for ', $article_basename, '.', "\n";
      exit 1;
    }
    print 'done.', "\n";
  }

  print 'The absolute form of the XML for ', $article_basename, ' does not exist.  Generating...';

  my $xsltproc_status = system ("xsltproc --output $article_absolute_xml $absrefs_stylesheet $article_xml 2>/dev/null");

  # Skip checking the exit code of xsltproc; we can be safe even when
  # it is non-zero, owing to errors like 'Missing .fex' and 'Missing
  # .bex'.  But we will check that the $article_absolute_xml exists.

  unless (-e $article_absolute_xml) {
    print "\n";
    print 'Error: we failed to generate the absolute form of the XML for ', $article_basename, '.', "\n";
    exit 1;
  }

  print 'done.', "\n";
}

unless (-e $article_absolute_xml) {
  print 'Error: the absolute form of the article ', $article_basename, ' does not exist.', "\n";
  exit 1;
}

unless (-r $article_absolute_xml) {
  print 'Error: the absolute form of the article ', $article_basename, ' is unreadable.', "\n";
  exit 1;
}

my @gitem_kinds = ('Property', 'Reduction', 'Definiens', 'Identify', '[RCF]Cluster');

my %item_to_extension =
  (
   'Definiens' => 'dfs',
   'Pattern' => 'eno',
   'Property' => 'epr',
   'Reduction' => 'erd',
   '[RCF]Cluster' => 'ecl',
   'Identify' => 'eid',
  );

## return three hash pointers - first for schemes, then theorems, then definitions
sub ParseRef
  {
    my ($filestem) = @_;
    open(REF,'<', "$filestem.refx") or die "Unable to open an input filehandle for $filestem.refx: $!";
    my @refs = ({},{},{});
    my $i = 0;
    while(<REF>)
      {
        if(/syThreeDots/)
          {
            $_ = <REF>;
            $_ =~ /x=\"(\d+)\"/ or die "bad REFX file";
            my $articlenr = $1;
            $_ = <REF>;
            $_ =~ /x=\"(\d+)\"/ or die "bad REFX file";
            my $refnr = $1;
            <REF>; <REF>;
            $refs[$i]->{"$articlenr:$refnr"} = 0;
          }
        if(/sySemiColon/)
          {
            $i++;
          }
    }
    close(REF);
    die "Wrong number of ref kinds: $i" if($i != 3);

    # DEBUG
    # foreach my $i (0 .. 2) {
    #  my %hash = %{$refs[$i]};
      # warn "ParseRef: hash number $i";
      # foreach my $key (keys (%hash)) {
        # warn "key: $key, value ", $hash{$key};
    #   }
    # }

    return \@refs;
}

## only callable with .eth or .esh; $refs is an array pointer of three hashes returned by ParseRef
sub PruneRefXML
{
  my ($xml_elem,$file_ext,$filestem,$refs_ref) = @_;

  my @refs = @{$refs_ref};

  my ($schs, $ths, $defs) = @refs;

  my $res = 0;


  my $xitemfile = $filestem . $file_ext;
  if (-e $xitemfile) {
    {
      open(XML, $xitemfile) or die "Unable to open an output filehandle for $xitemfile!";
      local $/; $_ = <XML>;
      close(XML);
    }
  } else {
    print "nothing to trim for $xitemfile\n";
    return;
  }

  my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]$xml_elem\b.*[<]\/$xml_elem>)(.*)/s;
  if (defined $xmlbeg) {
    ## call Mizar parser to get the tp positions
    my @xmlelems = $xmlnodes =~ m/(<$xml_elem\b.*?<\/$xml_elem>)/sg; # this is a multiline match

    open(XML1,">$filestem$file_ext") or die "Unable to open an output filehandle for $filestem$file_ext: $!";
    print XML1 $xmlbeg;

    if ($file_ext eq '.eth') {
      foreach my $elemnr (0 .. scalar(@xmlelems)-1) {
        my $first_line = (split /\n/, $xmlelems[$elemnr] )[0];

        $first_line =~ m/.*articlenr=\"(\d+)\".* nr=\"(\d+)\".* kind=\"([DT])\"/ or die "bad element $first_line";

        my ($ref, $kind) = ( "$1:$2", $3);
        my $needed = ($kind eq 'T')? $ths : $defs;

        if ( exists $needed->{$ref}) {
          print XML1 $xmlelems[$elemnr];
          $res++;
        }
      }
    } elsif ($file_ext eq '.esh') {
      foreach my $elemnr (0 .. scalar(@xmlelems)-1) {
        my $first_line = (split /\n/, $xmlelems[$elemnr] )[0];
        $first_line =~ m/.*articlenr=\"(\d+)\".* nr=\"(\d+)\".*/ or die "bad element $first_line";

        if ( exists $schs->{"$1:$2"}) {
          print XML1 $xmlelems[$elemnr];
          $res++;
        }
      }
    } else {
      die "bad extension $file_ext";
    }

    print XML1 $xmlend;
    close(XML1);
    return $res;
  }
}

my @item_kinds = @gitem_kinds;

#print "Brutalizing item $item of article $article...", "\n";

# print "Verifiying $item...", "\n";

my $item_xml_parser = XML::LibXML->new();
my $item_xml_doc = $item_xml_parser->parse_file ("$article_absolute_xml");

# take care of theorems and schemes first
my $parsed_ref = ParseRef ($article);
PruneRefXML ('Scheme', '.esh', $article, $parsed_ref);
PruneRefXML ('Theorem', '.eth', $article, $parsed_ref);

my $brutalizer_script = '/Users/alama/sources/mizar/mizar-items/miz_item_deps_bf.pl';

# sanity
unless (-e $brutalizer_script) {
  die "The needed brutalizer script doesn't exist at '$brutalizer_script'!";
}
unless (-r $brutalizer_script) {
  die "The brutalizer script at '$brutalizer_script' cannot be read!";
}
unless (-x $brutalizer_script) {
  die "The brutalizer script at '$brutalizer_script' is not executable!";
}

foreach my $item_kind (@item_kinds) {

  my $extension = $item_to_extension{$item_kind};

  if (-e "${article_basename}.${extension}") {
    my $exit_code = system ($brutalizer_script, $item_kind, $extension, $article);

    $exit_code = $exit_code >> 8;
    my $err_message = $!;
    if ($exit_code == 0) {
      print "successfully minimized item kind $item_kind", "\n";
    } else {
      print "failure", "\n";
      # die "Failure: something went wrong brutalizing item kind $item_kind for $$article_fragment_name of $article: the exit code was $exit_code and the error output was: $err_message";
    }
  } else {
    print "Success: nothing to brutalize for items of kind $item_kind", "\n";
  }
}

# now infer what the constructors should be
my @preds = $item_xml_doc->findnodes ('.//Pred');
my @funcs = $item_xml_doc->findnodes ('.//Func');
my @typs = $item_xml_doc->findnodes ('.//Typ');
my @adjectives = $item_xml_doc->findnodes ('.//Adjective');
my @patterns = $item_xml_doc->findnodes ('.//Pattern');

my %constr_table = ();

foreach my $kind_and_nr_bearer (@preds, @funcs, @typs, @adjectives) {
  unless ($kind_and_nr_bearer->exists ('@kind')) {
    die "We found a node that lacks a value for its KIND attribute! $kind_and_nr_bearer";
  }
  unless ($kind_and_nr_bearer->exists ('@nr')) {
    die "We found a node that lacks a value for its NR attribute! $kind_and_nr_bearer";
  }
  my $kind = $kind_and_nr_bearer->findvalue ('@kind');
  my $nr = $kind_and_nr_bearer->findvalue ('@nr');
  # warn "putting '$aid:$kind:$nr' into the pattern table";
  $constr_table{"$kind:$nr"} = 0;
}

foreach my $pattern_node (@patterns) {
  unless ($pattern_node->exists ('@constrkind')) {
    die "We found a Pattern node that lacks a CONSTRKIND attribute! $pattern_node"
  }
  unless ($pattern_node->exists ('@constrnr')) {
    die "We found a Pattern node that lacks a CONSTRNR attribute! $pattern_node"
  }
  my $constrkind = $pattern_node->findvalue ('@constrkind');
  my $constrnr = $pattern_node->findvalue ('@constrnr');
  # warn "putting '$constraid:$constrkind:$constrnr' into the pattern table";
  $constr_table{"$constrkind:$constrnr"} = 0;
}

my $xitemfile = "${article_basename}.aco";
{
  open(XML, $xitemfile)
    or die "Unable to open an output filehandle for $xitemfile in directory !";
  local $/; $_ = <XML>;
  close(XML);
}

my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]Constructor\b.*[<]\/Constructor>)(.*)/s;

open(XML1,'>', "${article_basename}.atr.pruned")
  or die "Unable to open an output filehandle for ${article_basename}.atr.pruned in directory  !";
print XML1 $xmlbeg;

my @xmlelems = $xmlnodes =~ m/(<Constructor\b.*?<\/Constructor>)/sg; # this is a multiline match
foreach my $pattern_str (@xmlelems) {
  $pattern_str =~ /<Constructor.*\bkind=\"([A-Z])\" .*\brelnr=\"([0-9]+)\"/ or die "Bad constr string: $pattern_str";
  my ($kind,$nr) = ($1,$2);
  # warn "looking for '$aid:$kind:$nr' in the pattern table...";
  if (defined $constr_table{"$kind:$nr"}) {
    # warn "wow, there's a match!";
    print XML1 "$pattern_str\n";
  }
}

print XML1 $xmlend;

close XML1;

exit 0;
