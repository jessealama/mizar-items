#!/usr/bin/perl

use strict;
use XML::LibXML;

# my @item_kinds = ('Pattern', 'Definiens', 'Identify', 'RCFCluster', 'Constructor');
my @item_kinds = ('Definiens', 'Identify', '[RCF]Cluster', 'Constructor');
# - Definiens: gather these from from XML, compare to what appers in dfs
#
# - Identification: similar to Definiens.
#
# - Patterns: collect PIDs from the XML file, use that as the set of patterns;
# otherwise, use brute force
#
# - Constructor: take esh/eth; from these, gather the constructors.
# Might fail; if it does, use the full brute-force approach.  We might
# need to do ths recursively, invoving the result types of
# constructors, result type of result type, etc.

my %item_to_extension =
  (
   'Definiens' => 'dfs',
   '[RCF]Cluster' => 'ecl',
#   'CCluster' => 'ecl',
#   'FCluster' => 'ecl',
#   'Scheme' => 'esh',
   'Constructor' => 'atr',
#   'Theorem' => 'eth',
   'Identify' => 'eid',
   'Pattern' => 'eno',
  );

my $article = $ARGV[0];

my $ramdisk = "/dev/shm/alama/itemization";
my $harddisk = "/mnt/sdb3/alama/itemization";

# Make sure we don't max out the ramdisk
my @stuff_in_ramdisk = `find $ramdisk -mindepth 1 -maxdepth 1 -type d`;
if (@stuff_in_ramdisk > 20) {
  die "Failure: ramdisk is full; refusing to proceed";
}

# sanity check: article exists on the harddisk
my $article_on_harddisk = "$harddisk/$article";
unless (-e $article_on_harddisk) {
    die "$article doesn't exist under $harddisk!";
}

# check that this article is brutalizable: it has been fully itemized
unless (-e "$article_on_harddisk/prel") {
    die "Failure: article $article was not properly itemized!";
}

print "Brutalizing $article...", "\n";

# sanity check: the article isn't already in the ramdisk
my $article_in_ramdisk = "$ramdisk/$article";
if (-e $article_in_ramdisk) {
    die "Failure: $article is already in the ramdisk -- why?";
}

## return three hash pointers - first for schemes, then theorems, then definitions
sub ParseRef
  {
    my ($filestem) = @_;
    open(REF,'<', "$filestem.refx") or die "Unable to open an input filehandle for $filestem.refx in directory $article_in_ramdisk: $!";
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
      open(XML, $xitemfile) or die "Unable to open an output filehandle for $xitemfile in directory $article_in_ramdisk!";
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

    open(XML1,">$filestem$file_ext") or die "Unable to open an output filehandle for $filestem$file_ext in directory $article_in_ramdisk: $!";
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

if (system ('cp', '-Rf', $article_on_harddisk, $ramdisk) != 0) {
  system ('rm', "-Rf", $article_in_ramdisk); # trash whatever is there
  die "Failure: error copying $article to the ramdisk: $!";
}

chdir $article_in_ramdisk or die "Unable to chdir to $article_in_ramdisk!";

my @items_for_article = `ls text/ckb*.miz | sed -e 's/\.miz//'`;
chomp @items_for_article;

foreach my $item (@items_for_article) {

  print "Brutalizing item $item of article $article...", "\n";

  print "Verifiying item...", "\n";
  
  my $verifier_time 
    = `timeout 5m /mnt/sdb3/alama/mizar-items/timed-quiet-verify.sh $item`;
  my $exit_code = $?;
  $exit_code >> 8;
  if ($exit_code != 0) {
    system ('rm', "-Rf", $article_in_ramdisk); # trash whatever is there
    if ($exit_code == 124) {
      die "Failure: timeout verifying item $item of article $article!";
    } else {
      die "Failure: we were unable to verify item $item of article $article!";
    }
  }
  chomp $verifier_time;
  print "It took $verifier_time seconds to verifiy $item", "\n";

  my $timeout = (int ($verifier_time * 2) + 1) . "s";

  print "Using a timeout of $timeout seconds", "\n";

  # absrefs
  system ("xsltproc /home/urban/gr/xsl4mizar/addabsrefs.xsl $item.xml -o $item.xml1 > /dev/null 2>&1");
  # skip checking the exit code because it seems that this doesn't always give us 0 -- bad :-<

  # take cae of theorems and schemes first
  my $parsed_ref = ParseRef ($item);
  PruneRefXML ('Scheme', '.esh', $item, $parsed_ref);
  PruneRefXML ('Theorem', '.eth', $item, $parsed_ref);

  # now brutalize the Patterns
  if (-e "$item.eno") {
    my $item_xml_parser = XML::LibXML->new();
    my $item_xml_doc = $item_xml_parser->parse_file ("$item.xml1");
    my @patterns = $item_xml_doc->findnodes ('.//*[@pid >= 0]');
    my @preds = $item_xml_doc->findnodes ('.//Pred');

    my %pattern_table = ();

    foreach my $pattern_node (@patterns, @preds) {
      my $aid = $pattern_node->findvalue ('@aid');
      my $kind = $pattern_node->findvalue ('@kind');
      my $nr = $pattern_node->findvalue ('@nr');
      unless (defined $aid && defined $kind && defined $nr) {
	die "We found a Pattern node in $item.eno that lacks either an AID, KIND, or NR attribute";
      }
      # warn "putting '$aid:$kind:$nr' into the pattern table";
      $pattern_table{"$aid:$kind:$nr"} = 0;
    }

    my $xitemfile = "$item.eno";
    {
      open(XML, $xitemfile) 
	or die "Unable to open an output filehandle for $xitemfile in directory $article_in_ramdisk!";
      local $/; $_ = <XML>;
      close(XML);
    }

    my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]Pattern\b.*[<]\/Pattern>)(.*)/s;

    open(XML1,'>', "$item.eno") 
      or die "Unable to open an output filehandle for $item.eno in directory $article_in_ramdisk: $!";
    print XML1 $xmlbeg;

    my @xmlelems = $xmlnodes =~ m/(<Pattern\b.*?<\/Pattern>)/sg; # this is a multiline match
    foreach my $pattern_str (@xmlelems) {
      $pattern_str =~ /aid=\"([^"]+)\" .*constrkind=\"([A-Z])\" constrnr=\"([0-9]+)\"/ or die "Bad pattern string: $pattern_str";
      my ($aid,$kind,$nr) = ($1,$2,$3);
      # warn "looking for '$aid:$kind:$nr' in the pattern table...";
      if (defined $pattern_table{"$aid:$kind:$nr"}) {
	# warn "wow, there's a match!";
	print XML1 "$pattern_str\n";
      }
    }

    print XML1 $xmlend;

    close XML1;
  } else {
    print "no patterns to trim for $item", "\n";
  }

  foreach my $item_kind (@item_kinds) {

    my $extension = $item_to_extension{$item_kind};
      
    if (-e "$item.$extension") {
      print "brutalizing item kind $item_kind for item $item of article $article...", "\n";
      my $exit_code = system ('/mnt/sdb3/alama/mizar-items/miz_item_deps_bf.pl', $item_kind, $extension, $item, $timeout);
	
      my $exit_code = $exit_code >> 8;
      my $err_message = $!;
      if ($exit_code == 0) {
	print "successfully minimized item kind $item_kind", "\n";
	# system ('cp', "$item.$extension", "$item-needed-$item_kind");
      } else {
	print "failure", "\n";
	system ('rm', "-Rf", "/dev/shm/alama/itemization/$article") == 0
	  or die "Failure: error deleting $article from the ramdisk!";
	die "Failure: something went wrong brutalizing item kind $item_kind for $item of $article: the exit code was $exit_code and the error output was: $err_message";
      }
    } else {
      print "Success: nothing to brutalize for items of kind $item_kind for item $item of article $article", "\n";
    }
  }
}

system ('rm', '-Rf', $article_on_harddisk);
system ('mv', $article_in_ramdisk, $harddisk) == 0
  or die "Failure: error moving $article out of the ramdisk!";

system ('touch', "$harddisk/$article-brutalization-stamp");

print "Success: brutalization of article $article succeeded", "\n";

exit 0;
