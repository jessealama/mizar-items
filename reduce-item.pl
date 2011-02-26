#!/usr/bin/perl -w

use strict;
use XML::LibXML;

my $item = $ARGV[0];

my ($article,$item_number) = split (/:/, $item);

my $article_fragment_name = "text/ckb$item_number";

my @gitem_kinds = ('Definiens', 'Identify', '[RCF]Cluster');

my %item_to_extension =
  (
   'Definiens' => 'dfs',
   'Pattern' => 'eno',
   '[RCF]Cluster' => 'ecl',
   'Identify' => 'eid',
  );

my $ramdisk = "/Volumes/ramdisk";
my $harddisk = "/tmp";
my $article_on_harddisk = "$harddisk/$article";
my $article_in_ramdisk = "$ramdisk/$article";

print "Brutalizing $article...", "\n";

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

my @item_kinds = @gitem_kinds;

print "Brutalizing item $item of article $article...", "\n";

print "Verifiying $item...", "\n";

chdir $article_in_ramdisk;

unless (-e "$article_fragment_name.miz") {
  die "Article fragment '$article_fragment_name.miz' doesn't exist!";
}

my $exit_code = system ("verifier -q -s -l $article_fragment_name > /dev/null 2> /dev/null");
$exit_code = $exit_code >> 8;
if ($exit_code != 0) {
  die "Failure: we were unable to verify $article_fragment_name of article $article (exit code $exit_code)!";
}

# absrefs
my $absrefs = '/Users/alama/sources/mizar/xsl4mizar/addabsrefs.xsl';
# sanity
unless (-e $absrefs) {
  die "The absrefs stylesheet at '$absrefs' doesn't exist!";
}
unless (-r $absrefs) {
  die "The absrefs stylesheet at '$absrefs' is not readable!";
}
system ("xsltproc $absrefs $article_fragment_name.xml -o $article_fragment_name.xml1 > /dev/null 2>&1");
# skip checking the exit code because it seems that this doesn't always give us 0 -- bad :-<

my $item_xml_parser = XML::LibXML->new();
my $item_xml_doc = $item_xml_parser->parse_file ("$article_fragment_name.xml1");

# take care of theorems and schemes first
my $parsed_ref = ParseRef ($article_fragment_name);
PruneRefXML ('Scheme', '.esh', $article_fragment_name, $parsed_ref);
PruneRefXML ('Theorem', '.eth', $article_fragment_name, $parsed_ref);

# now brutalize the Patterns

if (-e "$article_fragment_name.eno") {
  # first, save the eno file; we will brutalize it, but we might need the original form later
  system ("cp $article_fragment_name.eno $article_fragment_name.eno.orig");

  # now we start the brutalization proper
  my @pid_bearers = $item_xml_doc->findnodes ('.//*[@pid > 0]');
  my @preds = $item_xml_doc->findnodes ('.//Pred');
  my @patterns = $item_xml_doc->findnodes ('.//Pattern');

  my %pattern_table = ();

  foreach my $pid_bearer (@pid_bearers, @preds) {
    my $aid = $pid_bearer->findvalue ('@aid');
    my $kind = $pid_bearer->findvalue ('@kind');
    my $nr = $pid_bearer->findvalue ('@nr');
    unless (defined $aid && defined $kind && defined $nr) {
      die "We found a PID-bearing node in $article_fragment_name.eno that lacks either an AID, KIND, or NR attribute";
    }
    # warn "putting '$aid:$kind:$nr' into the pattern table";
    $pattern_table{"$aid:$kind:$nr"} = 0;
  }

  foreach my $pattern_node (@patterns) {
    my $constraid = $pattern_node->findvalue ('@constraid');
    my $constrkind = $pattern_node->findvalue ('@constrkind');
    my $constrnr = $pattern_node->findvalue ('@constrnr');
    unless (defined $constraid && defined $constrkind && defined $constrnr) {
      die "We found a Pattern node in $article_fragment_name.eno that lacks either a CONSTRAID, CONSTRKIND, or CONSTRNR attribute";
    }
    # warn "putting '$constraid:$constrkind:$constrnr' into the pattern table";
    $pattern_table{"$constraid:$constrkind:$constrnr"} = 0;
  }

  my $xitemfile = "$article_fragment_name.eno";
  {
    open(XML, $xitemfile) 
      or die "Unable to open an output filehandle for $xitemfile in directory $article_in_ramdisk!";
    local $/; $_ = <XML>;
    close(XML);
  }

  my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]Pattern\b.*[<]\/Pattern>)(.*)/s;

  open(XML1,'>', "$article_fragment_name.eno") 
    or die "Unable to open an output filehandle for $article_fragment_name.eno in directory $article_in_ramdisk: $!";
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

  # check that this heuristic worked.  If not, brutalize patterns in the usual way
  if (system ("verifier -q -s -l $article_fragment_name > /dev/null 2> /dev/null") != 0) {
    print "pattern heuristic failed", "\n";
    my @pattern_list = ('Pattern');
    push (@pattern_list, @item_kinds);
    @item_kinds = @pattern_list;
    system ("mv $article_fragment_name.eno.orig $article_fragment_name.eno");
  } else {
    print "pattern heuristic succeeded for $article_fragment_name", "\n";
  }

} else {
  print "no patterns to trim for $article_fragment_name", "\n";
}

foreach my $item_kind (@item_kinds) {

  my $extension = $item_to_extension{$item_kind};

  if (-e "$article_fragment_name.$extension") {
    print "brutalizing item kind $item_kind for item $article_fragment_name of article $article...", "\n";
    my $exit_code = system ('/Users/alama/sources/mizar/mizar-items/miz_item_deps_bf.pl', $item_kind, $extension, $article_fragment_name);
	
    $exit_code = $exit_code >> 8;
    my $err_message = $!;
    if ($exit_code == 0) {
      print "successfully minimized item kind $item_kind", "\n";
    } else {
      print "failure", "\n";
      die "Failure: something went wrong brutalizing item kind $item_kind for $$article_fragment_name of $article: the exit code was $exit_code and the error output was: $err_message";
    }
  } else {
    print "Success: nothing to brutalize for items of kind $item_kind for item $article_fragment_name of article $article", "\n";
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

my $xitemfile = "$article_fragment_name.aco";
{
  open(XML, $xitemfile)
    or die "Unable to open an output filehandle for $xitemfile in directory !";
  local $/; $_ = <XML>;
  close(XML);
}

my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]Constructor\b.*[<]\/Constructor>)(.*)/s;

open(XML1,'>', "$article_fragment_name.atr.pruned") 
  or die "Unable to open an output filehandle for $article_fragment_name.atr.pruned in directory  !";
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
