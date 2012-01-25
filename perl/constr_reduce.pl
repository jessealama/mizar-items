#!/usr/bin/perl

use strict;
use XML::LibXML;

my $article = $ARGV[0];


my @items_for_article = `ls $article/text/ckb*.miz | sed -e 's/\.miz//'`;
chomp @items_for_article;

foreach my $item (@items_for_article) {

    # now we start the brutalization proper
    my $item_xml_parser = XML::LibXML->new();
    my $item_xml_doc = $item_xml_parser->parse_file ("$item.xml1");
#    my @pid_bearers = $item_xml_doc->findnodes ('.//*[@pid > 0]');
    my @preds = $item_xml_doc->findnodes ('.//Pred');
    my @funcs = $item_xml_doc->findnodes ('.//Func');
    my @typs = $item_xml_doc->findnodes ('.//Typ');
    my @adjectives = $item_xml_doc->findnodes ('.//Adjective');
    my @patterns = $item_xml_doc->findnodes ('.//Pattern');

    my %constr_table = ();

    foreach my $pid_bearer (@preds, @funcs, @typs, @adjectives) {
      my $kind = $pid_bearer->findvalue ('@kind');
      my $nr = $pid_bearer->findvalue ('@nr');
      unless (defined $kind && defined $nr) {
  	die "We found a PID-bearing node in $item.eno that lacks either an AID, KIND, or NR attribute";
      }
      # warn "putting '$aid:$kind:$nr' into the pattern table";
      $constr_table{"$kind:$nr"} = 0;
    }

    foreach my $pattern_node (@patterns) {
      my $constrkind = $pattern_node->findvalue ('@constrkind');
      my $constrnr = $pattern_node->findvalue ('@constrnr');
      unless (defined $constrkind && defined $constrnr) {
  	die "We found a Pattern node in $item.eno that lacks either a CONSTRAID, CONSTRKIND, or CONSTRNR attribute";
      }
      # warn "putting '$constraid:$constrkind:$constrnr' into the pattern table";
      $constr_table{"$constrkind:$constrnr"} = 0;
    }

    my $xitemfile = "$item.aco";
    {
      open(XML, $xitemfile) 
  	or die "Unable to open an output filehandle for $xitemfile in directory !";
      local $/; $_ = <XML>;
      close(XML);
    }

    my ($xmlbeg,$xmlnodes,$xmlend) = $_ =~ m/(.*?)([<]Constructor\b.*[<]\/Constructor>)(.*)/s;

    open(XML1,'>', "$item.atr.pruned") 
      or die "Unable to open an output filehandle for $item.atr.pruned in directory  !";
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
}
