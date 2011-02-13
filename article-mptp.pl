#!/usr/bin/perl -w

use strict;

my $article_name = shift (@ARGV);
my $article_dir = "/mnt/sdb3/alama/itemization/$article_name";

# sanity
unless (-e $article_dir) {
  die "No such file or directory: $article_dir!";
}
unless (-d $article_dir) {
  die "The file '$article_dir' isn't actually a directory!";
}

my $article_text_dir = "$article_dir/text";

# sanity
unless (-e $article_text_dir) {
  die "No such file or directory: $article_text_dir!";
}
unless (-d $article_dir) {
  die "The file '$article_text_dir' isn't actually a directory!";
}

my %item_num_to_cluster = ();

sub print_cluster_fragment {
  my $item = shift;
  # this is a theorem -- need to get its nr
  my $second_line = `head -n 2 $item.miz | tail -n 1`;
  chomp $second_line;
  # warn "second line is $second_line";
  $second_line =~ m/nr=\"([0-9]+)\"/;
  my $theorem_nr = $1;
  my @xml_lines = `/mnt/sdb3/alama/mizar-items/items-needed-for-item.sh $item`;
  if (scalar (@xml_lines) == 0) {
    print "t", $theorem_nr, "_", $article_name, " : NOTHING", "\n";
  } else {
    foreach my $xml_fragment (@xml_lines) {
      chomp $xml_fragment;
      unless ($xml_fragment eq '') {
	$xml_fragment =~ m/([RFC])Cluster aid=\"([^\"]+)\" nr=\"([0-9]+)\"/;
	my $kind = $1;
	my $aid = lc $2;
	my $nr = $3;
	
	print "t", $theorem_nr, "_", $article_name, " : ";
	
	if ($aid =~ /^ckb[0-9]+/) { # local case
	  $aid =~ /ckb([0-9]+)/;
	  my $item_num = $1;
	  my $kind_cluster_nr = $item_num_to_cluster{$item_num};
	  my ($kind,$cluster_nr) = split (':', $kind_cluster_nr);
	  print $kind, "c", $cluster_nr, "_", $article_name, "\n";
	} else {
	  if ($kind eq "R") {
	    print "r";
	  } elsif ($kind eq "F") {
	    print "f";
	  } else {
	    print "c";
	  }
	  print "c", $nr, "_", $aid, "\n";
	}
      }
    }
  }
}
  
chdir $article_text_dir;
my @items = `find . -name "ckb*.miz" -type f | sed -e 's/.miz//'`;
chomp @items;
my $num_items = scalar @items;
foreach my $i (1 .. $num_items) {
  my $item = "ckb$i";
  my $second_line = `head -n 2 $item.miz | tail -n 1`;
  chomp $second_line;
  if ($second_line =~ /RegistrationBlock/) {
    my $cluster_line = `grep --max-count=1 '[RFC]Cluster' $item.miz`;
    chomp $cluster_line;
    $cluster_line =~ m/([RFC])Cluster/;
    my $kind = lc $1;
    $cluster_line =~ m/nr=\"([0-9]+)\"/;
    my $cluster_nr = $1;
    # warn "item $i of $article_name is cluster kind $kind, number $cluster_nr";
    $item_num_to_cluster{$i} = "$kind:$cluster_nr";
  } elsif ($second_line =~ /JustifiedTheorem/) {
    print_cluster_fragment ("ckb$i");
  }
}
