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

# given an XML-like fragment such as
#
# <RCluster aid="CKB19" nr="1">
#
# determine what item this is
sub xml_to_item {
  my $xml_fragment = shift;
  $xml_fragment =~ m/aid=\"CKB([0-9]+)\"/;
  if (defined $1) {
    return $1;
  } else {
    $xml_fragment =~ m/aid=\"([^\"]+)\"/;
    if (defined $1) {
      return $1;
    } else {
      die "Can't handle line '$xml_fragment'";
    }
  }
}

my %item_to_cluster = ();

sub item_to_mptp { # look inside an item (e.g., ckb5.miz) and determine what its MPTP name should be
  my $item = shift;

  # warn "this is item $item";

  my $second_line = `head -n 2 $item.miz | tail -n 1`;
  chomp $second_line;

  if ($second_line =~ /RegistrationBlock/) {
    my $cluster_line = `grep --max-count=1 '<[RFC]Cluster' $item.miz`;
    chomp $cluster_line;
    $cluster_line =~ m/nr=\"([0-9]+)\"/;
    # warn "cluster line of item $item is $cluster_line";
    my $cluster_num = $1;
    if ($cluster_line =~ /CCluster/) {
      # warn "registering ccluster number $cluster_num";
      $item_to_cluster{$item} = "cc$cluster_num";
    } elsif ($cluster_line =~ /FCluster/) {
      # warn "registering fcluster number $cluster_num";
      $item_to_cluster{$item} = "fc$cluster_num";
    } else {
      # warn "registering rcluster number $cluster_num";
      $item_to_cluster{$item} = "rc$cluster_num";
    }
  }

  if ($second_line =~ /JustifiedTheorem/) {
    # warn "found a theorem";
    $second_line =~ m/nr=\"([0-9]+)\"/;
    my $theorem_num = $1;
    my @needed_clusters = `/mnt/sdb3/alama/mizar-items/items-needed-for-item.sh $item`;
    chomp @needed_clusters;
    my $num_needed_clusters = scalar @needed_clusters;
    # warn "we found $num_needed_clusters needed clusters";
    foreach my $needed_cluster (@needed_clusters) {
      # warn "a needed clsuter: $needed_cluster";
      my $dependent_item = xml_to_item ($needed_cluster);
      print 't', $theorem_num, '_', $article_name, ':', $dependent_item, "\n";
    }
  }

}

chdir $article_text_dir;
my @items = `find . -name "ckb*.miz" -type f`;
chomp @items;
my $num_items = scalar @items;
foreach my $i (1 .. $num_items) {
  item_to_mptp ("ckb$i");
}

exit 0;
