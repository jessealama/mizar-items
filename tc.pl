#!/usr/bin/perl -w

use strict;

use Graph::TransitiveClosure::Matrix;
use Graph::AdjacencyMatrix;
use Graph::Directed; # or Undirected
use Graph::Writer::Dot;

my $dep_graph_file = $ARGV[0];

my $dep_graph  = Graph::Directed->new;

# read in the edges
warn "reading dep graph file";
open (GRAPH, '<', $dep_graph_file) or die "Can't open an input filehandle for $dep_graph_file";
my $num_edges = 0;
while (defined (my $line = <GRAPH>)) {
    chomp $line;
    my ($a,$b) = split (/ /, $line);
    $dep_graph->add_edge ($a, $b);
    $num_edges++;
}
close (GRAPH);

# Compute the transitive closure matrix.
warn "computing transitive closure...";
my $tcm = Graph::TransitiveClosure::Matrix->new($dep_graph, reflexive => 0);;
warn "...done";

my @tc_vertices = $tcm->vertices ();
# my @tc_edges = $tcm->edges ();
#print 'tc has ', scalar (@tc_vertices), ' vertices', "\n";
#print 'tc has ', scalar (@tc_edges), ' eges', "\n";

# warn "about to print dot output...";
# my $dot_writer = Graph::Writer::Dot->new ();
# $dot_writer->write_graph ($tcm, 'transitive-closure.dot');
# warn "...done";

my @tcm_array = @{$tcm};
#my $bm = $tcm_array[0];
#warn "\$bm is a ", ref($bm), "\n";

foreach my $vertex_a (@tc_vertices) {
  foreach my $vertex_b (@tc_vertices) {
    #warn $vertex_a, " ", $vertex_b, " ", $bm->get($vertex_a, $vertex_b);
    if ($tcm->is_reachable($vertex_a, $vertex_b)) {
      print "$vertex_a $vertex_b", "\n";
    }
  }
}

exit 0;
