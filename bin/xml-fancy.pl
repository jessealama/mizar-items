#!/usr/bin/perl -w

use strict;

sub nr_attribute {
  my $xml_line = shift;
  $xml_line =~ / nr=\"([0-9]+)\"/;
  return $1;
}

sub constrnr_attribute {
  my $xml_line = shift;
  $xml_line =~ / constrnr=\"([0-9]+)\"/;
  return $1;
}

sub defnr_attribute {
  my $xml_line = shift;
  $xml_line =~ / defnr=\"([0-9]+)\"/;
  return $1;
}

sub absconstrnr_attribute {
  my $xml_line = shift;
  $xml_line =~ / absconstrnr=\"([0-9]+)\"/;
  return $1;
}

sub aid_attribute {
  my $xml_line = shift;
  $xml_line =~ / aid=\"(\w+)\"/;
  return $1;
}

sub schemenr_attribute {
  my $xml_line = shift;
  $xml_line =~ / schemenr=\"([0-9]+)\"/;
  return $1;
}

sub kind_attribute {
  my $xml_line = shift;
  $xml_line =~ / kind=\"(\w)\"/;
  return $1;
}

sub constrkind_attribute {
  my $xml_line = shift;
  $xml_line =~ / constrkind=\"(\w)\"/;
  return $1;
}

sub xml_to_fancy {
  my $xml_fragment = shift;
  my $aid = aid_attribute ($xml_fragment);
  my $aid_lc = lc $aid;
  my $kind = kind_attribute ($xml_fragment);
  my $kind_lc = lc $kind;
  my $nr = nr_attribute ($xml_fragment);
  if ($xml_fragment =~ /<Theorem /) {
    if ($kind_lc eq 't') {
      return "$aid_lc:theorem:$nr";
    } else {
      return "$aid_lc:deftheorem:$nr";
    }
  } elsif ($xml_fragment =~ /<Pattern /) {
    return ("$aid_lc:$kind_lc" . 'pattern:' . $nr);
  } elsif ($xml_fragment =~ /<Constructor /) {
    return ("$aid_lc:$kind_lc" . 'constructor:' . $nr);
  } elsif ($xml_fragment =~ /<([RCF])Cluster /) {
    my $cluster_kind = $1;
    my $cluster_kind_lc = lc $cluster_kind;
    return ("$aid_lc:$cluster_kind_lc" . 'cluster:' . $nr);
  } elsif ($xml_fragment =~ /<Definiens /) {
    my $def_nr = defnr_attribute ($xml_fragment);
    my $constr_kind = constrkind_attribute ($xml_fragment);
    my $constr_kind_lc = lc $constr_kind;
    return ("$aid_lc:$constr_kind_lc" . 'definiens:' . $def_nr);
  } else {
    warn "Don't know how to handle XML fragment", "$xml_fragment";
    return "";
  }
}

while (defined (my $line = <STDIN>)) {
  chomp $line;
  my $fancy = xml_to_fancy ($line);
  print $fancy, "\n";
}

exit 0;
