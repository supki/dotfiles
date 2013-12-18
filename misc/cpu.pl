#!/usr/bin/env perl

use v5.18;
use strict;
use warnings;

use List::Util qw(sum);
use List::MoreUtils qw(pairwise);

sub main() {
  my ($proc) = @ARGV;
  my @as = cpu_data($proc);
  sleep 1;
  my @bs = cpu_data($proc);

  printf "%2.f%%\n", cpu_utilization(\@as, \@bs);
}


sub cpu_data($) {
  my $path = shift;

  open my $stat, '<', $path;
  my @values = split ' ', <$stat>;
  close $stat;

  @values[1 .. $#values];
}

sub cpu_utilization($$) {
  my ($as, $bs) = @_;

  my @diff  = pairwise { $b - $a } @$as, @$bs;
  my $total = sum @diff;

  my $cpu_utilization = sum(map { $_ / $total } @diff[0..2]) * 100;
}

main
