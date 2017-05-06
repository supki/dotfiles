#!/usr/bin/env perl

use v5.22;
use strict;
use warnings;

use File::Basename qw(dirname);
use Linux::Inotify2;

my $months = {
  January   => '01',
  February  => '02',
  March     => '03',
  April     => '04',
  May       => '05',
  June      => '06',
  July      => '07',
  August    => '08',
  September => '09',
  October   => '10',
  November  => '11',
  December  => '12',
};
my $month_matcher = join('|', keys(%$months));

my ($watched_dir) = @ARGV;
my $inotify = new Linux::Inotify2 or die "unable to create inotify object: $!";
$inotify->watch($watched_dir, IN_CREATE, sub {
  my ($e) = @_;
  my $oldname = $e->fullname;

  if ($e->name =~ /
    ^
    (?<name>[\w_]+)
    _
    (?<month>$month_matcher)
    _
    (?<year>\d+)
    (?<tag>_[^.]+)?
    (?<extension>\.[a-z0-9]+(?:\.[a-z0-9]+)?)
    $
    /x) {
    my $tag = defined($+{tag}) ? $+{tag} : '';
    my ($name, $year, $month, $extension) = @+{'name', 'year', 'month', 'extension'};
    my $month_idx = $months->{$month};
    my $newname = dirname($oldname) . "/${name}_${year}_${month_idx}${tag}${extension}";

    rename($oldname, $newname) or die "unable to rename $oldname to $newname: $!";
    say "$oldname -> $newname";
  } else {
    warn "unable to pattern match $oldname";
  }
}) or die "unable to create new watch: $!";

1 while $inotify->poll;
