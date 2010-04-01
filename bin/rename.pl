#!/usr/bin/perl
# Rename script, originally by Larry Wall

use warnings;

$use = 'Usage Examples:
rename \'s/\\.orig$//\' *.orig
rename \'y/A-Z/a-z/ unless /^Make/\' *
rename \'$_ .= ".bad"\' *.f
rename \'print "$_: "; s/foo/bar/ if <stdin> =~ /^y/i\' *
';

if ($#ARGV == -1) {
  print $use;
} else {
  $op = shift;
  for (@ARGV) {
      $was = $_;
      eval $op;
      die $@ if $@;
      rename($was,$_) unless $was eq $_;
  }
}
