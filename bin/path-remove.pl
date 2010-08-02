#!/usr/bin/env perl

# Clean up a path environment variable by removing duplicates and
# non-existent directories.
# With optional argument "-r REGEXP", removes any matching path element.
# Works for either space- or colon- delimiated paths.
# Usage:
#   # csh
#   setenv PATH `echo $PATH | $HOME/bin/share/path-remove.pl`
#   # sh
#   set path = (`echo $path | $HOME/bin/share/path-remove.pl`)

my $debug = 0;
# $debug = 1;

my $extra_regexp;               # undefined if not specified on command line.

while (scalar(@ARGV) > 0) {
  if ($ARGV[0] eq "-r") {
    if (scalar(@ARGV) < 2) {
      die "No regexp specified after -r";
    }
    shift @ARGV;
    $extra_regexp = shift @ARGV;
  } else {
    die "Unrecognized argument $ARGV[1]";
  }
}

$splitchar = ":";
@result = ();
while (<>) {
      chomp;
      if (/:/) {
	split(":");
	$splitchar = ":";
      } elsif (/ /) {
	split(" ");
	$splitchar = " ";
      } else {
	# no separators; assume colon, but don't set splitchar.
	split(":");
      }
      if ($debug) {
	print STDERR "splitchar: $splitchar\n";
	print STDERR "" . scalar(@_) . " initial components\n";
      }
      foreach $temp (@_) {
	if (! -e $temp) { next; }
	if (defined($already_seen{$temp})) { next; }
        if ($temp =~ /vortex\/(M3|Smalltalk)\/bin\/shell$/) { next; }
        if (defined($extra_regexp) && ($temp =~ /$extra_regexp/)) { next; }
        # This directory should appear in the output
        push(@result, $temp);
        $already_seen{$temp} = 1;
      }
}

if ($debug) {
  print STDERR "" . scalar(@result) . " final components\n";
}

print join($splitchar, @result);
