#!/usr/bin/env perl
# system_or_die.pm -- Perl utilities for calling external programs
# The externally-visible procedures are listed in the @EXPORT statement.

package system_or_die;
require 5.003;			# uses prototypes
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw( system_or_die backticks_or_die );

use English;
use strict;
$WARNING = 1;			# "-w" flag

use checkargs;
use Carp;

# Execute the command; die if its execution is erroneous.
# If optional second argument is non-zero, print the command to standard out,
# which may be helpful for indicating progress.
sub system_or_die ( $;$ ) {
  my ($command, $verbose) = check_args_range(1, 2, @_);
  if ($verbose) { print "$command\n"; }
  my $result = system($command);
  if ($result != 0) { croak "Failed executing $command"; }
  return $result;
}

# Execute the command and return the output; die if its execution is erroneous.
# If optional second argument is non-zero, print the command to standard out,
# which may be helpful for indicating progress.
sub backticks_or_die ( $;$ ) {
  my ($command, $verbose) = check_args_range(1, 2, @_);
  if ($verbose) { print "$command\n"; }
  if (wantarray) {
    my @result = `$command`;
    if ($CHILD_ERROR != 0) { croak "Failed executing $command"; }
    return @result;
  } else {
    my $result = `$command`;
    if ($CHILD_ERROR != 0) { croak "Failed executing $command"; }
    return $result;
  }
}


###########################################################################
### End of file
###

# Return true to indicate success loading this package.
1;
