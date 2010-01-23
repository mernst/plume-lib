#!/usr/bin/env perl

# Reading the table of contents of a conference or journal is a useful way
# to keep up-to-date with the latest research.  ACM even provides a "TOC
# service" that emails the table of contents of conferences and journals
# that you specify.  A problem is that the tables of contents contain the
# title of each article, but not its abstract, so you have to click on
# every abstract link.
#
# This script takes as input a filename or URL for an ACM digital library
# proceedings table of contents.  It produces, to standard output, a HTML
# file that augments the table of contents with abstracts for each paper.
# You can use it on:
#  * a URL from the ACM digital library.  Example:
#      acm-dl-abstracts.pl 'http://portal.acm.org/toc.cfm?id=964001' > popl2004-abstracts-orig.html
#  * email from the ACM TOC Service.  Example (after saving to local file):
#      acm-dl-abstracts.pl acm.html
# Then, browse it or print it.  If printing, use
#   htmldoc --webpage -t ps --outfile acm-abstracts.ps acm-abstracts-*.html
# because both html2ps and the Filefox print functionality tend to cut off
# content.  (Maybe the ACM HTML is malformed, or maybe this script should be
# enhanced.)

# This script takes a long time to run (a minute is not unusual); be patient!


use strict;
use English;
$WARNING = 1;

use LWP::Simple;
use File::Temp qw/ :mktemp  /;

if (scalar(@ARGV) != 1) {
  die "Expected exactly 1 argument, got " . scalar(@ARGV);
}
my $toc_url = $ARGV[0];

if (-e $toc_url) {
  $toc_url = "file:" . $toc_url;
}
my $toc_html = get($toc_url);
if (! defined $toc_html) { die "Couldn't get $toc_url" }

my ($fh, $file) = mkstemps( "acm-abstracts-XXXX", ".html");


# Parens around regexp put them in the list, too.
my @toc_sections = split(/(<\/div>)/, $toc_html);
for my $toc_section (@toc_sections) {
  if ($toc_section !~ /<A HREF="([^<]*?)#abstract" target="_self">abstract<\/A>/) {
    print $fh "$toc_section\n";
    next;
  }
  my $paper_url = $1;
  # print "PAPER_URL: $paper_url\n";
  my $full_paper_url = $paper_url;
  # print "FULL_PAPER_URL: $full_paper_url\n";
  if ($full_paper_url !~ /^http:\/\/portal[0-9]*.acm.org(:80)?\//) {
    $full_paper_url = " http://portal.acm.org/$full_paper_url";
  }
  # print "FULL_PAPER_URL: $full_paper_url\n";
  my $paper_html = get($full_paper_url);
  if (! defined $paper_html) { die "Couldn't get $full_paper_url"; }

  # Parens around regexp put them in the list, too.
  my @paper_sections = split(/(<\/div>)/, $paper_html);
  my $appended = 0;
  for my $paper_section (@paper_sections) {
    # print $fh "<!-- PAPER_SECTION: $paper_section -->\n";
    if ($paper_section =~ /<A NAME="abstract">ABSTRACT<\/A>/) {
      $appended = 1;
      $toc_section .= $paper_section . "</div>";
      last;
    }
  }
  if (! $appended) {
    print $fh "<b>DID NOT FIND ABSTRACT!</b>\n";
  }
  print $fh "<!-- toc_section -->\n";
  print $fh "$toc_section\n";
}
