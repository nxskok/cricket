use strict;
use warnings;
use 5.010;

use LWP::Simple;
use HTML::TableExtract;
use HTML::TreeBuilder;
use Data::Dumper;

main(@ARGV);

sub main {
  my ($url)=@_;
  # url actually goes to scorecard
  my $urlvc="$url?view=comparison";
  my $document=get($urlvc);
  my $root=HTML::TreeBuilder->new_from_content($document);
  # div class team-i-name if match going on
  # if finished, team 1 is A v B
  # result string is div class="innings-requirement"
  # decode to find which team batted 1st and which 2nd (what about D/L?)

  # or look at scorecard
  # th class="th-innings-heading" text of
  
  my $t1name;
  my $t2name;
  my @t=$root->find_by_tag_name('div');
  for my $t (@t) {
    my $c=$t->attr('class');
    next unless $c;
    if ($c eq 'team-1-name') {
      $t1name=$t->as_text();
    }
    if ($c eq 'team-2-name') {
      $t2name=$t->as_text();
    }
  }
  # remove score from end, if any
  for ($t1name,$t2name) {
    s/[0-9\/ ()ov.]+$//;
  }
  #say "team 1: $t1name";
  #say "team 2: $t2name";
  if (!defined $t2name) {
    # go to scorecard
    my $sc=get($url);
    my $rootsc=HTML::TreeBuilder->new_from_content($sc);
    my @th=$rootsc->find_by_tag_name("th");
    my @teams;
    for my $th (@th) {
      my $c=$th->attr('class');
      next unless $c eq "th-innings-heading";
      my $this=$th->as_text();
      next if $this eq "Bowling";
      if ($this=~/(.*)\sinnings/) {
	push @teams, $1;
      }
    }
    $t1name=$teams[0];
    $t2name=$teams[1];
  }
  my $tabs=HTML::TableExtract->new( );
  $tabs->parse($document);
  # Examine all matching tables
  foreach my $ts ($tabs->tables) {
    print "Table (", join(',', $ts->coords), "):\n";
    foreach my $row ($ts->rows) {
      no warnings "uninitialized";
      print join(',', @$row), "\n";
      use warnings;
    }
  }
  # make a file with team names in it, called teams.txt
  open my $out, ">", "teams.txt" or die "Cannot open teams file: $!";
  say $out "teams";
  say $out $t1name;
  say $out $t2name;
  close $out;
}
