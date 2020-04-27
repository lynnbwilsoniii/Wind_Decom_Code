#! /usr/bin/perl
# getmembers.pl -- change a structure declaration into a list of members
# Jim Raines, 29Nov99
#
# Note:  Only works for simple structure.

open(IN, $ARGV[0]) or die "Cannot open $ARGV[0].\n";
$go = 0;
$got = 0;

while (<IN>) {
  if (/struct edbheader_dc \{/){
    $go = 1; 
    print "enter structure\n";
}
  if ($go) {
    # print "$_";
  }

  if ($go && /int (\w+)/ ) {
    printf("\"%s\", ", $1);
    $got++;
  }

  if ($go && /^\};/) {
    $go = 0;
    print "\nexit structure -- got $got members\n";
  }
}

print "\n";
