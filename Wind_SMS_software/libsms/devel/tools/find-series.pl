#! /usr/bin/perl 
# find-series.pl -- find a series of numbers in UMD WIND PHA output
# usage: find-series.pl <series>

$thisprog = "find-series";

# check args
if (@ARGV >= 1) {
  $tomatch = $ARGV[0];
}
else {
  print "usage: $thisprog pattern\n";
  exit(1);
}

# input file
open(IN,"</home/jraines/wind/libsms/ref-data/jim990618.dat") or 
  die "Cannot open input file";

# slurp time an another field (to compare field, tcf) from file into arrays
$nline = 0;
$tcfs = "";
while (<IN>) {
  if (/^\s+\d{5}/){ # only lines where first field is 5 digit number qualify
    
    @line = split(/[ \s]+/,$_); # split by tabs or spaces
    #print "line=@line\n";

    $time[$nline] = $line[5];

    # ===> edit here to select other field to grab
    # NOTE: Add one to the count because $line[0] is just space
    $tcf[$nline] = $line[8]; # 7 is stopId
    $tcfs = $tcfs.$line[8];

    #print "time[$nline]=$time[$nline] tcfs=$tcfs\n";

    # increment line number
    $nline++;
  }
}

# find pattern in tcf array and print
if (0) {
  print "Using pattern matching method...\n";
   #print "tcfs=$tcfs\n";
   if ($tcfs =~ m/$tomatch/g) {
     $i = length($PREMATCH);
     print "String $MATCH matches beginning at time $time[$i]\n";
     

   }
   else {
     print "No matches.\n";
   }
}
else {
  print "Using index method...\n";
  $index = index($tcfs, $tomatch);
  if ($index != -1) {
    printf("String %s matches beginning at time %s\n",
	   substr( $tcfs,$index,length($tomatch)),$time[$index]);
  }
  else {
    print "No matches found.\n";
  }
}
    
