#! /usr/bin/perl 
# tab2array -- convert table to C array
# Jim Raines, 31Jan00

open(IN, "../ref-data/swics-dvs2eoq.dat") or die "Cannot open input file.";

while(<IN>){
  #print "$_\n";
  if (/^\s*(\d+)\s+(\d+\.\d+)/) {
    printf("%f, ",$2);
  }
}
 
print "\n";
