#! /usr/bin/perl -I/home/winddata/sw/libsms 
# swics-rates -- print SWICS engineering rates
# Jim Raines, 22Dec99

use libsms;
use Getopt::Long;
#package libsms;

# define some stuff
$thisprog = "swics-rates";
$version = "0.1";

# get command line options
GetOptions("help!");

# print usage statement and exit
if ($opt_help){
  print <<EOF;
$thisprog: prints selected rates from WIND/SWICS
version: $version 
usage: $thisprog yyyymmdd [optional args]
  Processes WIND/SMS data file for year (yyyy), month (mm) and day (dd).

  optional args:
  --help             prints this message
EOF
  exit(0);
}

# set date based from 1st argument (or use default)
if (@ARGV >= 1) {
  $Date = $ARGV[0];
}
else {
  print "$thisprog: year and date required; use --help option for syntax\n";
  exit(1);
}

## open file
$File = "/home/winddata/LV1/wi_lz_sms_".$Date."_v01.dat";
# print "$File\n";
$result = libsms::smsOpenFile($File);

# open output file
open(OUT,">swics-rates.out") or die "Cannot open output file.";
print OUT "# generated automatically by $thisprog on ";
print OUT scalar localtime;
print OUT "\n";
print OUT "# from $File\n";

## set debug level and trace
$result = libsms::smssetdbg(0);
$result = libsms::smssettrace(0);

## read cycle
$ncycle = 0;
while ( ($result = libsms::smsReadCycle()) == 0) { # fix: 0 -> SMSSUCCESS
  $ncycle++;

  # print time string for cycle
  printf("%s: Cycle[%3.3d] time is %s\n",$thisprog,$ncycle,
	 libsms::smsgtimes()); 
  
  printf("%s: xfsr[0-59] ",$thisprog);
  for ($nedb=0; $nedb < 60; $nedb++) {
      printf("%3.3d ",libsms::smsgxfsr($nedb));
      printf OUT ("%3.3d ",libsms::smsgxfsr($nedb));
  }
  printf("\n");

  $ss1970 = libsms::smsgss1970();
  printf("%s: seconds since 1970 = %d\n",$thisprog, $ss1970);
  printf OUT ("%d\n",$ss1970);

  for ($nedb=0; $nedb < 60; $nedb++) {
    $nxpha = libsms::smsgxnpha($nedb);
    if ($nxpha > 0) {
      printf("EDB[%2.2d] nxpha=%d\n",$nedb,$nxpha);
      for ($i = 0; $i < $nxpha; $i++){
	printf("  PHA[%3.3d]: eoq=%f energy=%f\n",$i,
	       libsms::smsgxeoq($nedb,$i),libsms::smsgxenergy($nedb,$i));
      }
    }
  }

  # separate cycles
  printf("\n");
    
}

libsms::smsCloseFile();
close OUT;
