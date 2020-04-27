#! /usr/bin/perl
# File:  wmpha.pl
#
# Description:
#   Dump PHA words from WIND/MASS.
#
# Author(s):  Jim Raines (jmr)
#
# Method:
#
# Revision controlled by CVS:
# $Id: wmpha.pl,v 1.1 2005/12/01 17:35:44 jfeeman Exp $
#
# Major Modification History:
#   29Nov2005   jmr   initial coding
#

use libsms;
use Getopt::Long;

# ----------------------------------------------------------------------
# init. variables
# ----------------------------------------------------------------------
$LV1DIR = $ENV{WINDLV1};
$thisprog = 'wmpha';

# print banner
print <<EOF;
$thisprog: Extracts WIND/MASS PHA data items and outputs in ASCII format
revision \$Id: wmpha.pl,v 1.1 2005/12/01 17:35:44 jfeeman Exp $ \n

EOF

# ----------------------------------------------------------------------
# ===> configure <===
# ----------------------------------------------------------------------
$FILL = -1.0;	# fill value for missing or omitted data


# ----------------------------------------------------------------------
# handle arguments
# ----------------------------------------------------------------------
GetOptions("help!");

# print usage statement and exit
if ($opt_help){
  print <<EOF;

usage: $thisprog yyyymmdd [optional args]
  Processes WIND/SMS data file for yyyymmdd.

  optional args:
  --debug=i          set libsms debug level to i (see libsms.html)
  --trace=i          set libsms trace to i (0 ==> off; 1 ==> on)  
                     (Outputs messages at each stage; see libsms.html.)
  --help             prints this message
EOF
  exit(0);
}

# set date based from 1st argument (or use default)
if (@ARGV >= 1) {
  $Date = $ARGV[0];
  $Date =~ /(\d{4})(\d{2})(\d{2})/;  # split out year
  $year = $1;
}
else {
  print "$thisprog: year and date required; use --help option for syntax\n";
  exit(1);
}
# ----------------------------------------------------------------------
# find and open input file
# ----------------------------------------------------------------------
# Find highest version of data file
## prototypical filename: wi_lz_sms_20000229_v01.dat
$base = "$LV1DIR/$year/wi_lz_sms_".$Date."_v";
$ext = ".dat";
@Files = glob("$base*.dat");  # returns all versions of file
$File = $Files[-1]; # takes last element of list which is highest version
print "$thisprog: will read from $File\n";

# open input file
$result = libsms::smsOpenFile($File);

# ----------------------------------------------------------------------
# open and init output file
# ----------------------------------------------------------------------

# -------------------- Rate file --------------------
$outfile = $thisprog."_rates_$Date.dat";
open(OUT,">".$outfile) or die "Cannot open $outfile.";

print "$thisprog: will output to $outfile\n";

# headers
print OUT "# Rate items from WIND/STICS\n";
print OUT "# File: $File\n";
print OUT "# Generated by $thisprog.pl on ";
print OUT scalar localtime;
printf OUT (", libsms v. %s\n", libsms::smsgver()); 
print OUT "# Columns headings:\n";

# column headings
$hdrfmt = "# %5s %4s %7s %3s %5s %8s %8s %8s %8s\n";
$datfmt = "  %5.5d %4.4d %7.3f %3d %5.2f %8d %8d %8d %8d\n";
printf OUT ($hdrfmt, 'srec','year','doyfr','dvs','e/q','fsr','dcr',
	    'br0','br1');


# -------------------- PHA file --------------------
$outfile = $thisprog."_pha_$Date.dat";
open(OUTPHA,">".$outfile) or die "Cannot open $outfile.";

print "$thisprog: will output to $outfile\n";

# headers
print OUTPHA "# PHA items from WIND/STICS\n";
print OUTPHA "# File: $File\n";
print OUTPHA "# Generated by $thisprog.pl on ";
print OUTPHA scalar localtime;
printf OUTPHA (", libsms v. %s\n", libsms::smsgver()); 
print OUTPHA "# Columns headings:\n";

# column headings
$hdrfmt_pha = "# %5s %4s %7s %3s %5s %6s %3s %5s\n";
$datfmt_pha = "  %5.5d %4.4d %7.3f %3d %5.2f %6d %3d %5.2f\n";
printf OUTPHA ($hdrfmt_pha, 'srec','year','doyfr','dvs','e/q','tofch',
	   'rng','wgt');

# ----------------------------------------------------------------------
# loop over cycles
# ----------------------------------------------------------------------
## read cycle
$ncycle = 0;
while ( ($result = libsms::smsReadCycle()) == 0) { # fix: 0 -> SMSSUCCESS
  $ncycle++;

  # get time string for cycle
  printf("%s: Cycle[%3.3d] time is %s\n",$thisprog,$ncycle,
	 libsms::smsgtimes()); 

  # calculate doy fraction (86400.e3 msec/day)
  $doyfr = libsms::smsgdoy() + libsms::smsgtime()/86400.e3;
			
  for ($nedb=0; $nedb < 60; $nedb++) {
    # EDB header info
    #printf("%s: EDB[%2.2d] time is %s",$thisprog,$nedb,
    #   smsgtimes()); # get time string for cycle

    # -------------------- Rate output --------------------
    printf OUT ($datfmt, 
		libsms::smsgscirec(),
		libsms::smsgyear(),
		$doyfr,
		libsms::smsgmdvs($nedb),
		libsms::smsgmeqtab($nedb, 1),
		libsms::smsgmfsr($nedb,1),
		libsms::smsgmdcr_s($nedb),
		libsms::smsgmbr0_s($nedb),
		libsms::smsgmbr1_s($nedb)
	       );

    # -------------------- PHA output --------------------
    if (libsms::smsgmnpha($nedb) >= 1){

      # init range and wgt arrays
      @num_pha = 0 x 2;
      @num_pha = (0, 0);
      @wgt     = 0 x 2;
      @wgt     = (0, 0);

      # -- calculate basic rate weighting ---
      # sum PHAs in each range
      for ($npha = 0; $npha < libsms::smsgmnpha($nedb); $npha++){
	$range = libsms::smsgmrange($nedb,$npha);
	if ($range == 0  || $range == 1) {
	  $num_pha[$range]++;
	}
      }

      # Note:
      # $br can sometimes be < $num_pha[], though technically
      # this is impossible.  This must be akin to some sort of round
      # off error as it only happens when the rates are very small.
      #
      # To prevent the non-sensical wgt < 1.0, wgt is set to 1.0 in this case.

      $br = libsms::smsgmbr0_s($nedb);
      if ($br > 0 && $br < $num_pha[0]) {$wgt[0] = $num_pha[0] / $br}
      else {$wgt[0] = 1.0}

      $br = libsms::smsgmbr1_s($nedb);
      if ($br > 0 && $br < $num_pha[1] ) {$wgt[1] = $num_pha[1] / $br}
      else {$wgt[1] = 1.0}

      # output PHA words (including basic rate weight)
      for ($npha = 0; $npha < libsms::smsgmnpha($nedb); $npha++){
	$range = libsms::smsgmrange($nedb,$npha);
	if ($range == 0  || $range == 1) {
	  printf OUTPHA 
	    ($datfmt_pha,
	     libsms::smsgscirec(),
	     libsms::smsgyear(),
	     $doyfr,
	     libsms::smsgmdvs($nedb),
	     libsms::smsgmeqtab($nedb, 1),
	     libsms::smsgmtof($nedb,$npha),
	     $range,
	     $wgt[$range]
	    );
	}
      } # loop over PHA
    } 

  } # loop of EDBs
} # loop over cycles



# ----------------------------------------------------------------------
# clean up and exit
# ----------------------------------------------------------------------
libsms::smsCloseFile();
close OUT;
close OUTPHA;

