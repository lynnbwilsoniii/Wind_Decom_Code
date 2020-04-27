#! /usr/bin/perl

#############################################################################
# NAME: ionRatioDriver.pl
#
# Purpose: Creates a date file which will be used to drive the
#          Suprathermal Version of the Ionic Ratio code.
#
# Date: October 30, 2013   --- Initial coding
#############################################################################

use Getopt::Long;
use constant PI => 3.14159;

# Determine the distribution function file for the ion in the numerator
print "Enter location of 3-D distribution function file for the ion in the numerator:\n";
print "(EX: ../../Data/2h/wtdcLV2_Lite_SCFrame_H+_DF_20010101-20010101.dat)\n";
chomp ($dfFileTop = <>);
print "\n";

# Determine the distribution function file for the ion in the denominator
print "Enter location of 3-D distribution function file for the ion in the denominator:\n";
print "(EX: ../../Data/2h/wtdcLV2_Lite_SCFrame_He2+_DF_20010101-20010101.dat)\n";
chomp ($dfFileBottom = <>);
print "\n";

# Determine the solar wind cutoff velocity
print "Enter the cutoff velocity:\n";
print "(In normalized velocity, W)\n";
chomp ($wCutOff = <>);

#Determine the location of the SWE file
print "Enter location of the SWE file used in this calculation:\n";
print "(EX: ../../../SWE_Files/YYYY_WIND_SWE_90Sec.dat)\n";
chomp ($sweFile = <>);
print "\n";

# Determine a tag to add to the output file
print "Enter descriptive tag for the output file:\n";
print "(EX: H+_over_He2+_20010101-20010101)\n";
chomp ($tag = <>);
print "\n";

#print to file
open(OUT, ">"."driver.dat") or die "Cannot open driver.dat";
print OUT "$dfFileTop\n";
print OUT "$dfFileBottom\n";
print OUT "$wCutOff\n";
print OUT "$sweFile\n";
print OUT "$tag\n";
close OUT;

print "Driver file has been wrote to driver.dat.  Process Complete!\n";