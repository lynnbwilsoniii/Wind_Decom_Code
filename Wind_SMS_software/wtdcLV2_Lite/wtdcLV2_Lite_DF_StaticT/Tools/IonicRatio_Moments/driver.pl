#! /usr/bin/perl

#############################################################################
# NAME: ionRatioDriver.pl
#
# Purpose: Creates a date file which will be used to drive the Ionic Ratio code.
#
# Date: October 30, 2013   --- Initial coding
#############################################################################

use Getopt::Long;
use constant PI => 3.14159;

# Determine the moment file for the ion in the numerator
print "Enter location of moment (with nvt output) file for the ion in the numerator:\n";
print "(EX ../../Data/2h/wtdcLV2_Lite_nvt_H+_20010101-20010101.dat)\n";
chomp ($momFileTop = <>);

# Determine the moment file for the ion in the denominator
print "Enter location of moment file (with nvt output) for the ion in the denominator:\n";
print "(EX ../../Data/2h/wtdcLV2_Lite_nvt_He2+_20010101-20010101.dat)\n";
chomp ($momFileBottom = <>);

# Determine a tag to add to the output file
print "Enter descriptive tag for the output file:\n";
print "(EX H+_over_He2+_20010101-20010101)\n";
chomp ($tag = <>);

#print to file
open(OUT, ">"."driver.dat") or die "Cannot open driver.dat";
print OUT "$momFileTop\n";
print OUT "$momFileBottom\n";
print OUT "$tag\n";

close OUT;
print "Driver file has been wrote to driver.dat.  Process Complete!\n";