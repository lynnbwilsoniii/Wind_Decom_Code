#! /usr/bin/perl

#############################################################################
# NAME: CSDRatioDriver.pl
#
# Purpose: Creates a date file which will be used to drive the Ionic Ratio code.
#
# Date: October 30, 2013   --- Initial coding
#############################################################################

use Getopt::Long;
use constant PI => 3.14159;

# Determine the moment file for the ion in the numerator
print "Enter directory where moment files (nvt fully integrated ones) are located:\n";
print "(EX ../../Data/2h/)\n";
chomp ($momFileDir = <>);

# Determine the moment file for the ion in the denominator
print "Enter the atomic element you would like to process:\n";
print "(EX He)\n";
chomp ($atom = <>);

# Determine the atomic number
print "Enter the atomic number for the element you would like to process:\n";
print "(EX 2)\n";
chomp ($atomNum = <>);

# Determine a tag to add to the output file
print "Enter time period which moment files were calculated over, in format that processor outputs:\n";
print "(EX YYYYMMDD-YYYYMMDD)\n";
chomp ($timePeriod = <>);

#print to file
open(OUT, ">"."driver.dat") or die "Cannot open driver.dat";
print OUT "$momFileDir\n";
print OUT "$atom\n";
print OUT "$atomNum\n";
print OUT "$timePeriod\n";

close OUT;
print "Driver file has been wrote to driver.dat.  Process Complete!\n";