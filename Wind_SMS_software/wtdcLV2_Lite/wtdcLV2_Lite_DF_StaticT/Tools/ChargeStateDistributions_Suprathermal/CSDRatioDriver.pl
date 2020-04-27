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
print "Enter directory where the distribution function files are located:\n";
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

# Determine the solar wind cutoff velocity
print "Enter the cutoff velocity:\n";
print "(In normalized velocity, W)\n";
chomp ($wCutOff = <>);

# Determine a tag to add to the output file
print "Enter time period which distribution function files were calculated over, in format that processor outputs:\n";
print "(EX YYYYMMDD-YYYYMMDD)\n";
chomp ($timePeriod = <>);

#Determine the location of the SWE file
print "Enter location of the SWE file used in this calculation:\n";
print "(EX ../../../SWE_Files/YYYY_WIND_SWE_90Sec.dat)\n";
chomp ($sweFile = <>);

#print to file
open(OUT, ">"."driver.dat") or die "Cannot open driver.dat";
print OUT "$momFileDir\n";
print OUT "$atom\n";
print OUT "$atomNum\n";
print OUT "$wCutOff\n";
print OUT "$timePeriod\n";
print OUT "$sweFile\n";

close OUT;
print "Driver file has been wrote to driver.dat.  Process Complete!\n";