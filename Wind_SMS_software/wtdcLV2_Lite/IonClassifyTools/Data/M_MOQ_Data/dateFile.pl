#! /usr/bin/perl

#############################################################################
# NAME: dateFile.pl
#
# Purpose: Creates a date file which will be used to run the PHAaccum C++ code.
#
# Date: September 14, 2009   --- Initial coding
#############################################################################

use Getopt::Long;
use constant PI => 3.14159;

$LV1DIR = "/shrg1/wind/LV1";
@badDays = ("19960529","19960710","19960804","19961230",
	    "19971021",
	    "19980702","19980703","19981113",
	    "19990223","19990314","19990329","19990421","19990422","19990818","19990908",
	    "19990909","19990930","19991114","19991130","19991201","19991202",
	    "20000409","20000410","20000502","20000503","20000525","20000610","20000626",
	    "20000710","20000723","20000724","20000804","20000815","20000816",
	    "20010221","20010223","20010410","20010411","20010805","20010816","20010817",
	    "20010818","20011003","20011004","20011114","20011202","20011203",
	    "20020524","20020721","20020809","20020810","20020829","20020830","20020918",
	    "20021009","20021010","20021128",
	    "20030814","20030815","20031018","20031020","20031021","20031022","20031023",
	    "20031024","20031026","20031027","20031028","20031029","20031030","20031102",
	    "20031121","20031203","20031221","20031222","20031223","20031224","20031225",
	    "20031227",
	    "20040101","20040102","20040103","20040104","20040105","20040106","20040107",
	    "20040108","20040109","20040110","20040111","20040112","20040113","20040114",
	    "20040115","20040116","20040117","20040118","20040119","20040120","20040121",
	    "20040122","20040123","20040124","20040125","20040126","20040127","20040128",
	    "20040202","20040203","20040205","20040209","20040210","20040211","20040421",
	    "20040423","20040425","20040426",
	    "20050121","20050601","20050711","20050731",
	    "20060501",
	    "20071003","20071112","20071203",
	    "20090711");

# Determine the ion to sort by
print "Select which ion you want accumulate: \n";
chomp ($ionName = <>);

print "Input Number of Points Required to Print: \n";
chomp ($numCycles = <>);

# Determine if the distribution function will be transformed
print "Turn on solar wind frame transformtion (1 = yes, 0 = no): \n";
chomp ($swFrame = <>);

# Determine type of DOY entry and populate the Date array
print "Select type of time span: \n";
print " 1: Full year \n";
print " 2: Single Day \n";
print " 3: Data file of days \n";
chomp ($doyType = <>);

if ($doyType == 1) {
	#processes full year
	print "Going to process full year.  Please enter Year: ";
	chomp($Date = <>);
	print "Warning! Ignores differing versions.  Accumulates All!!! \n";
	# Find highest version of data file
	$base = "$LV1DIR/$Date/wi_lz_sms_".$Date;
	#$base = "$LV1DIR/$year/";
	$ext = ".dat";
	@Files = glob("$base*.dat");  # returns all versions of file	
      	$numFiles = @Files;
	$year = $Date
	}
elsif ($doyType == 2) {
	#processes single day
	print "Going to process single day.  Please enter Day: ";
	chomp($Date = <>);
	# parse year out of date
	$Date =~ /(\d{4})(\d{2})(\d{2})/;
	$year = $1;
	$month = $2;
	$day = $3;

	# Find highest version of data file
	$base = "$LV1DIR/$year/wi_lz_sms_".$Date."_v";
	#$base = "$LV1DIR/$year/";
       	$ext = ".dat";
	@Files = glob("$base*.dat");  # returns all versions of file
        $Files = $Files[-1]; # takes last element of list which is highest version
	$numFiles = 1;
}
elsif ($doyType == 3) {
	#processes doy span from data file
	print "Going to process data file of DOY.  Please enter file name: ";
	chomp($fileNameDOY = <>);
	open(FILEDOY, "<", "$fileNameDOY");
	$i = 0;
	while ($line = <FILEDOY>) {
		$line += 0;
		$Date_temp = $line;
		$Date_temp =~ /(\d{4})(\d{2})(\d{2})/;
		$year = $1;
		$month = $2;
		$day = $3;
		# Find highest version of data file
		$base = "$LV1DIR/$year/wi_lz_sms_".$Date_temp."_v";
      		@Files_temp = glob("$base*.dat");
		$Files[$i] = $Files_temp[-1];
		$i++;
	}
	$numFiles = @Files;
	close FILEDOY
}

# Check to see if @Files has any Bad Day In them before Output
$j = 0;
for ($i = 0; $i < $numFiles; $i++){
    @Files[$i] =~ /.{31}(\d{8})/;
    if (( @found = grep(/\b$1\b/, @badDays))) {
	print "Skipping $1 --- Bad STICS data \n";
    } else {
	@outFiles[$j] = @Files[$i];
	$j++;
    }
}

# Write these file names to a data file
$numFiles = @outFiles;
open(OUT, ">"."dateFile.dat") or die "Cannot open dateFile.dat";
print OUT "$numFiles\n";
print OUT "$ionName\n";
print OUT "$numCycles\n";
print OUT "$swFrame\n";
for ($i = 0; $i < $numFiles; $i++){
    print OUT  "@outFiles[$i]\n";
}
close OUT;

print "Date File has been wrote to dateFile.dat.  Process Complete!\n";
