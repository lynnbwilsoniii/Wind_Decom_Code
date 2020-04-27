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
	    "20090711",
	    "20110504","20110505","20110506","20110507","20110508","20110509","20110510",
	    "20110511","20110512","20110513","20110514","20110515");

# Determine the ion to sort by
print "Select which ion you want accumulate: \n";
chomp ($ionName = <>);
print "\n";

# Determine the number of velocity bins which need to be satisfied to output to file
print "Input number of velocity bins to satisfy error threshold:\n";
chomp ($numBins = <>);
print "\n";

# Determine the required error threshold in order to output to file
print "Input measurement error threshold used in the adaptive cadence:\n";
print "(between 0.0 and 1.0)\n";
chomp ($errThresh = <>);
print "\n";

# Determine the velocity threshold for the adaptive cadence check
print "Input velocity threshold, in km/s:\n";
print "(only bins of velocities greater than this value will be considered for the adaptive cadence error check)\n";
chomp ($velThresh = <>);
print "\n";

#Determine the efficiency threshold
print "Input the desired efficiency threshold.\n";
print "(Only particles with EOQ experiencing a total efficiency greater than this value are considered. By default we use a value of 0.15):\n";
chomp ($effThresh = <>);
print "\n";

#Determine if the user wants to compress the data from all azimuthal components to only 1d
print "Compress data into N observation directions:\n";
print "(0 - 3-d Data; 1 - 2-d Only Seperated by Sector; 2 - 1d Compressed Data)\n";
chomp ($dirBol = <>);
print "\n";

#Determine the method for ion selection
print "Method of ion selection:\n";
print "(0 - TOF binning, 1 - Mass-MOQ binning):\n";
chomp ($selBol = <>);
print "\n";

#Determine the output data type
print "Format of data to output:\n";
print "(0 - Counts, 1 - Distribution Function, 2 - Differntial Flux):\n";
chomp ($dataOutput = <>);
print "\n";

#Determine if the user would like to create a moment file. Note this will only be seen 
#if the user is outputting a distribution function. If not, then we won't calculate the moments
if ($dataOutput == 1) {
	print "Create a file containing the density, velocity, and thermal velocity calculated from moments of the distribution function:\n";
	print "Additionally, a file containing the 0th, 1st, and 2nd order moments for each sector will be created.\n";
	print "(0 - no; 1 - yes)\n";
	chomp ($momentFile = <>);
} else {
	$momentFile = 0;
}
print "\n";

#Determine if the velocity cutoff for the suprathermal data. This only really matters if the moment files are being output
if ($momentFile == 1) {
	print "Enter the desired cutoff velocity (in W, normalized velocity) to be used during the moment calculation.\n";
	print "Only particles with velocities greater than this value will be consider. If no cut off is needed, enter 0.0:\n";
	chomp ($velCutOff = <>);
} else {
	$velCutOff = 0.0;
}
print "\n";

#Determine the location of the SWE file
print "Enter location of the SWE file used in this calculation:\n";
print "(EX: ../SWE_Files/YYYY_WIND_SWE_90Sec.dat)\n";
chomp ($sweFile = <>);
print "\n";

#Determine the output directory for the resulting files
print "Directory to output data files:\n";
print "(EX: Data/) \n";
chomp ($dirOutput = <>);
print "\n";

#Put in a fill value, since C++ gets angry with any empty directory
#This will allow the user to output the data file to the working directory
if ($dirOutput eq "") {
	$dirOutput = "WORKINGDIRECTORY";
}

# Determine type of DOY entry and populate the Date array
print "Select type of time span: \n";
print " 1: Full year \n";
print " 2: Single Day \n";
print " 3: Data file of days \n";
print " 4: Mission \n";
chomp ($doyType = <>);
print "\n";

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
elsif ($doyType == 4) {
    #processes entire mission
    print "Going to process full year.  Please enter Year: ";
    @Date = ("1996","1997","1998","1999","2000","2001","2002","2003","2004",
	     "2005","2006","2007","2008","2009","2010","2011","2012");
    print "Warning! Ignores differing versions.  Accumulates All!!! \n";
    for ($i = 0; $i<16; $i++) { 
      # Find highest version of data file
      $base = "$LV1DIR/@Date[$i]/wi_lz_sms_".@Date[$i];
      #$base = "$LV1DIR/$year/";
      $ext = ".dat";
      if ($i  == 0) {
	@Files = glob("$base*.dat");  # returns all versions of file
      } else {
	@Files2 = glob("$base*.dat");
	push(@Files, @Files2);
      }
    }
    $numFiles = @Files;
    $year = $Date
}

# Check to see if @Files has any Bad Day In them before Output
$j = 0;
for ($i = 0; $i < $numFiles; $i++){
    @Files[$i] =~ /.{31}(\d{8})/;
    if (( @found = grep(/\b$1\b/, @badDays))) {
	print "Skipping $1 --- Bad STICS data \n";
    } else {
	@outFilesTemp[$j] = @Files[$i];
	$j++;
    }
}

# Make sure highest version of files is only left
$j = 0;
$numFiles = @outFilesTemp;
for ($i = 0; $i < $numFiles; $i++) {
  $limit = $numFiles-1;
  if ($i < $limit) {
    @outFilesTemp[$i] =~ /(.{40})(.{3})/;
    $currentBase = $1;
    $currentVer = $2;
    $nextInd = $i+1;
    @outFilesTemp[$nextInd] =~ /(.{40})(.{3})/;
    $nextBase = $1;
    $nextVer  = $2;
    if ($currentBase eq $nextBase) {
      print "Skipping @outFilesTemp[$i] --- Multiple Versions \n";
    } else {
      @outFiles[$j] = @outFilesTemp[$i];
      $j++;
    }
  } else {
    @outFiles[$j] = @outFilesTemp[$i];
    $j++;
  }
}

# Write these file names to a data file
$numFiles = @outFiles;
open(OUT, ">"."dateFile.dat") or die "Cannot open dateFile.dat";
print OUT "$numFiles\n";
print OUT "$ionName\n";
print OUT "$numBins\n";
print OUT "$errThresh\n";
print OUT "$velThresh\n";
print OUT "$effThresh\n";
print OUT "$dirBol\n";
print OUT "$selBol\n";
print OUT "$velCutOff\n";
print OUT "$sweFile\n";
print OUT "$dataOutput\n";
print OUT "$momentFile\n";
print OUT "$dirOutput\n";
for ($i = 0; $i < $numFiles; $i++){
    print OUT  "@outFiles[$i]\n";
}
close OUT;

print "Date File has been wrote to dateFile.dat.  Process Complete!\n";
