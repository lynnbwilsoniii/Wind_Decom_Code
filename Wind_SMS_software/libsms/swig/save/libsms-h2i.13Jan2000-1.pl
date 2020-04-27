#! /usr/bin/perl
# libsms-h2i.pl -- copy marked sections from libsms.h into libsms.i
#                  for SWIG/Perl interface to libsms
# Jim Raines, 21Dec99

########################
# initialization stuff #
########################

# define section markers
$begin = "libsms-h2i-begin";
$end = "libsms-h2i-end";

# define file names
$Infile = "../include/libsms.h";
$Outfile = "libsms.i";

# other misc. variables
$fCopy = 0;  # flag to indicate when line is within marked section
$thisprog = "libsms-h2i";  # name of this program
$fSBlock = 0; # flag indicating if a SWIG block was started

# open input and output files
open(IN, $Infile ) or die "Cannot open $Infile\n";

# $Outfile = ">".$Outfile;
if ( -e $Outfile ) {
  @CMD = {'mv', $Outfile, $Outfile.".old"};
  printf("%s: moving %s %s\n",$thisprog,$Outfile, $Outfile.".old");
  system tcsh @CMD;
}
open(OUT, ">".$Outfile) or die "Cannot open $Outfile\n";

# put standard stuff in OUT
print OUT <<EOF ;
/* $Outfile -- prototypes for SWIG/Perl interface to libsms */
/* This file generated automatically by $thisprog */

/* name of perl module (swig directive) */
%module libsms

EOF
######################################################
# get to work copying marked sections from IN to OUT #
######################################################
printf("%s: copying marked sections from %s to %s\n",$thisprog,$Infile,
       $Outfile);
while (<IN>) {
  # turn copying on when a begin marker is found
  if ( /$begin/ ) {
    if (/%\{/) { # switch flag to open SWIG block
      $fSBBegin = 1;
    }
    $fCopy = 1;
    next;
  }
  # turn copying off when an end marker is found
  if ( /$end/ ) { 
    if (/\}%/) {   # switch flag to close SWIG block
      $fSBEnd = 1;
    }
    $fCopy = 0;
    print OUT "\n";
    next;
  }

  if ($fSBBegin) { # open SWIG block
    print OUT "%{ /* SWIG block, copied directly to wrapper */\n";
    $fSBBegin = 0;
  }
  elsif ($fSBEnd) {   # close SWIG block
    print OUT "\%} /* end of SWIG block */\n";
    $fSBEnd = 0;
  }
     
  if ( $fCopy ){ # copy input to output if copy flag is true
    print OUT "$_";
  }
}
