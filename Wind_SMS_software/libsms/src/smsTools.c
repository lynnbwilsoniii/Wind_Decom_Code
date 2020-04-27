/** \file smsTools.c
    \brief A collection of short functions for accessing individual
   parts of cycle read into memory by libsms
   The return value of some functions depends on whether that EDB was in high or low bit rate mode
*/
/*
   Author:
   Jim Raines

   Notes:

   1. Naming scheme: Each function name is a series of letters
   sms - name of library (Swics Mass Stics)
   x - SWICS ; m - Mass ; t - STICS
   g - get
   item

   e.g. smsgxfsr -- SMS get SWICS FSR

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: smsTools.c,v 1.15 2005/11/29 19:22:11 jfeeman Exp $

   Modifcation history:
     Changed smsgtfsr through smstssd so that they are called with detector
     numbers 1-3 but access matrix elements 0-2.  J. Raines, 9Feb00.

     Added smsgver and slightly modified smssetdbg.  J. Raines, 29Feb00.

     Added smssqualover and smsgedbqual.  Made getBits user
     accessible.  J. Raines, 3Mar00.

     Made smssdbg print more messages.  Made a wrapper smsGetBits.
     J. Raines, 8Mar00.  */

#include "libsms.h"

/*****************************************************************************/
/*		       miscellaneous functions                               */
/*****************************************************************************/
/* user level get bits function -- just calls internal, but it was too much 
   trouble to change the name of the internal to smsGetBits, so this is done. 
*/
unsigned smsGetBits(unsigned byte, int lobit,int hibit){
  return(getBits(byte,lobit,hibit));
}

/* set debug level */
int smssdbg(int iDbgLvl) {
  char thisprog[] = "smssdbg";

  gDbgLvl = iDbgLvl;

  /* automatically turn on data flow trace if debug level is high enough */
  if (gDbgLvl > 2) {
    printf("%s - debug value set to %1.1d; get ready for lots of output!\n",
	   thisprog, gDbgLvl);

    gTrace = TRUE;
    printf("%s - trace turned on automatically by high debug value (%d)\n",
	   thisprog, gDbgLvl);
  }

  return(gDbgLvl);
}
/* synonym for smssdbg */
int smssetdbg(int iDbgLvl) {
  return(smssdbg(iDbgLvl));
}

/* set trace value */
int smsstrace(int iTrace) {
  gTrace = iTrace;
  return(gTrace);
}
/* synonym for smsstrace */
int smssettrace(int iTrace) {
  return (smsstrace( iTrace));
}

/* set incomplete cycle override flag to i.  Default behavior (i=0) of
   smsReadCycle is to drop cycles that do not have a full 62 EDBs (60
   for the cycle plus 2 from the next for readout delay).  Giving this
   function an argument of 1 causes these incomplete cycles to be
   kept.  Note: smssqualover must also be set.  Returns the new value
   of the flag (which may be ignored.)  Use with smsgnedb. */
int smssinccycover(int i){
  gfIncCycOver = i;
  return(gfIncCycOver);
}

/* get number of EDBs from current cycle -- This is only needed with
   smssinccycover; other wise there are always 60 EDBs per cycle. */
int smsgnedb(){
  return(cycle.nedb);
}

/* Return whether given EDB from the current cycle is in high or low
   bit rate mode.  All EDBs in a cycle should be the same - if there
   are mixed bit rate mode EDBs within a cycle, they are marked with
   the EQMIXEDBRC flag. Returns 1 for high bit rate mode, 0 for
   low. */ 
int smsgbitrate(int nedb) {
  return(cycle.h_dc[nedb].fHighBitrate);
}

/* set quality override flag.  Default behavior (i=0) of smsReadCycle
   is to drop cycles with at least 1 bad EDB.  Set this to 1 to keep
   cycles with EDBs that are marked bad.  See smsgedbqual.  Returns
   the new value of the flag (which may be ignored.)  Use with
   smsgedbqual.  */
int smssqualover(int i){
  gfQualOver = i;
  return(gfQualOver);
}

/* %localstyle pre */
/* smsgedbqual -- get EDB quality flag for EDB[nedb].  This is a bit field;
   use getBits to read any bit.  NOTE: Use this only if you have some good 
   reason to want to keep cycles with bad EDBs, i.e. if you need every bit 
   of data collected in a certain time period.  

   Here are the values:

   bit     meaning when set
   ---     ----------------
   0       X: (none of these work anyway!)
           T: bad OMR & ER
	   M: bad MR and spin-oriented ER
   1       X: bad PHA, npha
           T: bad PHA, npha, HMR, BR, SMR
	   M: bad PHA, npha, BR, sector-oriented ER
   2-6     not used
   7       Entire EDB is bad.

   Example: skip bad EDBs

   libsms::smssoverqual(1);  # tells smsReadCycle *not* to skip 
                             # cycles with bad EDBs

   for ($nedb = 0; $nedb < 60; $nedb++){
     if (getBits(libsms::smsgedbqual(nedb),7,7)){
       next;  # skip to next iteration because edb is bad
     }

     # do important stuff with each EDB
   }

*/
int smsgedbqual(int nedb){
  return(cycle.EDBQuality[nedb]);
}
/* %localstyle format */

/* smsgver -- get version of library, in form major.minor.build */
char *smsgver(){
  //printf("smsgver: %s\n", gVersion);
  return(&gVersion[0]);
}
  
/* smsgtime -- get time from Data Record Header for cycle, return msec of day. 
   Note: These time functions return the time for the first EDB in cycle. */
long smsgtime(){ 
  char thisprog[] = "smsgtime";
  return(cycle.drh.msec); /* add usec and return array? */
}

/* smsgtimes -- get time string; returns string with format YYYYDOY HH:MM:SS
 (%d msec %d usec)\n */
char *smsgtimes(){
  static char szTime[120]; /* time string */
  char *pszTime; /* pointer to time string */
  sprintf(szTime, "%4.4d%3.3d %2.2d:%2.2d:%2.2d UT (%d msec %d usec)",
	 cycle.drh.year, cycle.drh.day, cycle.drh.hour, cycle.drh.min, 
	  cycle.drh.sec, cycle.drh.msec, cycle.drh.usec);
  pszTime = &szTime[0];
  //printf("smsgtimes: %s\n",szTime);
  return pszTime;
}

/* smsgyear -- get year of current cycle */
int smsgyear(){
  return(cycle.drh.year);
}

/* smsgdoy -- get day of year (doy) of current cycle */
int smsgdoy(){
  return(cycle.drh.day);
}

/* Get hours minutes seconds (hms) -- returns  hh:mm:ss of current cycle */
char *smsghms(){
  static char hms[9];

  sprintf(hms,"%2.2d:%2.2d:%2.2d",cycle.drh.hour,cycle.drh.min,
	  cycle.drh.sec);

  return(hms);
}
/* Get seconds since 1970 -- returns cycle time in seconds since 1970 */
double smsgss1970(){
  char thisprog[] = "smsgss1970";
  
  static double ss1970, sec; /* seconds since 1970, seconds of day
				NOTE: made static because of weird bug from
				Perl that caused some subsequent calls to 
				return time 1 day short.  See smsBugLog.txt
				on 15Feb00 for details. */

  int pyear; /* prev. year for leap checking */
  int doy, year;
  int nleapdays = 6; /* number of leap days; 72-92 leap years initially */
  int fLeap; /* flag; T for leap years */

  doy = smsgdoy();
  year = smsgyear();
  sec = smsgtime()/1000.0;
  
  /* count # leap days since one in 1992 -- will not contribute to Y3K bug */
  pyear = 1993;
  while (pyear <= year) {
    /* Leap years are 
       a) divisible by 4 and not a century or
       b) a century but divisible by 400 */
    if ((year % 100) == 0) { /* year is a century */
      if ((year % 400) == 0) fLeap = TRUE; /* is divisible by 400 */
    }
    else { /* year is not a century */
      if ((year % 4) == 0) fLeap = TRUE; /* is divisible by 4 */
    }

    if (fLeap){
      nleapdays++;
      if (0) printf("%s -I- %d is a leap year -- nleapdays = %d\n",
	     thisprog, pyear, nleapdays);
    }

    fLeap = FALSE;
    pyear++;
  }

  /* add up all the seconds -- 
     Note: doy - 1 is to keep from counting this DOY twice here because sec 
     deals with seconds of this DOY. */
  ss1970 =  ((year - 1970)*365 + nleapdays + doy - 1 )*86400 + sec;

  if (0)  printf("%s -I- year=%d doy=%d sec=%f ss1970=%f\n",thisprog,
	 year, doy, sec, ss1970);
  return(ss1970);
}

/* smsIsLeapYear -- returns 1 if arg. is a leap year, 0 otherwise. */
int smsIsLeapYear(int year) {
  int fLeap = FALSE;  /* flag; T(1) => this year is a leap year */

  /* Leap years are 
     a) divisible by 4 and not a century or
     b) a century but divisible by 400 */
  if ((year % 100) == 0) { /* year is a century */
    if ((year % 400) == 0) fLeap = TRUE; /* is divisible by 400 */
  }
  else { /* year is not a century */
    if ((year % 4) == 0) fLeap = TRUE; /* is divisible by 4 */
  }

  return (fLeap);
}

/* smsIncrementDate -- Increment WIND-style date (yyyymmdd) by one
   day, rolling over months and years if necessary.  Handles leap
   years properly. */
long int smsIncrementDate(long int date){
  /* set up number of days per month -- 
     Note: 0 makes array align with month numbers */
  int DaysInMonth[] = {0,31,28,31,30,31,30,31,31,30,31,30,31};
  int year,mon,day;

  /* parse out year, month and day */
  year = date / 10000;
  day = date % 100;
  mon = (date % 10000 - day)/100;

  /* check for leap year, bump up Feb. days if it is */
  if (smsIsLeapYear(year)) DaysInMonth[2] = 29;

  /* -- increment day, rolling over month and year if necessary -- */
  if (day == DaysInMonth[mon] ) { /* at end of month */
    day = 1;

    /* increment month, rolling over year if necessary */
    if (mon == 12){ /* at end of year */
      mon = 1;
      year++;
    }
    else {           /* not at end of year */
      mon++;
    }

  }
  else {             /* not at end of month */
    day++;
  }

  /* re-assemble yyyymmdd into date */
  date = (year*10000) + (mon*100) + day;

  return (date);
}


/* smsdedbhdr -- dump edb header; prints all decoded edb header values */
int smsdedbhdr(int nedb){
  int j, *ps, /* loop index and pointer to beginning of structure */
    nchar, /* num. characters printed */
    charleft = 80; /* characters left out of 80 */
  char thisprog[] = "smsdedbhdr";

  ps = &cycle.h_dc[nedb].numsf; /* point to beginning of structure */

  /* print, keeping track of nchar and inserting returns to avoid wrapping */
  printf("%s -I- decoded header of edb[%2.2d] -- \n  ",thisprog, nedb);
  for (j=0; j< cycle.h_dc[nedb].nmembers; j++) {
    nchar = printf("%s=%d ",cycle.h_dc[nedb].members[j],*ps++);
    charleft = charleft - nchar;
    if (charleft < 15){
      printf("\n  ");
      charleft = 78;
    }
  }
  printf("\n");

  return(SMSSUCCESS);    
}  

/* Get science record counter (from HDB) of current cycle.  This counter is
 a unique number for each HDB (and associated EDBs of the science record 
(AKA cycle).  */
long smsgscirec(){
  return(cycle.hdb.SciRecCount);
}

/* Get revolution spin counter from EDB[nedb] -- This the is low byte of this 
   counter.  It is found in EDB byte 4. */
int smsgrevcount(int nedb){
  return(cycle.h_dc[nedb].revcount);
}
/*****************************************************************************/
/*			 SWICS (x) functions                                 */
/*****************************************************************************/

/************************/
/* SWICS (x) eng. rates */
/************************/
/* get SWICS front SEDA (secondary electron detector assembly) rate for EDB[nedb] */
long smsgxfsr(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0) //low bit rate mode
    return(smsDecompress(cycle.edb[nedb].xfsr,cycle.hdb.xCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1) //high bit rate mode
    return(smsDecompress(cycle.edb_r_h[nedb].xfsr, cycle.hdb.xCompGrp1));
}

/* get SWICS deflection voltage step (DVS; AKA DPPS step) for EDB[nedb] */
long smsgxdvs(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(getBits(cycle.edb[nedb].xhk,0,5));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(getBits(cycle.edb_r_h[nedb].xhk, 0, 5));
}

/* Get SWICS deflection voltage step table (dvstab), element i (0 <= i <= 59)
   Method:  Returns 59 - i which is the fixed stepping table for SWICS,
   where i is the EDB number (AKA measurement spin number). */
int smsgxdvstab(int i){
  return(59 - i);
}

/*******************/
/* SWICS PHA words */
/*******************/
/* Get SWICS number of PHA words for EDB[nedb] */
long smsgxnpha(int nedb){
  return(cycle.h_dc[nedb].xnpha);
}

/* dump (print) raw parts of PHA[i] (first is i=0) from EDB[nedb] */ 
int smsdxpha(int nedb, int i){ 
  char thisprog[] = "smsdxpha";
  XPHA xpha_dc;
  int j;

  /* raw members of structure */
  char *members[] = {"sector","energy","range","ssdId","tof","ssdName"};
  int nmembers = 6;
  int *ps; /* pointer to sxpha structure */

  xpha_dc = decodeXPHA(nedb, i); /* decode PHA word into structure */

  /* print decoded members */
  ps = &xpha_dc.sector;

  printf("%s: ",thisprog);
  for (j=0; j < nmembers - 1; j++){
    printf("%s=%d ",members[j],*ps++);
  }
  /* doesn't work with pointer -- probably because ps is an int * */
  printf("%s=%s\n",members[j],xpha_dc.ssdName);

  return(SMSSUCCESS);
}

/* Get SWICS energy of PHA[i] from EDB[nedb] */
double smsgxenergy(int nedb, int i){
  XPHA xpha_dc;
  xpha_dc = decodeXPHA(nedb,i); /* decode this PHA word */

  return (xpha_dc.energy);
}

/* Get SWICS energy / charge (E/Q) of PHA[i] from EDB[nedb] */
double smsgxeoq(int nedb, int i){
  XPHA xpha_dc;
  xpha_dc = decodeXPHA(nedb,i); /* decode this PHA word */

  return (xpha_dc.eoq);
}

/* Get SWICS TOF (in physical units of ns) of PHA[i] from EDB[nedb] */
double smsgxtof(int nedb,int i){
  XPHA xpha_dc;
  double dtmp;
  xpha_dc = decodeXPHA(nedb,i); /* decode this PHA word */

  return (xpha_dc.tofns);
}  

/* Get SWICS energy at SSD in keV of PHA[i] from EDB[nedb] */
double smsgxessd(int nedb, int i){
  XPHA xpha_dc;
  xpha_dc = decodeXPHA(nedb,i); /* decode this PHA word */

  return (xpha_dc.energykev);
}

/* Get SWICS mass in amu PHA[i] from EDB[nedb] */
double smsgxmass(int nedb, int i){
  XPHA xpha_dc;
  xpha_dc = decodeXPHA(nedb,i); /* decode this PHA word */

  return (xpha_dc.mass);
}

/* Get SWICS mass/charge in amu PHA[i] from EDB[nedb] */
double smsgxmoq(int nedb, int i){
  XPHA xpha_dc;
  xpha_dc = decodeXPHA(nedb,i); /* decode this PHA word */

  return (xpha_dc.moq);
}

/* Get SWICS energy/charge table (eqtab), element i.  NOTE: This just uses
   smsgxdvstab and smscxdvs2eq. */
double smsgxeqtab(int i){
  return(smscxdvs2eq(smsgxdvstab(i)));
}

/* %localstyle pre */
/* Convert SWICS  deflection voltage step (dvs) to energy/charge.

   Note 1: This is the same energy/charge calculation as smsmgxeoq uses 
   (but that one inserts the particular DVS for the PHA word in question).

   Note 2: This is currently a hard-coded look up table and and does not 
   read values from the telemetry.  Values used here were obtained from 
   Alysha Reynard, Feb2000.  (They were calc. from TM originally.)*/
double smscxdvs2eq(int dvs){
  /* E/q from Alysha Reynard, Feb2000, will be replaced in future vers. */
  double eqtab[] = {2.604000, 2.448000, 2.309000, 2.170000, 
    2.031000, 1.892000, 1.753000, 1.632000, 1.528000, 1.423000, 1.319000,
    1.232000, 1.145000, 1.059000, 0.989000, 0.920000, 0.850000, 0.781000,
    0.728000, 0.676000, 0.642000, 0.590000, 0.537000, 0.503000, 0.468000,
    0.433000, 0.398000, 0.393000, 0.364000, 0.341000, 0.318000, 0.297000,
    0.278000, 0.258000, 0.242000, 0.226000, 0.209000, 0.196000, 0.183000,
    0.170000, 0.160000, 0.149000, 0.139000, 0.129000, 0.121000, 0.113000,
    0.105000, 0.099000, 0.092000, 0.085000, 0.081000, 0.076000, 0.069000,
    0.066000, 0.061000, 0.056000, 0.053000, 0.050000, 0.046000, 0.043000};

 return(eqtab[dvs]);
}
/* %localstyle format */
  
/*****************************************************************************/
/*			 STICS (t) functions                                 */
/*****************************************************************************/
/**************************************/
/* STICS Decommutated Values from HDB */
/**************************************/
/* get STICS measured positive voltage table (volts), element i*/
double smsgtmpvtab(int i){ 
  return(cycle.decomm_hdb.tMeasPosStepTab[i]);
}

/* get STICS measured positive voltage table (raw), element i */
int smsgtmpvtab_raw(int i){
  return(cycle.decomm_hdb.tMeasPosStepTab_Raw[i]);
}

/* get STICS measured negative voltage table, element i */
double smsgtmnvtab(int i){
  return(cycle.decomm_hdb.tMeasNegStepTab[i]);
}

/* get STICS measured negative voltage table (raw), element i */
int smsgtmnvtab_raw(int i){
  return(cycle.decomm_hdb.tMeasNegStepTab_Raw[i]);
}

/************************/
/* STICS non-eng. rates */
/************************/
long smsgthmr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].thmr[i],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].thmr[i], cycle.hdb.tCompGrp1));
}

long smsgtsmr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tsmr[i],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tsmr[i], cycle.hdb.tCompGrp1));  
}

long smsgtbr0(int nedb, int i){  /* get br0 out of edb=nedb, sector=i */
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tbr0[i],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tbr0[i], cycle.hdb.tCompGrp1));
}

long smsgtbr1(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tbr1[i],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tbr1[i], cycle.hdb.tCompGrp1));
}

long smsgtbr2(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tbr2[i],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tbr2[i], cycle.hdb.tCompGrp1));
}

long smsgtomr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tomr[i],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tomr[i], cycle.hdb.tCompGrp1));   
}

/********************/
/* STICS eng. rates */
/********************/
long smsgtfsr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tfsr[i-1],cycle.hdb.tCompGrp2));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tfsr[i-1], cycle.hdb.tCompGrp2));
}

/* right compression group? */
long smsgtrsr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].trsr[i-1],cycle.hdb.tCompGrp2));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].trsr[i-1], cycle.hdb.tCompGrp2));
}

long smsgtdcr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tdcr[i-1],cycle.hdb.tCompGrp2));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tdcr[i-1], cycle.hdb.tCompGrp2));
}

long smsgttcr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].ttcr[i-1],cycle.hdb.tCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].ttcr[i-1], cycle.hdb.tCompGrp1));
}

long smsgtssd(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].tssd[i-1],cycle.hdb.tCompGrp2));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].tssd[i-1], cycle.hdb.tCompGrp2));
}

/* get STICS deflection voltage step (DVS; AKA DPPS step) for EDB[nedb] */
int smsgtdvs(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(getBits(cycle.edb[nedb].thk,0,4));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(getBits(cycle.edb_r_h[nedb].thk, 0, 4));
}


/* Get STICS deflection voltage step table (dvstab), element i (0 <= i <= 59)
   Method: This gets a dvs sequence from the current cycle THE FIRST TIME
   IT IS CALLED.  Subsequent calls simply read that table out of static memory.
   It resets itself when a new file is opened.  

   Note:  This DOES NOT do
   checks to be sure that this table does not change during the reading of the
   file. If what you want is a dvs table from the current cycle, do something
   like this:  
   for ($i = 0; $i < 60; $i++){$dvstab[$i] = smsgtdvs(i)} */
int smsgtdvstab(int i){
  int j; /* loop counter */
  static int fFirstRun = TRUE; /* flag for things to do once */
  static int dvstab[60];   /* dvs table */
  long CurDate; /* yyyydoy of current cycle */
  static long CollectDate; /* yyyydoy when table was collected */

  /* make an identifier for the current date */
  CurDate = smsgyear()*1000 + smsgdoy();

  /* if file has changed read another table */
  if (!fFirstRun)
    if (CurDate != CollectDate) 
      fFirstRun = FALSE;

  if (fFirstRun){
    for (j = 0; j < 60; j++)
      dvstab[j] = smsgtdvs(j);
    fFirstRun = FALSE;
    CollectDate = smsgyear()*1000 + smsgdoy();
  }

  return(dvstab[i]);
}


/* get STICS mode: 0==>normal, 1==>diagnostic */
int smsgtmode(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(getBits(cycle.edb[nedb].thk,7,7));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(getBits(cycle.edb_r_h[nedb].thk, 7, 7));
}

/* get STICS mode string: normal or diagnostic */
char *smsgtmodes(int nedb){
  static char szMode[20];
  char *pszMode;

  if(cycle.h_dc[nedb].fHighBitrate == 0) {
    if (getBits(cycle.edb[nedb].thk,7,7))
      sprintf(szMode, "%s", "Diagnostic mode");
    else
      sprintf(szMode, "Normal mode");
  }
  else if(cycle.h_dc[nedb].fHighBitrate == 1) {
    if(getBits(cycle.edb_r_h[nedb].thk, 7, 7))
      sprintf(szMode, "%s", "Diagnostic mode");
    else
      sprintf(szMode, "Normal mode");
  }

  pszMode = &szMode[0];
  printf("smsgtmodes: %s\n",pszMode);
  return pszMode;
}



/*******************/
/* STICS PHA words */
/*******************/
/* get number of pha words from EDB[nedb]*/
int smsgtnpha(int nedb){
  return(cycle.h_dc[nedb].tnpha);
}

/* dump (print) raw parts of PHA[i] (first is i=0) from EDB[nedb] */ 
int smsdtpha(int nedb, int i){ 
  char thisprog[] = "smsdtpha";
  TPHA tpha_dc;
  int j;

  /* raw members of structure */
  char *members[] = {"stopId","startIdRng","energy","sector","ssdId","tof",
		     "ssdName"};
  int nmembers = 7;
  int *ps; /* pointer to stpha structure */

  tpha_dc = decodeTPHA(nedb, i); /* decode PHA word into structure */

  /* print decoded members */
  ps = &tpha_dc.stopId;

  printf("%s: ",thisprog);
  for (j=0; j < nmembers - 1; j++){
    printf("%s=%d ",members[j],*ps++);
  }
  /* doesn't work with pointer -- probably because ps is an int * */
  printf("%s=%s\n",members[j],tpha_dc.ssdName);

  return(SMSSUCCESS);
}

/* Get STICS stop Id of PHA[i] from EDB[nedb] */
long smsgtstopid(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.stopId);
}

/* Get STICS energy (digital but decompressed from PHA) of PHA[i]
   from EDB[nedb] */
long smsgted(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.energy);
}

/* Get STICS sector of PHA[i] from EDB[nedb] */
long smsgtsector(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.sector);
}

/* Get STICS SSD ID of PHA[i] from EDB[nedb] */
long smsgtssdid(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.ssdId);
}

/* Get STICS time of flight (digital; straight from PHA) of PHA[i] 
   from EDB[nedb] */
long smsgttofd(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.tof);
}

/* Get STICS start anode of PHA[i] from EDB[nedb] */
long smsgtstart(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.start);
}

/* Get STICS range of PHA[i] from EDB[nedb] */
long smsgtrange(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.range);
}

/* Get energy / charge (E/Q) of PHA[i] from EDB[nedb] */
double smsgteoq(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.eoq);
}

/* Get TOF (in physical units of ns) of PHA[i] from EDB[nedb] */
double smsgttof(int nedb,int i){
  TPHA tpha_dc;
  double dtmp;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.tofns);
}  

/* Get energy at SSD in keV of PHA[i] from EDB[nedb] */
double smsgtessd(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.energykev);
}

/* Get mass in amu PHA[i] from EDB[nedb] */
double smsgtmass(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.mass);
}

/* Get mass/charge in amu PHA[i] from EDB[nedb] */
double smsgtmoq(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i); /* decode this PHA word */

  return (tpha_dc.moq);
}

/* Get STICS NM (mass classification bin) number for EDB[nedb] PHA[i] --
   The mass part of the M-M/Q space is divided into 58 bins which are
   logarithmically spaced.  The bins are calculated by 
   mass{lower bound of Nm in amu} = 0.5*km^(Nm - 1) with 
   km = (95.0/0.5)^1/58 .  Nm = 0 for mass zero events.  */
int smsgtnm(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i);

  return(tpha_dc.nm);
}

/* Get STICS NQ (mass/charge classification bin) number for EDB[nedb] PHA[i] --
   The mass/charge part of the M-M/Q space is divided into 126 bins which are
   logarithmically spaced.  The bins are calculated by 
   M/Q{lower bound of Nq in amu/e} = 0.5*kq^(Nq - 1) with 
   km = (42.0/0.9)^1/126 .  Nm = 0 for mass zero events.  */
int smsgtnq(int nedb, int i){
  TPHA tpha_dc;
  tpha_dc = decodeTPHA(nedb,i);

  return(tpha_dc.nq);
}

/* Get STICS energy/charge table (eqtab), element i.  NOTE: This just uses
   smsgtdvstab and smsctdvs2eq. */
double smsgteqtab(int i){
  return(smsctdvs2eq(smsgtdvstab(i)));
}

/* Convert STICS  deflection voltage step (dvs) to energy/charge.
   Note: This is the same energy/charge calculation as smsmgteoq uses 
   (but that one inserts the particular DVS for the PHA word in question). */
double smsctdvs2eq(int dvs){
  double eq;

  /* predicted energy / charge (E/Q) -- Cham's thesis, p50 */
  eq = 6.190722*(pow(1.1225857,dvs));

  return(eq);
}
    
/*****************************************************************************/
/*			  MASS (m) functions                                 */
/*****************************************************************************/

/******************************/
/* MASS non-engineering rates */
/******************************/

/* get mass matrix rate i -- It is unclear at this time whether these are 
   double coincidence or single.  The rates are as follows:

   i        Rate
   -        ----
   0        H
   1        He3
   2        He4
   3        C
   4        N
   5        O
   6        Ne
   7        Mg
   8        Si
   9        Fe
*/
long smsgmmr(int nedb,int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return (smsDecompress(cycle.edb[nedb].mmr[i], cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mmr[i], cycle.hdb.mCompGrp1));
}

/* get MASS basic rate br0_s */
long smsgmbr0_s(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return (smsDecompress(cycle.edb[nedb].mbr0_s, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mbr0_s, cycle.hdb.mCompGrp1));
}

/* get MASS basic rate br0_ns */
long smsgmbr0_ns(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return (smsDecompress(cycle.edb[nedb].mbr0_ns, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mbr0_ns, cycle.hdb.mCompGrp1));
}

/* get MASS basic rate br1_s */
long smsgmbr1_s(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return (smsDecompress(cycle.edb[nedb].mbr1_s, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mbr1_s, cycle.hdb.mCompGrp1));
}

/* get MASS basic rate br1_ns */
long smsgmbr1_ns(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return (smsDecompress(cycle.edb[nedb].mbr1_ns, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mbr1_ns, cycle.hdb.mCompGrp1));
}


/*******************/
/* MASS eng. rates */
/*******************/

/* unsectored engineering rates */
/********************************/
long smsgmfsr(int nedb, int i){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mfsr[i],cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
     return(smsDecompress(cycle.edb_r_h[nedb].mfsr[i], cycle.hdb.mCompGrp1));
}

long smsgmfsrb(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mfsrb,cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mfsrb, cycle.hdb.mCompGrp1));
}

long smsgmmdcr(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mmdcr, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mmdcr, cycle.hdb.mCompGrp1));
}

/* sectored engineering rates */
/******************************/
long smsgmufsr_s(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mufsr_s, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mufsr_s, cycle.hdb.mCompGrp1));
}

long smsgmufsr_ns(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mufsr_ns, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mufsr_ns, cycle.hdb.mCompGrp1));
}

long smsgmrsr_s(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mrsr_s, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mrsr_s, cycle.hdb.mCompGrp1));
}

long smsgmrsr_ns(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mrsr_ns, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mrsr_ns, cycle.hdb.mCompGrp1));
}

long smsgmdcr_s(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mdcr_s, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mdcr_s, cycle.hdb.mCompGrp1));
}

long smsgmdcr_ns(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mdcr_ns, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mdcr_ns, cycle.hdb.mCompGrp1));
}

long smsgmmfsr(int nedb) {
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(smsDecompress(cycle.edb[nedb].mmfsr, cycle.hdb.mCompGrp1));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(smsDecompress(cycle.edb_r_h[nedb].mmfsr, cycle.hdb.mCompGrp1));
}


/* get mass deflection voltage step (DVS; aka DPPS step) from EDB[nedb] */
long smsgmdvs(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return (getBits(cycle.edb[nedb].mhk,0,5));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(getBits(cycle.edb_r_h[nedb].mhk, 0, 5));
}

/* Get MASS deflection voltage step table (dvstab), element i (0 <= i <= 59)
   Method: This gets a dvs sequence from the current cycle THE FIRST TIME
   IT IS CALLED.  Subsequent calls simply read that table out of static memory.
   It resets itself when a new file is opened.  

   Note:  This DOES NOT do
   checks to be sure that this table does not change during the reading of the
   file. If what you want is a dvs table from the current cycle, do something
   like this:  
   for ($i = 0; $i < 60; $i++){$dvstab[$i] = smsgmdvs(i)} */
int smsgmdvstab(int i){
  int j; /* loop counter */
  static int fFirstRun = TRUE; /* flag for things to do once */
  static int dvstab[60];   /* dvs table */
  long CurDate; /* yyyydoy of current cycle */
  static long CollectDate; /* yyyydoy when table was collected */

  /* make an identifier for the current date */
  CurDate = smsgyear()*1000 + smsgdoy();

  /* if file has changed read another table */
  if (!fFirstRun)
    if (CurDate != CollectDate) 
      fFirstRun = FALSE;

  if (fFirstRun){
    for (j = 0; j < 60; j++)
      dvstab[j] = smsgmdvs(j);
    fFirstRun = FALSE;
    CollectDate = smsgyear()*1000 + smsgdoy();
  }

  return(dvstab[i]);
}


/* get MASS calibration mode: 0==>no calibration, 1==>calibration */
int smsgmcalmode(int nedb){
  if(cycle.h_dc[nedb].fHighBitrate == 0)
    return(getBits(cycle.edb[nedb].mhk,7,7));
  else if(cycle.h_dc[nedb].fHighBitrate == 1)
    return(getBits(cycle.edb_r_h[nedb].mhk, 7, 7));
}


/******************/
/* MASS PHA words */
/******************/

/* Get MASS number of pha words from EDB[nedb]*/
int smsgmnpha(int nedb){
  return(cycle.h_dc[nedb].mnpha);
}

/* Get MASS energy/charge table (eqtab), element i.  NOTE: This just uses
   smsgmdvstab and smscmdvs2eq.   See smscmdvs2eq for acceptable
   values of source. */
double smsgmeqtab(int i, int source){
  /* FIX: make this static like dvstab */
  return(smscmdvs2eq(smsgmdvstab(i),source));
}

/* Convert MASS deflection voltage step (dvs) to energy/charge.
   source specifies the source of this information.  source = 1: According to
   the following formula, E/q = 0.5156 * 1.05135^dvs. source = 2:
   Values from HDB (commutated over 64 HDBs).  These were measured in
   the in the previous science record for the same spin.  NOTE: The
   measured values are not currently converted to voltages properly.
   Only the raw numbers are returned. */
double smscmdvs2eq(int dvs, int source){
  double eoq;

  if (source == 1){ /* use hard coded conversion */
    /* Formula from MASS_RATE_TABLE.FOR, UMD Source code; This
       produces eoq values from 9.89 (dvs=59) to 0.52 (dvs=0).  This
       corresponds very closely with the WIND/SMS paper, which species 
       9.5 to 0.5 keV/e.  */
    eoq = 0.5156*(pow(1.05135, dvs));
  }
  else {
    /* read out of set of HDBs */
    eoq = cycle.decomm_hdb.mMeasStepTab[dvs];
  }
  
  return(eoq);
}

/* Get MASS sector of PHA[i] from EDB[nedb] */
long smsgmsector(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i); /* decode this PHA word */

  return (mpha_dc.sector);
}

/* Get MASS range of PHA[i] from EDB[nedb] */
long smsgmrange(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i);

  return (mpha_dc.range);
}

/* Get MASS anode of PHA[i] from EDB[nedb] */
long smsgmanode(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i);

  return (mpha_dc.anode);
}

/* Get MASS tof of PHA[i] from EDB[nedb] */
long smsgmtof(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i);

  return (mpha_dc.tof);
}

/* Get MASS tof in ns of PHA[i] from EDB[nedb] */
double smsgmtofns(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i);

  return (mpha_dc.tofns);
}

/* Get MASS mass of PHA[i] from EDB[nedb] */
double smsgmmass(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i);

  return (mpha_dc.mass);
}

/* Get MASS mass per charge of PHA[i] from EDB[nedb] */
double smsgmmoq(int nedb, int i) {
  MPHA mpha_dc;
  mpha_dc = decodeMPHA(nedb,i);

  return (mpha_dc.moq);
}
    
/*****************************************************************************/
/*			   Getting Raw Data                                  */
/*****************************************************************************/
/* Note: these are only to be used when a special need precludes using
   the sms functions which give access to the formatted data. */

/* smsgchk -- get fixed house keeping data, minor frame mf, byte i.
   Call for mf ranging from 0 - 249, i = 17 or 18.  Note: This is
   currently only for the last record read from the file. */
unsigned char smsgchk(int mf, int i){
  if (i == 17)
    i = 0;
  else if (i == 18)
    i = 1;
  else
    i = 0;

  return(cycle.CHK[mf][i]);
}


/* smsghdb -- get housekeeping data block (HDB) byte i.  Call for i ranging
   from 0 - 279 to get all 280 bytes of the HDB. */
unsigned char smsghdb(int i){
  return(cycle.hdb.r[i]);
}

/* smsgedb() -- get experimental data block (EDB) */
/* not yet implemented */

/*****************************************************************************
			  Decoded CHK Items
 *****************************************************************************/

/* smsgdtempsc -- get DPU temperature (spacecraft thermistor) in
   degrees Celcius.  This routine reads CHK0 (via smsgchk(1,17))
   and converts it to temperature through a polynomial fit:

      rTemp = 6.558508E2 - 1.802378E1 * dCHK + 2.133615E-1 * pow(dCHK,2) - 
      1.295112E-3 * pow(dCHK,3) + 3.927925E-6 * pow(dCHK,4) - 
      4.754717E-9 * pow(dCHK,5) ;

       where rTemp is real temperature (in C)
             dCHK is the CHK value used, cast as double precision
	     pow(dCHK,x) is dCHK raised to the power x

   This fit was done in pre-flight calibration.
*/
double smsgdtempsc(){
  return(fTemperature(smsgchk(1,17),0));
}

/* smsgdtemp -- get DPU temperature in degrees Celcius.  This routine
   reads CHK9 (via smsgchk(31,17)) and converts it to a temperature
   through a polynomial fit:

      rTemp = 1.437601E2 - 3.224279 * dCHK + 4.848285E-2 * pow(dCHK, 2) - 
      4.101237E-4 * pow(dCHK, 3) + 1.682762E-6 * pow(dCHK, 4) - 
      2.660079E-9 * pow(dCHK, 5)


       where rTemp is real temperature (in C)
             dCHK is the CHK value used, cast as double precision
	     pow(dCHK,x) is dCHK raised to the power x

   This fit was done in pre-flight calibration.
*/
double smsgdtemp(){
  return(fTemperature(smsgchk(31,17),1));
}

/* smsgxaetempsc -- get SWICS AE temperature (spacecraft thermistor)
   in degrees Celcius.  This routine reads CHK1 (via
   smsgchk(3,18)) and converts it to a temperature through a
   polynomial fit.  See smsgdtempsc().

*/
double smsgxaetempsc(){
  return(fTemperature(smsgchk(3,18),0));
}

/* smsgxssdtemp -- get SWICS SSD temperature (PAPS referenced) in degrees 
   Celcius.  This routine reads CHK29 (via smsgchk(97,18)) and converts it
   to a temperature through a polynomial fit:

      rTemp = 7.886257E1 - 1.848392 * dCHK + 2.143749E-2 * pow(dCHK, 2) -
      1.483171E-4 * pow(dCHK, 3) + 5.149126E-7 * pow(dCHK, 4) - 
      7.140936E-10 * pow(dCHK, 5) ;

       where rTemp is real temperature (in C)
             dCHK is the CHK value used, cast as double precision
	     pow(dCHK,x) is dCHK raised to the power x

   This fit was done in pre-flight calibration.
*/
double smsgxssdtemp(){
  return(fTemperature(smsgchk(97,18),2));
}

/* smsgmaetempsc -- get MASS AE temperature (spacecraft thermistor) in
   degrees Celcius.  This routine reads CHK2 (via smsgchk(7,18))
   and converts it to a temperature through a polynomial equation.  See
   smsgdtempsc. */
double smsgmaetempsc(){
  return(fTemperature(smsgchk(7,18),0));
}

/* smsgmaetemp -- get MASS AE temperature in
   degrees Celcius.  This routine reads CHK39 (via smsgchk(131,17))
   and converts it to a temperature through a polynomial equation.  See
   smsgxssdtemp. */
double smsgmaetemp(){
  return(fTemperature(smsgchk(131,17),2));
}

/* smsgtssdtempsc -- get STICS SSD temperature (spacecraft thermistor)
   in degrees Celcius.  This routine reads CHK3 (via
   smsgchk(11,17)) and converts it to a temperature through a
   polynomial fit.  See smsgxaetempsc. */
double smsgtssdtempsc(){
  return(fTemperature(smsgchk(11,17),0));
}

/* smsgtaetempsc -- get STICS AE temperature (spacecraft thermistor)
   in degrees Celcius.  This routine reads CHK4 (via
   smsgchk(13,18)) and converts it to a temperature through a
   polynomial fit.  See smsgxaetempsc. */
double smsgtaetempsc(){
  return(fTemperature(smsgchk(13,18),0));
}

/* smsgtaetemp -- get STICS AE temperature
   in degrees Celcius.  This routine reads CHK60 (via
   smsgchk(201,17)) and converts it to a temperature through a
   polynomial fit.  See smsgxssdtemp. */
double smsgtaetemp(){
  return(fTemperature(smsgchk(201,17),2));
}

/* smsgtssdtemp -- get STICS SSD temperature
   in degrees Celcius.  This routine reads CHK61 (via
   smsgchk(203,18)) and converts it to a temperature through a
   polynomial fit.  See smsgxssdtemp. */
double smsgtssdtemp(){
  return(fTemperature(smsgchk(203,18),2));
}

