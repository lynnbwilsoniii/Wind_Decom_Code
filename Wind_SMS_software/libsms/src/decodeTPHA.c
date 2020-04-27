/** \file decodeTPHA.c
    \brief Decode STICS PHA word into detailed structure
*/
/**
   Description: Breaks double TPHA word into component parts
   and returns tpha structure.  (where t/T stands for STICS)
   See DPU manual, pages 294-5.
*/
/*
   Author: Jim Raines, 2Dec99

   Method: 
   The 4 byte PHA word is first assembled then taken apart into various 
   components as described on p293-4 in the DPU manual.  Conversion are from
   the doctoral thesis of Kancham Chotoo, Dept. of Physics,
   Univ. of Maryland, 1998.

   Requires: 

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: decodeTPHA.c,v 1.9 2012/05/16 17:31:19 jonthom Exp $

   Modification History: (keep this last section)

   Changed M/q constant and added 1.5 and 0.5 in energy conversion according
   to what Peter Bedini found in UMD sw.  J. Raines, 28Feb00

   Reworked debugging output to format nicely.  J. Raines, 30Aug2000
*/

#include "libsms.h"

TPHA decodeTPHA(int nedb, int npha){ /* nedb ==> edb # ; npha ==> PHA word # */
  /* decode STICS PHA word */
  int i,j;  /* short range loop counters */
  char thisprog[] = "decodeTPHA"; /* name of this program */

  BYTE PhaWord[4];
  double dtpha; /* double STICS PHA word */

  TPHA stpha; /* tpha structure */
  char *szSSD[] = { "none", "B", "M", "T"}; /* names of STICS detectors */

  /* constants and variables for PHA calc., defined in calc. code below */
  double u; /* voltage at step; eoq calc. */
  double Ed; /* digital energy; energy calc. */ 
  double A1, A2, A3, A4, A5, A6, X, Y, lnM; /* A's are polynomial coef; mass */
  double C1; /* constant; moq calc. */
  
  /* variables for mass and m/q classification */
  double km; /* constant which defines nm bin size */
  static int fFirstRun = TRUE; /* flag indicating first run of this routine */
  int nm; /* nm loop counter */
  static double nmbins[59]; /* array of NM lower mass bounds */
  double kq; /* constant which defines nq bin size */
  int nq; /* nq loop counter */
  static double nqbins[128]; /* array of NQ lower mass/charge bounds */
  
  /*****************************/
  /* collect bytes of PHA word */
  /*****************************/
  /*++ There needs to be two of these, one for 'cycle.edb' and one for 'cycle.edb_r_h' ++*/

  /* jahf - 30Nov2005 - It *may* be possible that reversing these bytes is wrong... */

  /* jahf - 01Dec2005 - Nope. Reversing the bytes here appears to be
     the correct thing to do. I used tpha.pl and plot_tphatofhist.pro
     (in libsms/tools) to create a STICS tof histogram and the plot
     looked decent when the bytes are reversed here and looked wrong
     when I didn't reverse the bytes here. */
  for (j = 0; j < 4; j++) {
    if(isHigh == 1) {
      PhaWord[j] = cycle.edb_r_h[nedb].tpha[4*npha + 3 - j]; /*++ ??? ++*/
    }
    else if(isHigh == 0) {
      PhaWord[j] = cycle.edb[nedb].tpha[4*npha + 3 - j]; /* reverse bytes */
    }
  }

  dtpha = dBuildWord(PhaWord,0,3);
 
  /******************************************************/
  /* pull apart word in order of diagram in DPU manual */
  /******************************************************/
  stpha.stopId = getBits(dtpha,30,31);
  stpha.startIdRng = getBits(dtpha,25,29);
  stpha.energy = getBits(dtpha,16,24); /* compressed; DPU man. uses bad nota.*/
  stpha.sector = getBits(dtpha,12,15);
  stpha.ssdId = getBits(dtpha,10,11);
  stpha.tof = getBits(dtpha,0,9);

  strcpy(stpha.ssdName, szSSD[stpha.ssdId]);

  /* do a little decoding -- thesis p50 */
  stpha.start = (int) stpha.startIdRng/3;
  stpha.range = stpha.startIdRng - 3*stpha.start;

  /***********************************************/
  /* calculate some quantities in physical units */
  /***********************************************/

  /* empirical energy / charge  (E/Q) */
  u = 2*smsgtmpvtab(nedb);   /* u is voltage at this step 
				  assuming -dppshv = -( dppshv ) */

  stpha.eeoq = 23.534*u; /* derived by Thomas Zurbuchen from inner and outer
			   radii of entrance system, using centrip. force */

  /* predicted energy / charge (E/Q) -- Cham's thesis, p50 */
  stpha.eoq = smsctdvs2eq(smsgtdvs(nedb));

  /* time of flight -- Cham's thesis, p50 */
  stpha.tofns = (stpha.tof - 44)/2.3725306895;

  /* energy -- Cham's thesis, p50 (mostly) */
  if (stpha.energy < 256) /* decompress energy */
    Ed = stpha.energy;
  else if ((stpha.energy >= 256) && (stpha.energy < 384))
    Ed = 2*stpha.energy - 256 + 0.5; /* 0.5 from Peter; found in UMD sw */
  else if ((stpha.energy >= 384) && (stpha.energy < 1024))
    Ed = 4*stpha.energy - 1024 + 1.5; /* 1.5 from Peter; found in UMD sw */

  stpha.energykev = (Ed + 6)/0.37654782; 

  /* mass -- Cham's thesis, p51 */
  A1 = 2.69575; /* polynomial coeficients */
  A2 = -0.843766;
  A3 = -2.38009;
  A4 = 0.385641;
  A5 = 0.0513127;
  A6 = 0.0690096;

  X = log(stpha.energykev); /* defined to be consistent with thesis notation */
  Y = log(stpha.tofns);

  lnM = A1 + A2*X + A3*Y + A4*X*Y + A5*pow(X,2) + A6*pow(Y,3);

  if (stpha.energykev < 21.0) /* give mass=0 below energy cutoff (of SSD?) */
    stpha.mass = 0;
  else
    stpha.mass = exp(lnM); /* actual calc. of mass */

  /* mass/charge -- Cham's thesis, p50-1 */
  C1 = 1.5;  /* temporary until we figure out what to switch on */
  //stpha.moq = 1.91189e-05*(stpha.eoq - C1)*pow(stpha.tofns,2);
  
  /* revised constant which Peter Bedini found in the UMD sw */
  stpha.moq = 1.9159E-05*(stpha.eoq - C1)*pow(stpha.tofns,2);

  if (stpha.moq >= 11){ /* calc. again if true */
    C1 = 2.5;
    stpha.moq = 1.91189e-05*(stpha.eoq - C1)*pow(stpha.tofns,2);
  }

  /**************************************************/
  /* Classification functions -- Cham's thesis, p51 */
  /**************************************************/

  /* NM -- create mass bins and  */
  km = pow((95.0/0.5),(float)1/58);

  if (fFirstRun) { 
    if (DEBUG > 0) printf("%s -D- Nm bins (lower bounds in amu) --\n",thisprog);
    for (nm = 1; nm < 59; nm++){
      nmbins[nm] = 0.5*pow(km,nm - 1);
      if (DEBUG > 0) {
	printf(" %5.2e",nmbins[nm]);  /* print out bins */
	if (!(nm % 8)) printf("\n");  /* break line every 8 */
      }
    }
    if (DEBUG > 0) printf("\n");
  }

  /* classify this mass */
  if (stpha.mass == 0){
    stpha.nm = 0;
  }
  else {
    nm = 1;
    while (nm < 59){ /* to keep from overflowing array bounds */
      if ((stpha.mass > nmbins[nm]) && (stpha.mass < nmbins[nm + 1])){
	stpha.nm = nm;
	break;
      }

      nm++;
    }
  }

  /* MQ -- create mass/charge bin */
  kq = pow((42.0/0.9),(float)1/126);

  if (fFirstRun) {
    if (DEBUG > 0) printf("%s -D- Nq bins (lower bounds in amu/e) --\n",thisprog);
    for (nq = 1; nq < 127; nq++){
      nqbins[nq] = 0.9*pow(kq,nq - 1);
      if (DEBUG > 0) {  
	printf(" %5.2e",nqbins[nq]); /* print out bins */
	if (!(nq % 8)) printf("\n"); /* break line every 8 */
      }
    }
    if (DEBUG > 0) printf("\n"); 

    fFirstRun = FALSE; /* we are done with things to run on first run only */
  }

  /* classify this mass/charge */
  nq = 1;
  while (nq < 127) {  /* to keep from overflowing array bounds */
    if ((stpha.moq > nqbins[nq]) && (stpha.moq < nqbins[nq + 1])){
	stpha.nq = nq;
	break;
    }
    
    nq++;
  }
    

  /**************************************/
  /* print decoded members if debug > 2 */
  /**************************************/
  if (DEBUG > 2) {
    char *members[] = {"stopId","startIdRng","energy","sector","ssdId","tof",
		       "ssdName"};
    int nmembers = 7;
    int *ps; /* pointer to stpha structure */

    ps = &stpha.stopId;

    printf("%s -D- ",thisprog);
    for (i=0; i < nmembers - 1; i++){
      printf("%s=%d ",members[i],*ps++);
    }
    printf("%s=%s\n",members[i],&stpha.ssdName[0]);
  }

  return(stpha);
}
