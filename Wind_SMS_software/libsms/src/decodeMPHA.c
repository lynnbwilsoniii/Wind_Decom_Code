/** \file decodeMPHA.c
    \brief Decode MASS PHA word into detailed structure
*/
/**
   Description: Breaks double MPHA word into component parts
   and returns mpha structure.  (where m/M stands for MASS)
   See DPU manual, pages 297.
*/

#include "libsms.h"

MPHA decodeMPHA(int nedb, int npha){ /* nedb ==> edb # ; npha ==> PHA word # */
  /* decode MASS PHA word */
  int i,j;  /* short range loop counters */
  char thisprog[] = "decodeMPHA"; /* name of this program */

  BYTE PhaWord[2];  /* 2 bytes/word for MASS PHA words */
  double dmpha; /* double MASS PHA word */

  MPHA smpha; /* mpha structure */

  /* constants and variables for PHA calc., defined in calc. code below */
  //double u; /* voltage at step; eoq calc. */
  //double Ed; /* digital energy; energy calc. */ 
  //double A1, A2, A3, A4, A5, A6, X, Y, lnM; /* A's are polynomial coef; mass */
  //double C1; /* constant; moq calc. */
  
  /* variables for mass and m/q classification */
  //double km; /* constant which defines nm bin size */
  //static int fFirstRun = TRUE; /* flag indicating first run of this routine */
  //int nm; /* nm loop counter */
  //static double nmbins[59]; /* array of NM lower mass bounds */
  //double kq; /* constant which defines nq bin size */
  //int nq; /* nq loop counter */
  //static double nqbins[128]; /* array of NQ lower mass/charge bounds */
  
  /*****************************/
  /* collect bytes of PHA word */
  /*****************************/
  /*++ There needs to be two of these, one for 'cycle.edb' and one for 'cycle.edb_r_h' ++*/

  /* jahf - 30Nov2005 - In decodeTPHA.c, these bytes are reversed, so
     that's what was originally done here for MASS. But then the TOF
     plot looked totally wrong. Not reversing the bytes resulted in
     the TOF looking much better. The jury is still out... */

  /* jahf - 01Dec2005 - Nope. NOT reversing the bytes here appears to
     be the correct thing to do (which is the opposite of STICS). I
     used wmpha.pl and plot_mphatofhist.pro (in libsms/tools) to
     create a MASS tof histogram and the plot looked decent when the
     bytes are NOT reversed here and looked wrong when I did reverse
     the bytes here. So it looks like MASS PHA works opposite of STICS
     PHA as far as byte order here. */

  for (j = 0; j < 2; j++) {
    if(isHigh == 1) {
      //PhaWord[j] = cycle.edb_r_h[nedb].mpha[2*npha + 1 - j]; /* reverse bytes */
      PhaWord[j] = cycle.edb_r_h[nedb].mpha[2*npha + j];
    }
    else if(isHigh == 0) {
      //PhaWord[j] = cycle.edb[nedb].mpha[2*npha + 1 - j];
      PhaWord[j] = cycle.edb[nedb].mpha[2*npha + j];
    }
  }

  dmpha = dBuildWord(PhaWord,0,1);
  
  /******************************************************/
  /* pull apart word in order of diagram in DPU manual */
  /******************************************************/
  smpha.sector = getBits(dmpha,15,15);
  smpha.range = getBits(dmpha,14,14);
  smpha.anode = getBits(dmpha,12,13);
  smpha.tof = getBits(dmpha,0,11);


  /***********************************************/
  /* calculate some quantities in physical units */
  /***********************************************/

  /* time of flight in nanoseconds */
  //smpha.tofns = (smpha.tof - 44)/2.3725306895;
  /* From SMS manual p.375-376:
     Tm=(Td-TOCi)/TADCi
     where
     Tm is the measured time-of-flight in ns,
     Td is the digital time-of-flight contained in the event word in [channel] 	    (0...4095)
     TOCi is the conversion offset of the time-of-flight ADC of the instrument 
     analog electronics in [channel] for i=0,1
     TADCi is the time-of-flight A/D conversion factor of the ADC in [channel/ns] for i=0,1
  */
  //smpha.tofns = (smpha.tof - X)/X;
  smpha.tofns = 0;

  /* mass */
  /*  TODO */
  smpha.mass = 0;

  /* mass/charge */
  /* TODO */
  smpha.moq = 0;

  /**************************************************/
  /* Classification functions -- Cham's thesis, p51 */
  /**************************************************/

  /* TODO: Do later if necessary.*/

  /****************/
  /* Return */
  /****************/
  return(smpha);
}
