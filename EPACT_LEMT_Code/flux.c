/*



        flux.c - This program processes Wind/LEMT and some APE 
				 encyclopedia (ency) files to produce flux files
				 at various averaging periods and PHA files. 
                 It uses subroutines that access data through a 
				 directory.
				 flux.exe runs from the DOS command line
				 This program now runs on Windows 7 - 32bit version
				 compiled by the Intel CODEBLDR C compiler using
				 subroutines contained therein.

        
        flux.c
        modified to get software rate verse for LEMT  11/1/94

        **********************************************************
        ****** Modified for 12-hr output - restore flux.sav ******
        **********************************************************

        July 14 2000 spin correction   8/1/00
        Dead-time corrections modified  1/8/01
        Change Quiet criterion to 2.5-3.2 MeV/n O   3/1/02
        fb correction set front & back to 4 sectors each  2//16/09
           list as frontQ & backQ
         (5/4/07  Live time multiplied by HW-COIN/HW-Pks OK) no change!

        compare() disabled 5/28/13 for noisey C7 & C8 (on feb 13, 2013)
        striptime[2] included to reduce livetime
           afterr C7 & C8 turnoff May 9 2013.
*/
#include <bios.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <memory.h>
#include <dos.h>
#include <time.h>
#include <ctype.h>
#include <malloc.h>                    /* sizes etc            */
#include <graph.h>
#include "size.h"                      /* global values,  array */
#include "panel.h"
#define EXT_ENCDAT  1
#define EXT_TDIR  1
#define EXT_EPACT 1
#include "enc.h"
#include "tdir.h"
#include "epact.h"
#include "pha.h"
#include  "encdat_proto.h"
#include  "tdir_proto.h"
#include  "tim_proto.h"

int short_file=0;   /* Deletes PH and sector verses from output */

extern struct PANEL pan[NPAN];
extern char *get_col();
short Max_type= 'I';
short Max_mult=1;

extern char Tbuf[];                   /* encdat.c */
extern short Prim;
extern short mul;
extern short Ncols;
extern short Nvers;


extern char timbf[20];
extern char  *pass_bl(char  *str);
extern char  *endof(char  *str);

short Push_key = -1;                     /* windo.c */
char Itok[4];
char cbuf[BLK];
union REGS inregs,outregs;
long Step_ph;

extern short doffset[3],eoffset[3];  /* offset corrections for lemt pulse heights */

#define  NABINS   47
#define  NBBINS   47
#define  NAVG     13    /* No. of averaging periods */
#define  EBINS    15
typedef struct
{
        double E0;
        double E1;
} EN_BIN;
EN_BIN En[EBINS]=                       /* Derived from Oxygen */
{       {  0.5 ,  1.0 },    /*  0 */
        {  1.0 ,  1.3 },    /*  1 */
        {  1.3 ,  1.65},    /*  2 */
        {  1.65,  2.1 },    /*  3 */
        {  2.1 ,  2.5 },    /*  4 */
        {  2.56,  3.17},    /*  5 */
        {  3.17,  3.88},    /*  6 */
        {  3.88,  4.68},    /*  7 */
        {  4.68,  6.0 },    /*  8 */
        {  6.0 ,  7.4 },    /*  9 */
        {  7.4 ,  9.2 },    /* 10 */
        {  9.2 , 13.4 },    /* 11 */
        { 13.4 , 20.0 },    /* 12 */
        { 20.0 , 30.0 },    /* 13 */
        { 30.0 , 50.0 }     /* 14 */
};
EN_BIN EnHe[EBINS]=
{       {  0.5 ,  1.0 },    /*  0x*/
        {  1.0 ,  1.3 },    /*  1x*/
        {  1.3 ,  1.65},    /*  2 */
        {  1.65,  2.0 },    /*  3 */
        {  2.0 ,  2.4 },    /*  4 */
        {  2.4 ,  3.0 },    /*  5 */
        {  3.0 ,  3.7 },    /*  6 */
        {  3.7 ,  4.53},    /*  7 */
        {  4.53,  6.0 },    /*  8 */
        {  6.0 ,  7.4 },    /*  9 */
        {  7.4 ,  9.6 },    /* 10?*/
        {  9.6 , 13.4 },    /* 11x*/
        { 13.4 , 20.0 },    /* 12x*/
        { 20.0 , 30.0 },    /* 13x*/
        { 30.0 , 50.0 }     /* 14x*/
};
EN_BIN EnC[EBINS]=
{       {  0.5 ,  1.0 },    /*  0x*/
        {  1.0 ,  1.3 },    /*  1x*/
        {  1.3 ,  1.65},    /*  2x*/
        {  1.65,  2.0 },    /*  3x*/
        {  2.0 ,  2.57},    /*3+4 */
        {  2.57,  3.19},    /*  5 */
        {  3.19,  3.85},    /*  6 */
        {  3.85,  4.8 },    /*  7 */
        {  4.8 ,  5.8 },    /*  8 */
        {  5.8 ,  7.2 },    /*  9 */
        {  7.2 ,  9.1 },    /* 10 */
        {  9.1 , 13.7 },    /* 11 */
        { 13.7 , 20.1 },    /* 12 */
        { 20.0 , 30.0 },    /* 13x*/
        { 30.0 , 50.0 }     /* 14x*/
};
EN_BIN EnNe[EBINS]=
{       {  0.5 ,  1.0 },    /*  0x*/
        {  1.0 ,  1.3 },    /*  1x*/
        {  1.3 ,  1.65},    /*  2x*/
        {  1.65,  2.1 },    /*  3x*/
        {  2.1 ,  2.5 },    /*  4x*/
        {  2.7 ,  3.27},    /*4+5 */
        {  3.27,  3.98},    /*  6 */
        {  3.98,  4.72},    /*  7 */
        {  4.72,  5.92},    /*  8 */
        {  5.92,  7.87},    /*  9 */
        {  7.87,  9.96},    /* 10 */
        {  9.96, 12.7 },    /* 11 */
        { 12.7 , 18.4 },    /* 12 */
        { 18.4 , 28.0 },    /* 13 */
        { 30.0 , 50.0 }     /* 14x*/
};
EN_BIN EnSi[EBINS]=                       /* Si  */
{       {  0.5 ,  1.0 },    /*  0 */
        {  1.0 ,  1.3 },    /*  1 */
        {  1.3 ,  1.65},    /*  2 */
        {  1.65,  2.1 },    /*  3 */
        {  2.1 ,  2.5 },    /* x4 */
        {  2.5 ,  3.2 },    /*  5 */
        {  3.2 ,  4.0 },    /*  6 */
        {  4.0 ,  4.9 },    /*  7 */
        {  4.9 ,  6.0 },    /*  8 */
        {  6.0 ,  7.9 },    /*  9 */
        {  7.9 ,  9.7 },    /* 10 */
        {  9.7 , 13.6 },    /* 11 */
        { 13.6 , 18.2 },    /* 12 */
        { 18.2 , 30.0 },    /* 13 */
        { 30.0 , 40.0 }     /* 14 */
};
EN_BIN EnFe[EBINS]=                       /* Fe  */
{       {  0.5 ,  1.0 },    /*  0 */
        {  1.0 ,  1.3 },    /*  1 */
        {  1.3 ,  1.65},    /*  2 */
        {  1.65,  2.1 },    /*  3 */
        {  2.1 ,  2.5 },    /* x4 */
        {  2.4 ,  3.0 },    /*  5 */
        {  3.0 ,  3.95},    /*  6 */
        {  3.95,  4.8 },    /*  7 */
        {  4.8 ,  5.9 },    /*  8 */
        {  5.9 ,  7.8 },    /*  9 */
        {  7.8 ,  9.3 },    /* 10 */
        {  9.3 , 12.5 },    /* 11 */
        { 12.5 , 18.2 },    /* 12 */
        { 18.2 , 24.0 },    /* 13 */
        { 24.0 , 40.0 }     /* 14 */
};
double Hgeom[EBINS]= { 1., 1., 1., 6., 8., 8., 1., 1., 1., 1., 1., 1., 1., 1.};
EN_BIN EnA[NABINS]=
{       {  4.6 ,   5.6},    /*  bkg  */
        {  4.6 ,   5.6},    /*  1 H  */
        {  5.6 ,   6.9},    /*  2 H  */
        {  6.9 ,   8.9},    /*  3 H  */
        {  8.9 ,  12.1},    /*  4 H  */
        { 12.1 ,  14.1},    /*  5 H  */
        { 14.1 ,  17.2},    /*  6 H  */
        { 17.2 ,  21.2},    /*  7 H  */
        { 21.2 ,  24.8},    /*  8 H  */
        {  4.4 ,  16.8},    /*  9 H2 */
        {  5.7 ,   7.0},    /* 10 He3*/
        {  7.0 ,   9.0},    /* 11 He3*/
        {  9.0 ,  12.1},    /* 12 He3*/
        { 12.1 ,  17.2},    /* 13 He3*/
        { 17.2 ,  25.9},    /* 14 He3*/
        { 25.9 ,  29.2},    /* 15 He3*/
        {  4.6 ,   5.6},    /* 16 He4*/
        {  5.6 ,   6.9},    /* 17 He4*/
        {  6.9 ,   8.9},    /* 18 He4*/
        {  8.9 ,  12.1},    /* 19 He4*/
        { 12.1 ,  14.1},    /* 20 He4*/
        { 14.1 ,  17.2},    /* 21 He4*/
        { 17.2 ,  21.2},    /* 22 He4*/
        { 21.2 ,  23.3},    /* 23 He4*/   /* was 24.6 */
        {  4.6 ,  6.4 },    /* 24 bkg*/
        {  8.7 ,  12.0},    /* 25 C  */
        { 12.0 ,  17.2},    /* 26 C  */
        { 17.2 ,  26.0},    /* 27 C  */
        { 26.0 ,  33.8},    /* 28 C  */
        { 33.8 ,  44.0},    /* 29 C  */
        {  8.7 ,  12.0},    /* 30 N  */
        { 12.0 ,  17.2},    /* 31 N  */
        { 17.2 ,  26.0},    /* 32 N  */
        { 26.0 ,  33.8},    /* 33 N  */
        { 33.8 ,  44.0},    /* 34 N  */
        {  8.7 ,  12.0},    /* 35 O  */
        { 12.0 ,  17.2},    /* 36 O  */
        { 17.2 ,  26.0},    /* 37 O  */
        { 26.0 ,  33.8},    /* 38 O  */
        { 33.8 ,  44.0},    /* 39 O  */
        {  8.7 ,  12.0},    /* 40 bkg*/
        { 10.0 ,  17.2},    /* 41 Ne */
        { 17.2 ,  26.0},    /* 42 Ne */
        { 26.0 ,  44.0},    /* 43 Ne */
        { 17.2 ,  26.0},    /* 44 Fe */
        { 26.0 ,  44.0},    /* 45 Fe */
        { 44.0 ,  80.0}     /* 46 Fe */
};
EN_BIN EnB[NBBINS]=
{       { 18.9 ,  21.9},    /*  bkg  */
        { 18.9 ,  21.9},    /*  1 H  */
        { 21.9 ,  24.5},    /*  2 H  */
        { 24.5 ,  27.9},    /*  3 H  */
        { 27.9 ,  31.9},    /*  4 H  */
        { 31.9 ,  39.8},    /*  5 H  */
        { 39.8 ,  44.3},    /*  6 H  */
        { 44.3 ,  49.3},    /*  7 H  */
        { 49.3 ,  57.8},    /*  8 H  */
        { 57.8 ,  70.9},    /*  9 H  */
        { 19.0 ,  24.1},    /* 10 H2 */
        { 24.1 ,  31.5},    /* 11 H2 */
        { 31.5 ,  44.4},    /* 12 H2 */
        { 22.3 ,  27.6},    /* 13 He3*/
        { 27.6 ,  32.8},    /* 14 He3*/
        { 32.8 ,  39.7},    /* 15 He3*/
        { 39.7 ,  49.9},    /* 16 He3*/
        { 49.9 ,  71.4},    /* 17 He3*/
        { 18.1 ,  22.0},    /* 18 He4*/   /* was 18.9 */
        { 22.0 ,  24.5},    /* 19 He4*/
        { 24.5 ,  27.9},    /* 20 He4*/
        { 27.9 ,  32.0},    /* 21 He4*/
        { 32.0 ,  39.9},    /* 22 He4*/
        { 39.9 ,  44.3},    /* 23 He4*/
        { 44.3 ,  49.3},    /* 24 He4*/
        { 49.3 ,  57.8},    /* 25 He4*/
        { 57.8 ,  70.9},    /* 26 He4*/
        { 39.0 ,  49.0},    /* 27 bkg*/
        { 39.0 ,  49.0},    /* 28 C  */
        { 49.0 ,  71.0},    /* 29 C  */
        { 71.0 ,  86.0},    /* 30 C  */
        { 86.0 , 132.0},    /* 31 C  */
        { 39.0 ,  49.0},    /* 32 N  */
        { 49.0 ,  71.0},    /* 33 N  */
        { 71.0 ,  86.0},    /* 34 N  */
        { 86.0 , 105.0},    /* 35 N  */
        {105.0 , 132.0},    /* 36 N  */
        { 39.0 ,  49.0},    /* 37 O  */
        { 49.0 ,  71.0},    /* 38 O  */
        { 71.0 ,  86.0},    /* 39 O  */
        { 86.0 , 105.0},    /* 40 O  */
        {105.0 , 132.0},    /* 41 O  */
        {132.0 , 156.0},    /* 42 O  */
        { 39.0 ,  49.0},    /* 43 bkg*/
        { 70.0 , 105.0},    /* 44 Fe */
        {105.0 , 156.0},    /* 45 Fe */
        {156.0 , 250.0}     /* 46 Fe */
};

typedef struct
{
        short vnbr;
        float rcnt;
        float rtim;
        long  mcnt;
        long  lfbin[47];
} OUT_VRS;
OUT_VRS  ov;

/*  PH binning variables */

#define NPART   47
#define NFINEZ  386

struct SPECI
{
        double z;
        double a;
        char *name;
        short wt;     /* Wt=1 will overwrite in bin table, 0 won't */
        short resp;   /* Select curve for plot response output     */
};

extern struct SPECI spec[NPART];

extern double Ebin[16];
/*   0    1    2     3    4    5    6    7    8    9   10    11  12     13   14    15
{ 0.5, 1.0, 1.4, 1.75, 2.1, 2.5, 3.2, 4.0, 5.0, 6.2, 8.,  10., 14.,  20.,  30., 50.  };
*/

extern long Binbuf[NPART][16];  /* PH counts, by species and energy */
extern long Binzbuf[NFINEZ][8];  /* PH counts, by .2*Z and energy */
extern int Win[2][16];          /* Window PH counts, input & binned */
                                /* - note this differs from the win[3][16]
                                   buffer defined below */
extern long Bin149;
int LastlowZ;  /* used for output of Z>33 fluxes */
/*  End PH binning variables */



double SpinPer=3.;      /* Spin period sec. */

#define NPASS   52
char Belts[NPASS][2][16]={  {"94 Nov 16 0800","94 Nov 16 1900"},
                            {"94 Nov 30 2100","94 Dec  1 1000"},
                            {"94 Dec 12 1330","94 Dec 13 0130"},
                            {"94 Dec 24 1100","94 Dec 25 0000"},
                            {"95 Aug  2 1200","95 Aug  2 2000"}, /* Shadow -no HV */
                            {"95 Aug 20 0000","95 Aug 20 1500"},
                            {"95 Nov 29 0400","95 Nov 29 1800"}, /* Shadow - LEMT off */
                            {"95 Dec 21 1700","95 Dec 22 0400"},
                            {"96 Jan 13 0200","96 Jan 13 0700"},
                            {"96 Apr 18 0500","96 Apr 18 1600"},
                            {"96 Aug 19 1900","96 Aug 20 0900"},
                            {"96 Sep 11 1400","96 Sep 12 0600"},
                            {"96 Oct  4 0100","96 Oct  4 2100"},
                            {"96 Nov 15 1600","96 Nov 16 1100"},
                            {"96 Dec  2 2000","96 Dec  3 1400"},
                            {"97 Jun 11 2300","97 Jun 12 0700"},
                            {"97 Jul  3 2100","97 Jul  4 0900"},
                            {"97 Jul 26 0500","97 Jul 26 0700"},
                            {"97 Sep  8 1600","97 Sep  8 2300"},
                            {"98 jul 24 1500","98 jul 25 0300"},
                            {"98 aug 15 2100","98 aug 16 0600"},
                            {"98 oct 26 0600","98 oct 26 1600"}, /* EPACT off */
                            {"98 nov 13 1200","98 nov 13 2300"}, /* EPACT off */
                            {"99 jan 18 0900","99 jan 18 1800"},
                            {"99 feb  4 1800","99 feb  5 0000"},
                            {"99 feb 22 1100","99 feb 22 1800"},
                            {"99 mar 11 1600","99 mar 12 0300"},
                            {"99 mar 29 0100","99 mar 29 1300"},
                            {"99 aug 18 1600","99 aug 19 0300"},
                            {"99 sep  9 0400","99 sep  9 1200"},
                            {"99 sep 30 0600","99 oct  1 0600"}, /* ~1st & 15th to Jan */
                            {"00 jan 16 1200","00 jan 16 2200"},
                            {"00 apr  9 1800","00 apr 10 1200"},
                            {"00 may  2 1800","00 may  3 1200"},
                            {"00 may 25 1800","00 may 26 1000"},
                            {"00 jun 10 0500","00 jun 10 1700"},
                            {"00 jun 26 1700","00 jun 27 1800"}, /* EPACT off*/
                            {"00 jul  9 2200","00 jul 10 1000"},
                            {"00 jul 23 0500","00 jul 23 1500"},
                            {"00 aug  3 2000","00 aug  4 1000"},
                            {"00 aug 15 1900","00 aug 16 0900"},
                            {"01 oct  3 1000","01 oct  3 1800"},
                            {"01 oct 23 1600","01 oct 23 2100"},
                            {"01 nov 14 0700","01 nov 14 1900"},
                            {"01 dec  2 0700","01 dec  2 1400"},
                            {"02 jul 21 1330","02 jul 22 0100"},
                            {"02 aug  9 2350","02 aug 10 0940"},
                            {"02 aug 29 2220","02 aug 30 0920"},
                            {"02 sep 17 2030","02 sep 18 1200"},
                            {"02 oct  9 1830","02 oct 10 0700"},
                            {"02 nov  1 0930","02 nov  1 1500"},
                            {"02 nov 27 2000","02 nov 28 0600"}
                          };
long BeltTime[NPASS][2];
int Inbelts;
#define NEVENT  94
char EventT[NEVENT][2][16]={   /* for Quiet */
 {"94 Nov  3 0000","94 Dec  2 0000"},
 {"94 Dec  4 0600","94 Dec 17 0000"},
 {"94 Dec 23 1000","94 Dec 27 0800"},
 {"94 Dec 31 0000","95 Jan 14 0000"},
 {"95 Jan 15 1600","95 Jan 20 1800"},
 {"95 Jan 25 2200","95 Feb 18 1200"},
 {"95 Feb 19 2000","95 Feb 23 0200"},
 {"95 Feb 25 1600","95 Mar 19 0600"},
 {"95 Mar 22 0000","95 Mar 25 0800"},
 {"95 Mar 28 2000","95 Apr  5 1400"},
 {"95 Apr  7 0000","95 Apr 18 0200"},
 {"95 Apr 22 0800","95 may 14 1000"},
 {"95 May 15 2000","95 May 16 0800"},
 {"95 May 21 1400","95 May 27 1200"},
 {"95 May 29 1600","95 Jun 16 1600"},
 {"95 Jun 18 1000","95 Jun 26 0400"},
 {"95 Jun 26 2000","95 Jul 10 0000"},
 {"95 Jul 15 0200","95 Jul 19 0000"},
 {"95 Jul 21 1200","95 Jul 24 0800"},
 {"95 Jul 25 2200","95 Aug  3 1400"},
 {"95 Aug  8 1800","95 Aug  9 1600"},
 {"95 Aug 10 1800","95 Aug 14 1400"},
 {"95 Aug 24 2000","95 Sep  3 2200"},
 {"95 Sep  5 0000","95 Sep 19 2000"},
 {"95 Sep 23 0000","95 Sep 28 0000"},
 {"95 Oct  2 1400","95 Oct 27 1400"},
 {"95 Oct 31 0800","95 Nov 12 0000"},
 {"95 Nov 27 0800","95 Dec  7 0000"},
 {"95 Dec 11 0800","95 Dec 15 1200"},
 {"95 Dec 23 0600","96 Jan  2 2000"},
 {"96 Jan 13 0600","96 Jan 14 1800"},
 {"96 Jan 19 1600","96 Jan 25 0800"},
 {"96 Apr 20 0400","96 Apr 23 1200"},
 {"96 May 13 1200","96 May 17 1600"},
 {"96 May 21 0800","96 May 21 1800"},
 {"96 May 24 1400","96 Jun  4 0400"},
 {"96 Jun 20 0800","96 Jun 27 1200"},
 {"96 Jul  1 1600","96 Jul  2 1800"},
 {"96 Jul  8 0800","96 Jul 20 0400"},
 {"96 Aug  9 0800","96 Aug 17 0000"},
 {"96 Aug 26 1400","96 Aug 29 1400"},
 {"96 Sep  9 1200","96 Sep 19 1600"},
 {"96 Sep 27 0800","96 Sep 29 0800"},
 {"96 Oct  6 0600","96 Oct 14 0000"},
 {"96 Nov  1 2200","96 Nov 13 2000"},
 {"96 Nov 24 0800","96 Dec  8 0200"},
 {"96 Dec 23 2200","96 Dec 31 0200"},
 {"97 Jan 20 0000","97 Jan 22 1600"},
 {"97 Feb  7 0600","97 Feb 10 0600"},
 {"97 Feb 25 2200","97 Feb 27 0400"},
 {"97 Apr  1 1600","97 Apr 17 0200"},
 {"97 Apr 20 1000","97 Apr 23 1200"},
 {"97 May 12 0200","97 May 30 0000"},
 {"97 Jun 10 0000","97 Jun 11 1000"},
 {"97 Jul 23 1400","97 Jul 30 0200"},
 {"97 Aug 10 0400","97 Aug 13 1000"},
 {"97 Sep  2 0000","97 Sep  5 0000"},
 {"97 Sep 10 0000","97 Oct  1 1200"},
 {"97 Oct  7 1600","97 Oct 11 0000"},
 {"97 Oct 12 0800","97 Oct 26 2200"},
 {"97 Nov  1 0200","97 Dec  2 1200"},
 {"97 Dec  4 1600","97 Dec 17 0600"},
 {"97 Dec 19 0800","97 Dec 21 0600"},
 {"97 Dec 29 2000","97 Dec 30 1800"},
 {"98 Jan  2 1400","98 Jan  7 0400"},
 {"98 Jan 11 1200","98 Jan 12 1400"},
 {"98 Jan 19 0800","98 Jan 21 1200"},
 {"98 Jan 26 0000","98 Feb  1 0000"},
 {"98 Feb  5 0600","98 Feb  7 2200"},
 {"98 Feb 15 1800","98 Feb 18 0800"},
 {"98 Feb 20 0000","98 Mar  2 1200"},
 {"98 Mar  7 1000","98 Mar 10 1800"},
 {"98 Mar 21 2000","98 jul 16 0000"},
 {"98 jul 16 1200","98 oct  8 0000"},
 {"98 oct 15 0600","98 dec 14 0000"},
 {"98 dec 17 0900","99 mar 23 1200"},
 {"99 mar 26 0000","99 mar 30 0000"},
 {"99 mar 31 0000","99 apr  3 0600"},
 {"99 apr  7 0600","99 may  1 0000"},
 {"99 may  3 0800","99 jul 23 0000"},
 {"99 jul 24 1200","99 sep  1 0600"},
 {"99 sep  2 1800","99 oct  2 1500"},
 {"99 oct  8 1800","99 dec 17 0000"},
 {"99 dec 19 0800","00 may 31 1200"},
 {"00 jun  2 1500","00 jul  4 1200"},
 {"00 jul  7 1600","00 oct  7 1600"},
 {"00 oct  9 1200","01 feb  8 1600"},
 {"01 feb 10 1600","01 feb 22 0000"},
 {"01 feb 24 0300","01 mar  5 1600"},
 {"01 mar  7 0000","01 may  3 1600"},
 {"01 may  7 0800","01 jun 29 0000"},
 {"01 jul  2 0000","01 jul 17 1800"},
 {"01 jul 19 1200","01 jul 26 0000"},
 {"01 jul 28 0600","01 sep 30 0000"}
                         };
long EventTime[NEVENT][2];
/* Times of change in threshold levels of LEMT */
char Threslvl[2][16]= {"95 Jun 30 1546", "95 Jul  5 1559"};
long Threshtime[2];
int Thrstate;
/* Times of C E-y strip 2 failure & turnoff */
/* Striptime 3 is turnoff of dome detectors C7 and C8  */
char Strip_t[3][16]= {"99 Mar 11 1900", "99 May 14 1515", "13 May  9 1555"};
long Striptime[3];

char *ApeAEnd="1995 feb 27 0500";
char *ApeBEnd="1995 oct  8 0000";
long AEnd, BEnd, StartT, EndT;
/*
char Starttime[]="1994 Nov  3 0000";
char Endtime[]="2004 apr 12 0000";

char Starttime[]="2004 apr 12 0000";
char Endtime[]="2007 nov 20 0000";

char Starttime[]="2007 nov 20 0000";
char Endtime[]="2013 feb 13 0000";

char Starttime[]="2013 feb 13 0000";
char Endtime[]="2019 Apr 29 0000";
*/

char Starttime[]="2013 feb 13 0000";
char Endtime[]="2020 Jan 13 0000";

unsigned char *Sptr;
int Spin[3];
long TimeBin[NAVG];
long Firstt, Lastt;
float RateCount, LiveTime;
float hwrate[3][8];
float win[3][16];
double Nprot[3], Nhe[3], Nhvy[3];
double Win1_5[3], Win6_12[3], Win13_15[3];
short *ApeASW, *ApeBSW;
long count;
long errors, maxerrs;

typedef struct
{
        long Tint[2];
        long  Maj[2];
        float HWrate[3][8];
        float WinA[3][16];
        float DomeThr[3][16];
        float DomeSum[3];
        float Nprots[3];
        float Nhes[3];
        float Nhvys[3];
        float Noxy[3];
        float Spind[3];
        float HWApeA[16];
        float HWApeB[16];
        float HWIT[32];
        float ApeSpins[3];
        float ApeFrames[3];
        long  ApeAbox[NABINS];
        long  ApeBbox[NBBINS];
        double Sprot;
        double She;
        double Shvy;
        double Sum1_5;
        double Sum6_12;
        double Sum13_15;
        double Spins;
        double Live_spins;
        double He_spins;
        double H_spins;
        long SWbox[150];
        long ASWbox[150];
        long BSWbox[150];
        long CSWbox[150];
        double Allbox;
        int Maxthr;
        long PHbox[NPART][16];
        long PHzbox[NFINEZ][8];
        long PHwina[2][16];
        long PH149;
        double PHwin6_12;
        double W15;
        long Magsec[5][16];
} SUMBLOCK;

SUMBLOCK Avg[NAVG];

#define TEMPAVG   1     /* save-restore temporary averages */

long AvgInt=   900;           /* Flux Averaging Interval */
double Avgcnt[NAVG]={ 1.,  4.,32.,  96., 288., 2592., 9504., 0., 0.,
                        96., 288., 2592., 10368.};
FILE  *Ofi, *Ffi[NAVG], *Rfi[NAVG], *Sfi[NAVG], *Efi, *Avgfi;
FILE  *Phfi, *Dirfi[3], *Qfi;
char *Ona="dead.out";
char *Ena="error.log";
char *Fna[NAVG]={"c:\\wind\\flux\\flx15m.out",  /* "",  */
                 "c:\\wind\\flux\\flx1hr.out",  /* "",  */
                 "c:\\wind\\flux\\flx8hr.out",  /* "",  */
                 "c:\\wind\\flux\\flx1da.out",
                 "c:\\wind\\flux\\flx3da.out",
                 "c:\\wind\\flux\\flx27d.out","","","",
                 "c:\\wind\\flux\\qflx1da.out",
                 "c:\\wind\\flux\\qflx3da.out",
                 "c:\\wind\\flux\\qflx27d.out",
                 "c:\\wind\\flux\\qflx108d.out"};
/* NOTE: Rate file will only be written if there is a corresp. flux file */
char *Rna[NAVG]={"",                     /* "c:\\wind\\flux\\rt15m.out"  */
                 "c:\\wind\\flux\\rt1hr.out",  /* ""  */
                 "",  /* "c:\\wind\\flux\\rt8hr.out"  */
                 "",  /* "c:\\wind\\flux\\rt1da.out"  */
                 "",                     /* "c:\\wind\\flux\\rt6da.out"  */
                 "","","","",            /* "c:\\wind\\flux\\rt27d.out"  */
                 "c:\\wind\\flux\\qrt1da.out",
                 "",                     /* "c:\\wind\\flux\\qrt6da.out"  */
                 "",                     /* "c:\\wind\\flux\\qrt27d.out"  */
                 ""};                    /* "c:\\wind\\flux\\qrt6mo.out"  */
char *Sna[NAVG]={ "","",
                  "",  /* "c:\\wind\\flux\\spec8hr.out", */
                  "",  /* "c:\\wind\\flux\\spec1da.out", */
                  "",  /* "c:\\wind\\flux\\spec6da.out", */
                  "c:\\wind\\flux\\spec27d.out",
                  "c:\\wind\\flux\\specsum.out",
                  "c:\\wind\\flux\\spquiet.out",
                  "c:\\wind\\flux\\spfefre.out",
                  "",  /* "c:\\wind\\flux\\qspec1da.out", */
                  "",  /* "c:\\wind\\flux\\qspec6da.out", */
                  "c:\\wind\\flux\\qspec27d.out",
                  "c:\\wind\\flux\\qspec108.out"};
int Close;

 char *Phna= "c:\\wind\\pha\\phax.out";
/* char *Phna= "";        kill output */
char *Dna[3]= {"c:\\wind\\pha\\phax.dir",    //c:\\wind\\pha\\phax.dir
               "c:\\wind\\pha\\phaqx.dir",    //c:\\wind\\pha\\phaqx.dir
               "c:\\wind\\pha\\phafx.dir"};   //c:\\wind\\pha\\phafx.dir
char *Qna= "c:\\wind\\pha\\quiet.out";
long Dirpos[3];
long Nchapt[3];
#define NPUSH   30
long Pchap[NPUSH][2];
int Ipush;
int     LowFeVal;
double Fquiet[2];      /* 6-hr Avg intensity of 2 - 3.7 MeV/n 4He */
double FquietO[2];      /* 6-hr Avg intensity of 2.5 - 3.3 MeV/n O */
double QcritHe=.3e-4;   /* Quiet criterion He was 3.e-5 ( < 5/2/01)*/
                       /* was 2e-4 before 1/25/10, changed back, 3He added  */
double QcritO=2.e-5;    /* Quiet criterion 2e-5 based on 2.5-3.2 O, not He */
                        /* change O to C  1/24/10 no effect, changed back */
double FeCrit=1.e-3;   /* Max. int. 2-3.7 4He for "Fe-free" */
int Fullspec=0;         /* Output full spectrum, APE, etc   */

#define  NPHB   10000
unsigned short Phabuf[NPHB];
int  Ipha=1, Npha;


#define INFILES 59
char *Ency[INFILES]={"c:\\wind\\ency\\ency01.dir",   /* Nov  2 - Dec 31 94 */
                     "c:\\wind\\ency\\ency02.dir",   /* Dec 31 - Apr  9 95 */
                     "c:\\wind\\ency\\ency03.dir",   /* Apr  9 - Jul 14 95 */
                     "c:\\wind\\ency\\ency04.dir",   /* Jul 14 - Sep 10 95 */
                     "c:\\wind\\ency\\ency05.dir",   /* Sep 10 - Nov 28 95 */
                     "c:\\wind\\ency\\ency06.dir",   /* Nov 28 - Jan 21 96 */
                     "c:\\wind\\ency\\ency07.dir",   /* Jan 21 - Apr  8 96 */
                     "c:\\wind\\ency\\ency08.dir",   /* Apr  8 - Jun 10 96 */
                     "c:\\wind\\ency\\ency09.dir",   /* Jun 10 - Oct 28 96 */
                     "c:\\wind\\ency\\ency10.dir",   /* Oct 28 - Apr  1 97 */
#if 0
char *Ency[INFILES]={
#endif
                     "c:\\wind\\ency\\ency11.dir",   /* Apr  1 - Oct 23 97 */
                     "c:\\wind\\ency\\ency12.dir",   /* Oct 23 - Apr  2 98 */
                     "c:\\wind\\ency\\ency13.dir",   /* Apr  2 - sep  9 98 */
                     "c:\\wind\\ency\\ency14.dir",   /* sep  9 - jan  1 99 */
                     "c:\\wind\\ency\\ency15.dir",   /* jan  1 - jul  5 99 */
                     "c:\\wind\\ency\\ency16.dir",   /* jul  5 - nov 28 99 */
                     "c:\\wind\\ency\\ency17.dir",   /* nov 28 - apr 21 00 */
                     "c:\\wind\\ency\\ency18.dir",   /* apr 21 - sep 24 00 */
                     "c:\\wind\\ency\\ency19.dir",   /* sep 24 - mar 26 01 */
                     "c:\\wind\\ency\\ency20.dir",   /* mar 26 - aug 19 01 */
                     "c:\\wind\\ency\\ency21.dir",   /* aug 19 - jan  1 02 */
                     "c:\\wind\\ency\\ency22.dir",   /* jan  1 - jul 15 02 */
                     "c:\\wind\\ency\\ency23.dir",   /* jul 15 - feb  1 03 */
                     "c:\\wind\\ency\\ency24.dir",   /* feb  1 - sep 15 03 */
                     "c:\\wind\\ency\\ency25.dir",   /* sep 15 - mar  8 04 */
                     "c:\\wind\\ency\\ency26.dir",   /* mar  8 - aug 11 04 */
                     "c:\\wind\\ency\\ency27.dir",   /* aug 11 - jan 31 05 */
                     "c:\\wind\\ency\\ency28.dir",   /* jan 31 - jul 26 05 */
                     "c:\\wind\\ency\\ency29.dir",   /* jul 26 - jan  1 06 */
                     "c:\\wind\\ency\\ency30.dir",   /* jan  1 - sep 28 06 */
                     "c:\\wind\\ency\\ency31.dir",   /* sep 28 - jan  1 07 */
                     "c:\\wind\\ency\\ency32.dir",   /* jan  1 - jun 13 07 */
                     "c:\\wind\\ency\\ency33.dir",   /* jun 13 - nov 20 07 */
                     "c:\\wind\\ency\\ency34.dir",   /* nov 20 - may 12 08 */
                     "c:\\wind\\ency\\ency35.dir",   /* may 12 - dec  8 08 */
                     "c:\\wind\\ency\\ency36.dir",   /* dec  8 - jun  8 09 */
                     "c:\\wind\\ency\\ency37.dir",   /* jun  8 - nov 30 09 */
                     "c:\\wind\\ency\\ency38.dir",   /* nov 30 - may 14 10 */
                     "c:\\wind\\ency\\ency39.dir",   /* may 24 - dec 06 10 */
                     "c:\\wind\\ency\\ency40.dir",   /* dec  6 - may  9 11 */
                     "c:\\wind\\ency\\ency41.dir",   /* may  9 - oct 24 11 */
                     "c:\\wind\\ency\\ency42.dir",   /* oct 24 - mar 19 12 */
                     "c:\\wind\\ency\\ency43.dir",   /* mar 19 - oct 15 12 */
                     "c:\\wind\\ency\\ency44.dir",   /* oct 15 - may 13 13 */
                     "c:\\wind\\ency\\ency45.dir",   /* may 13 - dec  2 13 */
                     "c:\\wind\\ency\\ency46.dir",   /* dec  2 - jun 16 14 */
                     "c:\\wind\\ency\\ency47.dir",   /* jun 16 - nov 14 14 */
                     "c:\\wind\\ency\\ency48.dir",   /* nov 14 - mar 30 15 */
                     "c:\\wind\\ency\\ency49.dir",   /* mar 30 - jan  1 16 */
                     "c:\\wind\\ency\\ency50.dir",   /* jan  1 - May  2 16 */
                     "c:\\wind\\ency\\ency51.dir",   /* May  2 - Oct 31 16 */
                     "c:\\wind\\ency\\ency52.dir",   /* Oct 31 - Feb 20 17 */
                     "c:\\wind\\ency\\ency53.dir",   /* Feb 20 - Jul 24 17 */
                     "c:\\wind\\ency\\ency54.dir",   /* Jul 24 - jan  1 18 */
                     "c:\\wind\\ency\\ency55.dir",   /* Jan  1 - may 28 18 */
                     "c:\\wind\\ency\\ency56.dir",   /* May 28 - Oct  8 18 */
                     "c:\\wind\\ency\\ency57.dir",   /* Oct  8 - Apr  1 19 */
                     "c:\\wind\\ency\\ency58.dir",   /* Apr  1 - Aug 26 19 */
                     "c:\\wind\\ency\\ency59.dir"};  /* Aug 26 - Jan 13 20 */
int Auto, Fullauto;

int Chargefile=0;            /* Enable output of file of charges of
                                individual particles, 0 for flux only.
                                See lemtres.c                           */
extern FILE *Chfi;
extern char *Chna;

double sumd[3],sume[3],sumn[3],sumi;
char dat[10], tim[10];

int Noutdir;  /*Dir opens-closes */

main(argc, argv)
int   argc;
char  *argv[];

{
        char *ptr;
        int i;
        int nfialloc;

        if ((nfialloc=_init_handle_count(40))==-1)
                /* Number of concurrently open files */
        {
                puts("Can't allocate 40 files");
                puts(_strerror(NULL));
                exit(1);
        }
        printf(" Files allocated = %2d\n",nfialloc);
         init_params();
        if ((Efi=fopen(Ena,"w"))==0)
        {
                puts("Cannot open file ");
                puts(Ena);
                exit(1);
        }
        if ( argc>1)
        {
                F_name[0]=argv[1];
                new_file(0);            /* Read header and init input file */
        }
        else
        {
                Fullauto=1;
                auto_file();

/*                printf("usage: flux  encyfile.in");
                to_dos();
*/
        }
        Nfi=1;
        _strdate(dat);                   /* current date */
        _strtime(tim);                   /* current time */

        if ((Ofi=fopen(Ona,"w"))==0)
        {
                puts("Cannot open file ");
                puts(Ona);
                exit(1);
        }
        fprintf(Ofi,"ASCII \n");
        fprintf(Ofi,"Dead time rates \n");
        fprintf(Ofi,"#0  Rate \n");
        fprintf(Ofi,"I1   0     100   Rel. day \n");
        fprintf(Ofi,"F1   0     10000  Dome Sum Dth Rate c/s \n");
        fprintf(Ofi,"F1   0     12000  Epeaks Rate c/s \n");
        fprintf(Ofi,"F1   1     600  N Ox \n");
        fprintf(Ofi,"F1   0     3     Tel No. \n");
        fprintf(Ofi,"#End\n");

        AEnd=sscantime(ApeAEnd, &ptr);
        BEnd=sscantime(ApeBEnd, &ptr);
        StartT=sscantime(Starttime, &ptr);
        EndT=sscantime(Endtime, &ptr);
        for (i=0; i<NPASS; i++)
        {
                BeltTime[i][0]=sscantime(Belts[i][0],&ptr);
                BeltTime[i][1]=sscantime(Belts[i][1],&ptr);
        }
        for (i=0; i<NEVENT; i++)
        {
                EventTime[i][0]=sscantime(EventT[i][0],&ptr);
                EventTime[i][1]=sscantime(EventT[i][1],&ptr);
        }
        for (i=0; i<2; i++)
        {
                Threshtime[i]=sscantime(Threslvl[i],&ptr);
                Striptime[i]=sscantime(Strip_t[i],&ptr);
        }
                Striptime[2]=sscantime(Strip_t[2],&ptr);
        for (i=0; i<NAVG; i++)
        {
                if (!*Fna[i])
                        continue;
                if ((Ffi[i]=fopen(Fna[i],"wb"))==0)
                {
                        puts("Cannot open file ");
                        puts(Fna[i]);
                        exit(1);
                }
                if (i<7)
                    printf("Flux output %5.2lf hr avg on file %s\n"
                        ,AvgInt*Avgcnt[i]/3600.,Fna[i]);
                if (i>8)
                    printf("Quiet-time Flux output %5.2lf day avg on file %s\n"
                        ,AvgInt*Avgcnt[i]/3600./24.,Fna[i]);
                if  (*Rna[i] && (Rfi[i]=fopen(Rna[i],"wb")) )
                {
                    if (i<7)
                        printf("Rate output %5.2lf hr avg on file %s\n"
                            ,AvgInt*Avgcnt[i]/3600.,Rna[i]);
                    if (i>8)
                        printf("Quiet-time Rate output %5.2lf day avg on file %s\n"
                             ,AvgInt*Avgcnt[i]/3600./24.,Rna[i]);
                }
                OutFlxHdr(i);   /* closes files */
        }
        for (i=2; i<NAVG; i++)
        {
                if (!*Sna[i])
                        continue;
                if ((Sfi[i]=fopen(Sna[i],"wb"))==0)
                {
                        puts("Cannot open file ");
                        puts(Sna[i]);
                        exit(1);
                }
                if (i<7)
                    printf("Spectrum output %5.2lf day avg on file %s\n"
                        ,AvgInt*Avgcnt[i]/3600./24.,Sna[i]);
                else if (i==7)
                    printf("Quiet time spectrum, %s\n",Sna[i]);
                else if (i==8)
                    printf("Low-Fe Spectrum (no solar events), %s\n",Sna[i]);
                else if (i>8)
                    printf("Quiet Spectrum output %5.2lf day avg on file %s\n"
                        ,AvgInt*Avgcnt[i]/3600./24.,Sna[i]);
                OutSpectHdr(i);
                fclose(Sfi[i]);
                Sfi[i]=0;
        }
        if (*Phna && (Phfi=fopen(Phna,"wb"))==0)
        {
                puts("Cannot open file ");
                puts(Phna);
                exit(1);
        }
        else if (*Phna)
        {
                fprintf(Phfi,"BINARY \r\n");
                fprintf(Phfi,"EPACT \r\n");
                fprintf(Phfi,"LEMT compressed pulse height data\r\n");
                fprintf(Phfi," %s\r\n",Starttime);
                fprintf(Phfi," %s\r\n",Endtime);
                fprintf(Phfi," 0  0  0002\r\n");
                fprintf(Phfi,"#0 Rate\r\n");
                fprintf(Phfi," T2         Time\r\n");
                fprintf(Phfi,"#x2a00 PHA                     \r\n");
                fprintf(Phfi," I     0    24000    byte_cnt  \r\n");
                fprintf(Phfi," U     0   65535     Dp        \r\n");
                fprintf(Phfi," U     0   65535     Ep        \r\n");
                fprintf(Phfi," I     0     8       D_fast    \r\n");
                fprintf(Phfi," I     0     8       E_fast    \r\n");
                fprintf(Phfi," I     0    16       D_add     \r\n");
                fprintf(Phfi," I     0     8       E_x       \r\n");
                fprintf(Phfi," I     0     8       E_y       \r\n");
                fprintf(Phfi," I     0     2       E_x_pres  \r\n");
                fprintf(Phfi," I     0     3       Tel_num   \r\n");
                fprintf(Phfi," I     0    16       Win_A     \r\n");
                fprintf(Phfi," I     0     8       Win_B     \r\n");
                fprintf(Phfi," I     0    64       Spinsec   \r\n");
                fprintf(Phfi," I     0   255       SW Bin    \r\n");
                fprintf(Phfi," F    16  65000      Dph_offset\r\n");
                fprintf(Phfi," F    10  65000      Eph_offset\r\n");
                fprintf(Phfi," F    30   150       D_long    \r\n");
                fprintf(Phfi," F   -50    50       D_lat     \r\n");
                fprintf(Phfi," F    16  65000      D_corr    \r\n");
                fprintf(Phfi," F    10  65000      E_corr    \r\n");
                fprintf(Phfi,"#End\r\n");
                fclose(Phfi);
                Phfi=0;
        }
        for (i=0; i<3; i++)
        {
                if (!*Phna || !*Dna[i])
                        continue;
                if ((Dirfi[i]=fopen(Dna[i],"wb"))==0)
                {
                        puts("Cannot open file ");
                        puts(Dna[i]);
                        exit(1);
                }
                fprintf(Dirfi[i],"; %s\r\n",Dna[i]);
                fprintf(Dirfi[i],"Directory  created %.2s/%.2s/%.2s %s\r\n",
                    dat+6,dat,dat+3,tim);
                printf("Directory %s  created %.2s/%.2s/%.2s %s\n",
                    Dna[i],dat+6,dat,dat+3,tim);
                fprintf(Dirfi[i],"%-18s",Phna);
                fprintf(Dirfi[i]," %18s",Starttime);
                fprintf(Dirfi[i]," %18s",Endtime);
                Dirpos[i]=ftell(Dirfi[i]);
                fprintf(Dirfi[i],"          0          0\r\n");
                fprintf(Dirfi[i],"#End\r\n");
                fclose(Dirfi[i]);
                Dirfi[i]=0;
        }
        if ((Qfi=fopen(Qna,"w"))==0)    /* Quiet time list */
        {
                puts("Cannot open file ");
                puts(Qna);
                exit(1);
        }
        else
        {
                fprintf(Qfi,"ASCII \n");
                fprintf(Qfi,"Quiet time PHAs \n");
                fprintf(Qfi," %18s\n",Starttime);
                fprintf(Qfi," %18s\n",Endtime);
                fprintf(Qfi," 0   0   0600\n");
                fprintf(Qfi,"#0  Rate \n") ;
                fprintf(Qfi,"T2                time\n");
                fprintf(Qfi,"F2  1e-6   %8.1lg 2.0-3.7 4He \n",QcritHe);
                fprintf(Qfi,"F1   0     1      Q flag (0=Fe free, 1=Q)\n");
                fprintf(Qfi,"F1   0    20      Low Fe Val\n");
                fprintf(Qfi,"#End\n");
                fclose(Qfi);
                Qfi=0;
        }

        open_tbl();     /* RE table for PH analysis */

        time_average();
        for (i=0;  i<3;  i++)
        {
            fprintf(Ofi,"0  %3.0lf  %5.1lf  %5.1lf  %5.0lf  %1d\n",
                sumi,sumd[i]/sumi,sume[i]/sumi,sumn[i],i);
        }

        fclose(Ofi);
        to_dos();
}

/************************* init_file() **************************************/
/*   Call first_time based on directory/nondirectory file type              */
/****************************************************************************/

init_file(void)
{
       if (dir_mode[0])                  /* directory file */
         first_time();
       else                                  /* non-directory files */
         first_time_ptfiles();

}

/*************************** select_type ************************************/
/*  Call appropiate function depending on file type                         */
/****************************************************************************/

select_type(void)
  {
   if (dir_mode[0])       /* directory file */
      if (!next_time())
         return 0;
      else
         return 1;
   else                      /* non-directory files */
      if (!next_time_ptfiles())
         return 0;
      else
         return 1;
  }

/*****************************************************************************/
/*****************************************************************************/

file_close(FILE *fistart)
{
        int i;
        FILE *fiptr;

        fprintf(Efi,"\nClose:\n");
                for (i=0; i<NAVG; i++)
                    if (*Fna[i] && Ffi[i])
                    {
                       fprintf(Efi,"  %20.20s  %8X\n",Fna[i], Ffi[i]);
                       fclose(Ffi[i]);
                       Ffi[i]=0;
                    }
                for (i=0; i<NAVG; i++)
                    if (*Rna[i] && Rfi[i])
                    {
                       fprintf(Efi,"  %20.20s  %8X\n",Rna[i], Rfi[i]);
                       fclose(Rfi[i]);
                       Rfi[i]=0;
                    }
                for (i=0; i<NAVG; i++)
                    if (*Sna[i] && Sfi[i])
                    {
                       fprintf(Efi,"  %20.20s  %8X\n",Sna[i], Sfi[i]);
                       fclose(Sfi[i]);
                       Sfi[i]=0;
                    }
                if (*Phna && Phfi)
                {
                        fprintf(Efi,"Phfi=%8X  %20.20s\n",Phfi,Phna);
                        fclose(Phfi);
                        Phfi=0;
                }
                for (i=0; i<3; i++)
                    if (*Dna[i] && Dirfi[i])
                    {
                       fprintf(Efi,"  %20.20s  %8X\n",Dna[i], Dirfi[i]);
                       fclose(Dirfi[i]);
                       Dirfi[i]=0;
                    }
                if (Qfi)
                {
                        fprintf(Efi,"  %20.20  %8x\n",Qna,Qfi);
                        fclose(Qfi);
                        Qfi=0;
                }
                fprintf(Efi,"fdir=%8X  %20.20s\n",fdir[0],F_name[0]);
                fprintf(Efi,"F_in=%8X  %20.20s\n",F_in[0],fnp[0]);
        if (fistart)
            fiptr=fistart;
        else
            fiptr=Efi;

        for (i=0; i<35; i++)
        {
                fprintf(Efi," cnt=%2d, fd=%2d %20.20s %40.40s\n",
                    (int) fiptr->_cnt,(int) fiptr->_fd,(char *) fiptr->_temp_name
                    ,fiptr->_ptr);
                if (!fiptr->_next_stream)
                    break;
                fiptr=fiptr->_next_stream;
        }
        fflush(Efi);
        printf("\nAll %2d files closed\n",i=fcloseall());
        if ((Ofi=fopen(Ona,"a"))==0)
        {
                puts("Cannot open file ");
                puts(Ona);
                exit(1);
        }
        if ((Efi=fopen(Ena,"a"))==0)
        {
                puts("Cannot open file ");
                puts(Ena);
                exit(1);
        }
        fprintf(Efi,"\nAll %2d files closed\n",i);
}


extern short kvers;
extern short next_off, ax;

auto_file()
{
        int c;
        int i;
        int nfree;

        if (TEMPAVG && Avg[6].Tint[0])  /* Data present, write buffer */
        {
/*
                if ((Avgfi=fopen("c:\\wind\\temp.avg","wb"))==0)
                {
                        puts("Cannot open file c:\\wind\\temp.avg");
                        exit(1);
                }
                puts("temp.avg buffer written\n");
                fwrite(Avg,sizeof(Avg),1,Avgfi);
                fclose(Avgfi);
                Avgfi=0;
*/
        }
        else if (TEMPAVG)  /* No data present, read buffer */
        {
/*
                if ((Avgfi=fopen("c:\\wind\\temp.avg","rb"))==0)
                {
                        puts("Cannot open file c:\\wind\\temp.avg - No buffer read.\n");
                }
                else
                {
                        fread(Avg,sizeof(Avg),1,Avgfi);
                        fclose(Avgfi);
                        Avgfi=0;
                        puts("temp.avg buffer read\n");
                }
*/

        }
        if ( Auto>=INFILES)
                return 0;
# if 1
        if(fdir[0])
        {
            fclose(fdir[0]);
            *Dirname[0]=0;
            if(F_in[0])
               fclose(F_in[0]);
            if(F_spin[0])
               fclose(F_spin[0]);
            if (fin[0])
                fclose(fin[0]);
            if (fdirf[0])
                fclose(fdirf[0]);
            fdir[0]=0;
            F_in[0]=0;
            F_spin[0]=0;
            fin[0]=0;
            fdirf[0]=0;
        }
#endif
        file_close(F_in[0]);
        for (i=0; i<Ncols+3; i++)
        {
                if (Ax_labl[i])
                {
                        free(Ax_labl[i]);
                        Ax_labl[i]=0;
                }
        }
        nfree=_freect(1024);
        printf("\nAuto=%d  free=%d K ",Auto,nfree);
        if (!Fullauto)
            printf("\nAuto File, Mount disk for %s (+=bypass, Esc=quit, enter=go)\n"
                 ,Ency[Auto]);
        while(1)
        {
                if (Auto>=INFILES)
                        return 0;
                if (Fullauto)
                        c='\r';
                else if ((c=key())==0x1b)      /* ESC */
                {
                    Auto=INFILES;
                    return 0;
                }
                if ( (c)=='\r' )   /* CR */
                {
                    F_name[0]=Ency[Auto];
                    Nvers=Ncols=Dvers=0;
                    kvers=next_off=ax=0;
                    if(fdir[0])
                    {
                        fclose(fdir[0]);
                        *Dirname[0]=0;
                        if(F_in[0])
                           fclose(F_in[0]);
                        if(F_spin[0])
                           fclose(F_spin[0]);
                        if (fin[0])
                            fclose(fin[0]);
                        if (fdirf[0])
                            fclose(fdirf[0]);
                        fdir[0]=0;
                        F_in[0]=0;
                        F_spin[0]=0;
                        fin[0]=0;
                        fdirf[0]=0;
                    }
                /*  _strtime(tim);                    current time
                    printf("\nTime= %s\n",tim);       */
                    new_file(0);            /* Read header and init PHA file */
                    Auto++;
                    break;
                }
                else if (c=='+')
                {
                    Auto++;
                    if (Auto>=INFILES)
                        return 0;
                if (!Fullauto)
        printf("\nAuto File, Mount disk for %s (+=bypass, Esc=quit, enter=go)\n"
                ,Ency[Auto]);
                }
        }
        return 1;
}

unsigned short *swbin;

time_average(void)
 {
        unsigned short *jloc, *cloc;
        FILE *infi;
        int i, error, k;
        short verse, v1, ntels, byte_ax;
        long time, *tm;
        float *rate;
        char buffer[80];
        char ch;
        short dome;
        double  x, nproc, nbinned;
        unsigned char *ptr;
        short epact_instr=0;
        long majfr, onoff, majsave=0;
        int badape;
        double temp;
        struct UNCOMP_PH *lmtph;
        char *run_vloc;
        long byte_cnt;
        unsigned char *magsecptr[5];
        int good;

        do   /* File loop */
        {
            if ( pan[cw].Ax_lim[0].l[1]<=sscantime(Starttime, &ptr))
                continue;
            if ( pan[cw].Ax_lim[0].l[0]>=sscantime(Endtime, &ptr))
                continue;
            if ( pan[cw].Ax_lim[0].l[0]<=sscantime(Starttime, &ptr))
                 pan[cw].Ax_lim[0].l[0]=sscantime(Starttime, &ptr);
            if ( pan[cw].Ax_lim[0].l[1]>=sscantime(Endtime, &ptr))
                 pan[cw].Ax_lim[0].l[1]=sscantime(Endtime, &ptr);
            _strtime(tim);                   /* current time */
            init_file();
            printf("\nTime= %s\n",tim);
            pan[cw].Dout=1;  /* 3 for sec */
            time=pan[cw].Ax_lim[0].l[0];
            sprintime(time,cbuf);
            printf("Begin: %s\n",cbuf);
            time=pan[cw].Ax_lim[0].l[1];
            sprintime(time,cbuf);
            printf("  End: %s\n",cbuf);
            count=0;


            do
            {
                    if (!read_all())      /* All verses (each time) */
                            break;
                    time=*(tm= (long *) get_col(0));
//          sprintime(time,cbuf);
//          printf("     : %s\n",cbuf);
                    if (time < pan[cw].Ax_lim[0].l[0])
                        continue;
                    if (time > pan[cw].Ax_lim[0].l[1])
                        break;
/*
                    sprint_val(0,buffer,2);
                    printf("%s  \n",buffer);
                    printf("AvgInt=%d\n",AvgInt);
*/
                    if (time/AvgInt != TimeBin[0])
                    {
                        NewTimeBin(time);
                        count=0;
                        errors=0;
                        if (key()==0x1b)      /* ESC */
                        {
                            Auto=INFILES;
                            break;
                        }
                    }
                    nproc=0.;
                    error=0;
                    for (verse=0; verse<Nvers; verse++)    /* Each verse */
                    {
/*                      printf("verse loop: %2d %4x %4x\n",
                            verse,V_num[verse],V_loc[verse]);          */
                        if (V_num[verse]==0x2400)
                        {
                            if (!V_loc[verse])
                                    break;
                            Sptr=V_loc[verse];
                            onoff=Sptr[29];
                            majfr=*((unsigned short *)(Sptr+115));
                            majfr+=Sptr[102]*65536L;
                          /* printf("on/off=%2x  majfr=%d\n",
                                    onoff, majfr);
                          */
                            if (onoff&3 != 3 )
                            {
                                    majsave=majfr;
                                    break;
                            }
                            if ( majfr<majsave+17)
                                    break;
                            Spin[0]=Sptr[91];
                            Spin[1]=Sptr[54];
                            Spin[2]=Sptr[55];
/*                            printf("Spins= %d %d %d\n",Spin[0], Spin[1], Spin[2]); */
                        }
                        else if (V_num[verse]==0x2500)  /* Tel A */
                        {
                            if (!V_loc[verse])
                                    break;
                            jloc=V_loc[verse]+18;
                            Nprot[0]=x=unpack_dbl(jloc[0]);
                            nproc+=x;
                            Nhe[0]=x=unpack_dbl(jloc[1]);
                            nproc+=x;
                            Nhvy[0]=x=unpack_dbl(jloc[2]);
                            nproc+=x;
                            jloc=V_loc[verse]+30;
/*
                            for (i=14; i>=0; i--)
                                    printf(" Win%2d= %4x %8.0lf\n",15-i,jloc[i], unpack_dbl(jloc[i]));
*/
                            for (i=0; i<16; i++)
                            {
                                    win[0][i]=unpack_dbl(jloc[15-i]);
                            }
                            i=0;
                            Win1_5[i]=unpack_dbl(jloc[14]);  /* Win_A 1 */
                            Win1_5[i]+=unpack_dbl(jloc[13]);
                            Win1_5[i]+=unpack_dbl(jloc[12]);
                            Win1_5[i]+=unpack_dbl(jloc[11]);
                            Win1_5[i]+=unpack_dbl(jloc[10]);
                            Win6_12[i]=unpack_dbl(jloc[9]);
                            Win6_12[i]+=unpack_dbl(jloc[8]);
                            Win6_12[i]+=unpack_dbl(jloc[7]);
                            Win6_12[i]+=unpack_dbl(jloc[6]);
                            Win6_12[i]+=unpack_dbl(jloc[5]);
                            Win6_12[i]+=unpack_dbl(jloc[4]);
                            Win6_12[i]+=unpack_dbl(jloc[3]);
                            Win13_15[i]=unpack_dbl(jloc[2]);
                            Win13_15[i]+=unpack_dbl(jloc[1]);
                            Win13_15[i]+=unpack_dbl(jloc[0]); /* Win_A 15 */
                            jloc=V_loc[verse]+78;
                            for (i=0; i<8; i++)
                            {
                                    hwrate[0][i]=unpack_dbl(jloc[i]);
                            }
#if 0
                              printf("\nA HW  %8.0f  %8.0f %8.0f\n",
                                Win1_5[0], Win6_12[0], Win13_15[0]);
                          /*      win[0][13], win[0][14], win[0][15]);  */
#endif
                        }
                        else if (V_num[verse]==0x2501)  /* Tel B */
                        {
                            if (!V_loc[verse])
                                    break;
                            jloc=V_loc[verse]+18;
                            Nprot[1]=x=unpack_dbl(jloc[0]);
                            nproc+=x;
                            Nhe[1]=x=unpack_dbl(jloc[1]);
                            nproc+=x;
                            Nhvy[1]=x=unpack_dbl(jloc[2]);
                            nproc+=x;
                            jloc=V_loc[verse]+30;
                            for (i=0; i<16; i++)
                            {
                                    win[1][i]=unpack_dbl(jloc[15-i]);
                            }
                            i=1;
                            Win1_5[i]=unpack_dbl(jloc[14]);  /* Win_A 1 */
                            Win1_5[i]+=unpack_dbl(jloc[13]);
                            Win1_5[i]+=unpack_dbl(jloc[12]);
                            Win1_5[i]+=unpack_dbl(jloc[11]);
                            Win1_5[i]+=unpack_dbl(jloc[10]);
                            Win6_12[i]=unpack_dbl(jloc[9]);
                            Win6_12[i]+=unpack_dbl(jloc[8]);
                            Win6_12[i]+=unpack_dbl(jloc[7]);
                            Win6_12[i]+=unpack_dbl(jloc[6]);
                            Win6_12[i]+=unpack_dbl(jloc[5]);
                            Win6_12[i]+=unpack_dbl(jloc[4]);
                            Win6_12[i]+=unpack_dbl(jloc[3]);
                            Win13_15[i]=unpack_dbl(jloc[2]);
                            Win13_15[i]+=unpack_dbl(jloc[1]);
                            Win13_15[i]+=unpack_dbl(jloc[0]); /* Win_A 15 */
                            jloc=V_loc[verse]+78;
                            for (i=0; i<8; i++)
                            {
                                    hwrate[1][i]=unpack_dbl(jloc[i]);
                            }
                        }
                        else if (V_num[verse]==0x2502)  /* Tel C */
                        {
                            if (!V_loc[verse])
                                    break;
                            jloc=V_loc[verse]+18;
                            Nprot[2]=x=unpack_dbl(jloc[0]);
                            nproc+=x;
                            Nhe[2]=x=unpack_dbl(jloc[1]);
                            nproc+=x;
                            Nhvy[2]=x=unpack_dbl(jloc[2]);
                            nproc+=x;
                            jloc=V_loc[verse]+30;
                            for (i=0; i<16; i++)
                            {
                                    win[2][i]=unpack_dbl(jloc[15-i]);
                            }
                            i=2;
                            Win1_5[i]=unpack_dbl(jloc[14]);  /* Win_A 1 */
                            Win1_5[i]+=unpack_dbl(jloc[13]);
                            Win1_5[i]+=unpack_dbl(jloc[12]);
                            Win1_5[i]+=unpack_dbl(jloc[11]);
                            Win1_5[i]+=unpack_dbl(jloc[10]);
                            Win6_12[i]=unpack_dbl(jloc[9]);
                            Win6_12[i]+=unpack_dbl(jloc[8]);
                            Win6_12[i]+=unpack_dbl(jloc[7]);
                            Win6_12[i]+=unpack_dbl(jloc[6]);
                            Win6_12[i]+=unpack_dbl(jloc[5]);
                            Win6_12[i]+=unpack_dbl(jloc[4]);
                            Win6_12[i]+=unpack_dbl(jloc[3]);
                            Win13_15[i]=unpack_dbl(jloc[2]);
                            Win13_15[i]+=unpack_dbl(jloc[1]);
                            Win13_15[i]+=unpack_dbl(jloc[0]); /* Win_A 15 */
                            jloc=V_loc[verse]+78;
                            for (i=0; i<8; i++)
                            {
                                    hwrate[2][i]=unpack_dbl(jloc[i]);
                            }
                        }
                        else if (V_num[verse]==0x2800)  /* A only SW */
                        {
                            if (!V_loc[verse])
                                    continue;
                            byte_ax=V_col[verse];
                            swbin=(short *) get_col(byte_ax+2);
                            for (i=0; i<150; i++)
                            {
                                    x=unpack_dbl(swbin[i]);
                                    Avg[0].ASWbox[i]+=x;
                            }
                        }
                        else if (V_num[verse]==0x2801)  /* B only SW */
                        {
                            if (!V_loc[verse])
                                    continue;
                            byte_ax=V_col[verse];
                            swbin=(short *) get_col(byte_ax+2);
                            for (i=0; i<150; i++)
                            {
                                    x=unpack_dbl(swbin[i]);
                                    Avg[0].BSWbox[i]+=x;
                            }
                        }
                        else if (V_num[verse]==0x2802)  /* C only SW */
                        {
                            if (!V_loc[verse])
                                    continue;
                            byte_ax=V_col[verse];
                            swbin=(short *) get_col(byte_ax+2);
                            for (i=0; i<150; i++)
                            {
                                    x=unpack_dbl(swbin[i]);
                                    Avg[0].CSWbox[i]+=x;
                            }
                        }
                        else if (V_num[verse]==0x2a00)  /* PHA   */
                        {
                            int nsample, tel;
                            if (!V_loc[verse])
                                    break;
                            pan[cw].Verse=verse;
                            run_vloc = V_loc[pan[cw].Verse];
                            jloc = (short *)V_loc[verse];
                            byte_cnt=  jloc[1];
                            nsample=0;
                            /*       printf("\n  Dp    Ep   Df  Ef  Da  Ex  Ey Exp Tel ");
                                     printf(" WA  WB   Spn   SWB \n");
                            */
                            if (Chargefile)  /* lmtchg.out */
                            {
                                if ((Chfi=fopen(Chna,"ab"))==0)
                                {
                                        puts("Cannot open file for append ");
                                        puts(Chna);
                                        exit(1);
                                }
                            }
                            for (i=0; i<50; i++) /* Read individual pulse heights */
                            {
                                 cloc= (unsigned short *) (V_loc[verse] = run_vloc+10*i);
                                 expnd(V_num[verse]);
                                 lmtph=(struct UNCOMP_PH *)V_loc[verse];
                                 jloc = (short *)V_loc[verse];
                                 jloc += 2;
                                 if (Avg[0].Tint[0]>Striptime[0]
                                         && lmtph->Tel_num==2
                                         && lmtph->E_y==2 )
                                 {
                                     byte_cnt-=10;
                                     continue;
                                 }
                                 if (jloc[0] )
                                 {
                                     buffer[0]=0;      /* Charge file -Verse 0 */
                                     buffer[1]=0;
                                     memcpy(buffer+2,tm,8);      /* time  */
                                     memcpy(buffer+10,jloc+21,8);   /* corr. pulse heights  */
                                     memcpy(buffer+18,jloc+12,2);   /* SW bin  */
                                     good=find_z(3,lmtph->Win_A,0.,(double)lmtph->D_corr,
                                          (double)lmtph->E_corr,lmtph->SW_Bin,buffer);
                                 }
                                 if (!good)
                                 {
                                     byte_cnt-=10;
                                     continue;
                                 }
                                 nsample++;
                                 if (cloc[2] && Ipha<NPHB+5)
                                 {
                                 /* Subtract offsets only if output file
                                    will have no verse 25  */
                                     tel=lmtph->Tel_num;
                                     Phabuf[Ipha++]=lmtph->Dp+doffset[tel];
                                     Phabuf[Ipha++]=lmtph->Ep+eoffset[tel];
                                     Phabuf[Ipha++]=cloc[4];
                                     Phabuf[Ipha++]=cloc[5];
                                     Phabuf[Ipha++]=cloc[6];
                                     Npha++;
                                 }
                                 if (lmtph->SW_Bin>83 && lmtph->SW_Bin<89)
                                 {   /*
                                     printf("\n  Dp    Ep   Df  Ef  Da  Ex  Ey Exp Tel ");
                                     printf(" WA  WB   Spn   SWB \n");
                                     if (jloc[0])
                                     printf("%5u %5u %3u %3u %3u %3u %3u %3u %3u %3u %3u %5u %5u \n",
                                             jloc[0],jloc[1],jloc[2],jloc[3],jloc[4],jloc[5],
                                             jloc[6],jloc[7],jloc[8],jloc[9],jloc[10],jloc[11],jloc[12]);
                                     */

                                     Avg[0].Noxy[lmtph->Tel_num]++;
                                 }
                                 byte_cnt-=10;
                            }
                            if (Chargefile)
                                 fclose(Chfi);
/*                          printf("  nsample=%d Nox=%3.0f %3.0f %3.0f %u\n",
                                nsample,Avg[0].Noxy[0],Avg[0].Noxy[1],Avg[0].Noxy[2],jloc); */
                        }
                        if (V_num[verse]!=0x2600)
                                continue;  /* Skip if not swrate verse */
                        if (!V_loc[verse])
                                break;
                        pan[cw].Verse = verse;
                        byte_ax=V_col[verse];
                        ntels= *get_col(byte_ax+1);
                        swbin=(short *) get_col(byte_ax+2);
                        nbinned=0.;
                        for (i=0; i<150; i++)
                        {
                            x=unpack_dbl(swbin[i]);
                            nbinned+=x;
                            if (x<0.)
                                    break;
                            if (x>20000.)
                                    continue;
                        }

/*                        sprintime(time,buffer);
                        printf("%s      \r",buffer);   */
                        for (i=0; i<3; i++)
                        {
                            if (Spin[i]<3 || Spin[i]>65)
                                    error=1;
                            if (!compare(Win6_12))
                                    error=1;
                            if (!compare(Win13_15))
                                    error=1;
                        }
                        if (x<0. || fabs(nbinned-nproc)>.005*(nbinned+nproc)+2)  /*  Error */
                                error=1;
                        if (error)
                        {
                            errors++;
                            sprintime(time,buffer);
                            fprintf(Efi,"Error %s  nbinned=%8.0lf nproc=%8.0lf\n",
                                   buffer,nbinned, nproc);
/*
                              fprintf(Efi,"Spins= %d %d %d\n",Spin[0], Spin[1], Spin[2]);
                              fprintf(Efi,"A HW  %8.0lf  %8.0lf %8.0lf\n",
                                Win1_5[0], Win6_12[0], Win13_15[0]);
                              fprintf(Efi,"B HW  %8.0lf  %8.0lf %8.0lf\n",
                                Win1_5[1], Win6_12[1], Win13_15[1]);
                              fprintf(Efi,"C HW  %8.0lf  %8.0lf %8.0lf\n",
                                Win1_5[2], Win6_12[2], Win13_15[2]);
                              fprintf(Efi,"A SW  %8.0lf  %8.0lf %8.0lf\n",
                                Nprot[0], Nhe[0], Nhvy[0]);
                              fprintf(Efi,"B SW  %8.0lf  %8.0lf %8.0lf\n",
                                Nprot[1], Nhe[1], Nhvy[1]);
                              fprintf(Efi,"C SW  %8.0lf  %8.0lf %8.0lf\n",
                                Nprot[2], Nhe[2], Nhvy[2]);
                            fprintf(Efi,"verse[%d]=%x, V_loc=%lx, V_floc=%lx\n",
                                    verse-1,V_num[verse-1],V_loc[verse-1],V_floc[verse-1]);
                            fprintf(Efi,"verse[%d]=%x, V_loc=%lx, V_floc=%lx\n   0:",
                                    verse,V_num[verse],V_loc[verse],V_floc[verse]);
                            ptr=V_loc[verse];
                            for (i=0; i<160; i++)
                            {
                                    fprintf(Efi," %2x",(int)(*ptr++));
                                    if ((i%16)==15)
                                            fprintf(Efi,"\n%3d: ",i+1);
                            }
                            fprintf(Efi,"\n");
*/
                            break;
                        }
            /*  Valid data, contribute to sums      */
                        if (!Firstt)
                        {
                               Firstt=time;
                               Avg[0].Tint[0]=time;
                               Avg[0].Maj[0]=majfr;
                        }
                        Avg[0].Tint[1]=Lastt=* (( (long *) get_col(0))+1);
                        Avg[0].Maj[1]=majfr;
                        for (i=0; i<150; i++)
                        {
                                x=unpack_dbl(swbin[i]);
                                Avg[0].SWbox[i]+=x;
                                Avg[0].Allbox+=x;
                        }

                        magsecptr[0]=(unsigned char *) (swbin+150);
                        magsecptr[1]=magsecptr[0]+51;
                        magsecptr[2]=magsecptr[1]+17;
                        magsecptr[3]=magsecptr[2]+17;
                        magsecptr[4]=magsecptr[3]+17;
                        for (i=0; i<5; i++)
                        {
                                long sect, pow;
                                pow=*magsecptr[i];
                                for (k=1; k<17; k++)
                                {
                                        sect=magsecptr[i][k];
                                        Avg[0].Magsec[i][k-1]+=(sect<<pow);
                                }
                        }

                        for (i=0; i<3; i++)
                        {
                                Avg[0].Sum1_5+=Win1_5[i];
                                Avg[0].Sum6_12+=Win6_12[i];
                                Avg[0].Sum13_15+=Win13_15[i];
                                Avg[0].Sprot+=Nprot[i];
                                Avg[0].She+=Nhe[i];
                                Avg[0].Shvy+=Nhvy[i];
                                Avg[0].Nprots[i]+=Nprot[i];
                                Avg[0].Nhes[i]+=Nhe[i];
                                Avg[0].Nhvys[i]+=Nhvy[i];
                                /* Correct July 14, 2000 event */
                                if (Spin[i] > 35)
                                {
                                    Avg[0].Spins+=Spin[i]*.833333;
                                    Avg[0].Spind[i]+=Spin[i]*.833333;
                                }
                                else
                                {
                                    Avg[0].Spins+=Spin[i];
                                    Avg[0].Spind[i]+=Spin[i];
                                }
                        }

                        for (i=0; i<8; i++)
                        {
                                Avg[0].HWrate[0][i]+=hwrate[0][i];
                                Avg[0].HWrate[1][i]+=hwrate[1][i];
                                Avg[0].HWrate[2][i]+=hwrate[2][i];
                        }
                        for (i=0; i<16; i++)
                        {
                                Avg[0].WinA[0][i]+=win[0][i];
                                Avg[0].WinA[1][i]+=win[1][i];
                                Avg[0].WinA[2][i]+=win[2][i];
                        }
                        dome=majfr&15;
                        Avg[0].DomeThr[0][dome]+=hwrate[0][3];
                        Avg[0].DomeThr[1][dome]+=hwrate[1][3];
                        Avg[0].DomeThr[2][dome]+=hwrate[2][3];
                    /* Get APE data */
                        for (v1=0; v1<Nvers; v1++)    /* Each verse */
                        {
                            if (V_num[v1]==0x2401)
                            {
                                if (!V_loc[v1])
                                        break;
                                ptr=V_loc[v1]+72;
                                if (ptr[0]<3 || ptr[0]>65) /* Spin error */
                                        break;
                                if (ptr[6]<3 || ptr[6]>65)
                                        break;
                            }
                            if (V_num[v1]==0xA500)
                            {
                                if (!V_loc[v1])
                                        break;
                                ApeASW=V_loc[v1]+4;
                                badape=0;
                            /*  if (time>AEnd)
                                    continue;   */
                                for (i=0; i<NABINS; i++)
                                {
                                        temp=unpack_dbl(ApeASW[i]);
                                        if (temp<0. ||temp >2.e9)
                                                badape=1;
                                }
                                if (badape)
                                        break;
                                for (i=0; i<NABINS; i++)
                                {
                                        temp=unpack_dbl(ApeASW[i]);
                                        if (i>42 && temp> 1.e4)  /* patch */
                                            continue;
                                        Avg[0].ApeAbox[i]+=temp;
                                }
                        /*      printf("Sum=%6ld Unp=%6.0lf ", Avg[0].ApeAbox[1], unpack_dbl(ApeASW[1]) );      */
                            }
                            if (V_num[v1]==0xA501)
                            {
                                if (!V_loc[v1])
                                        break;
                                ApeBSW=V_loc[v1]+4;
                                badape=0;
                           /*   if (time>BEnd)
                                    continue;     */
                                for (i=0; i<NBBINS; i++)
                                {
                                        temp=unpack_dbl(ApeBSW[i]);
                                        if (temp<0. ||temp >2.e9)
                                                badape=1;
                                }
                                if (badape)
                                        break;
                                for (i=0; i<NBBINS; i++)
                                {
                                        Avg[0].ApeBbox[i]+=unpack_dbl(ApeBSW[i]);
                                }
                          /*    printf(" B");   */
                            }
                            if (V_num[v1]==0xA700)
                            {
                                if (!V_loc[v1])
                                        continue;
                                jloc=V_loc[v1]+4;
                           /*   if (time>AEnd)
                                    continue;   */
                                Avg[0].ApeFrames[0]++;
                                /* Correct July 14, 2000 event */
                                if (*ptr > 35)
                                    Avg[0].ApeSpins[0]+=(*ptr)*.833333;
                                else
                                    Avg[0].ApeSpins[0]+=*ptr;
                          /*    printf(" Fr=%6.0hf Sp=%6.0hf sp=%3hd\n",
                                        Avg[0].ApeFrames[0],Avg[0].ApeSpins[0],*ptr); */
                                for (i=0; i<13; i++)
                                {
                                        Avg[0].HWApeA[i]+=unpack_dbl(jloc[i]);
                                }
                            }
                            if (V_num[v1]==0xA701)
                            {
                                if (!V_loc[v1])
                                        continue;
                                jloc=V_loc[v1]+4;
                            /*  if (time>BEnd)
                                    continue;  */
                                Avg[0].ApeFrames[1]++;
                                /* Correct July 14, 2000 event */
                                if (ptr[6] > 35)
                                    Avg[0].ApeSpins[1]+=ptr[6]*.833333;
                                else
                                    Avg[0].ApeSpins[1]+=ptr[6];
                                for (i=0; i<12; i++)
                                {
                                        Avg[0].HWApeB[i]+=unpack_dbl(jloc[i]);
                                }
                            }
                            if (V_num[v1]==0xA702)
                            {
                                if (!V_loc[v1])
                                        continue;
                                jloc=V_loc[v1]+4;
                                Avg[0].ApeFrames[2]++;
                                /* Correct July 14, 2000 event */
                                if (ptr[16] > 35)
                                    Avg[0].ApeSpins[2]+=ptr[16]*.833333;
                                else
                                    Avg[0].ApeSpins[2]+=ptr[16];
                                for (i=0; i<32; i++)
                                {
                                        Avg[0].HWIT[i]+=unpack_dbl(jloc[i]);
                                }
                            }
                        }
                        count++;
                    }                     /* end (verse) for loop */
/*                  if (count++==20)
                            break;
*/
            }                                 /* end (time) do while loop */
            while (select_type());
        }
        while (auto_file());     /* File mounting loop */
        Close=1;
        NewTimeBin(time);
}


/**************************************************************************/
/* Initializes parameters necessary for panel structure                   */
/**************************************************************************/

init_params()
 {
short   i;
      i = 0;
      pan[i].Ishist =  0;
      pan[i].off[0] =  0;
      pan[i].off[1] =  1;
      pan[i].off[2] =  2;
      pan[i].List_off =  0;
      pan[i].Z_select = 2;
      pan[i].Z_off_color = 64;
      pan[i].Zbox = 4;
      pan[i].Err_bar = 1;
      pan[i].Join_en =  0;
      pan[i].Corr_en =  0;
      pan[i].Dout = 1;
      pan[i].Check_all = 1;
      Box_size[0] = 1500.0;
      Box_size[1] = 1275.0;
      pan[i].Dzoom = 4;
      pan[i].Dpow = 2;
      pan[i].Setc_x = 512;
      pan[i].Setc_y = 325;
      Binlist = 0;
      Portrait = 1;
      pan[i].T_mode = 0;
      pan[i].Toler = 300;
      pan[i].T_step = .9;
      pan[i].T_scan =  0;
      pan[i].T_lag = .1;
      Fontn =  1;
      pan[i].k_page =  0;
      pan[i].k_del = 0;
      pan[i].ed_col =  0;
      pan[i].ed_row = 0;
      pan[i].maincurs = 0;
      pan[i].Phas =  0;
      pan[i].Nodes =  0;
      pan[i].Maxn = 1;
      pan[i].display_status = 1;
      pan[i].write_labels = 2;
      pan[i].X_label = 2;
      pan[i].Y_label = 2;
      pan[i].Z_label = 2;
      pan[i].zax_step = 2;
      pan[i].Dif_d[0] = 0;
      pan[i].Dif_d[1] = 0;
      pan[i].Dif_d[2] = 0;
      pan[i].sav_po[0] = 0;
      pan[i].sav_po[1] = 0;
      pan[i].sav_po[2] = 0;
      pan[i].delete_count = 0;
      pan[i].T_e = 0;
      pan[i].Verse = 1;
      for(i=0; i< MFILE; i++)
         File_mode[i] = 0;
      i = 0;
     strcpy(Ex,"Histogram");
      Mixed_mode = 0;

}


key(void)                            /* Read keyboard with immediate return.
                                     - Remap keys preceeded by null byte.
                                     - Return -1 if no key pressed.
                                     - Returns (& clears) pushed key,  if any.
                                     */
{
        short c;

        if ((c = Push_key)!= -1)
                Push_key = -1;
        else if (!_bios_keybrd(_KEYBRD_READY))
                return (-1);
        else if ((c = chi()) == 0)
                c = 0x100|chi();
        return(c);
}


/***************************************************************************/
/***************************************************************************/

chi(void)                            /* Do DOS int 21 for keyboard char */
{
        short c;
        inregs.h.dl = 0xff;
        inregs.x.ax = 0x0600;
        c = intdos(&inregs, &outregs);
        return (c&0x00ff);
}

to_dos()                                      /* Exit to DOS    */
{
        exit(1);
}


/* Defined for the benefit of other modules */
mmin(x,y)
short x, y;
{
        int z;
        z=x<y?x:y;
        return z;
}

mmax(x,y)
short x, y;
{
        int z;
        z=x>y?x:y;
        return z;
}
indx(i,list,n)  /* Index j of i<=list[j] */
short i, list[], n;
{
        int j;
        for (j=0; j<n && i>list[j]; j++)
                ;
        return(j);
}
calc_alg()
{
}
scan_alg()
{
}
calc_int()
{
}
lg_plt()
{
}
set_ax()
{
}
set_one()
{
}
OutFlxHdr(int k)
{
        double hrs;
        int i, kp, j;

        if (!Ffi[k])
                return;
        fprintf(Ffi[k],"Binary\r\n");
        hrs=AvgInt*Avgcnt[k]/3600.;
        if (hrs < 24.)
            fprintf(Ffi[k]," %5.2lf-hr Avg LEMT rates and fluxes\r\n",hrs);
        else
            fprintf(Ffi[k]," %5.2lf-day Avg LEMT rates and fluxes\r\n",hrs/24.);

//      sprintime(pan[cw].Ax_lim[0].l[0],Tbuf);
        fprintf(Ffi[k]," %s\r\n",Starttime);
//      sprintime(pan[cw].Ax_lim[0].l[1],Tbuf);
        fprintf(Ffi[k]," %s\r\n",Endtime);
        sprintime((long)(AvgInt*Avgcnt[k]+.5),Tbuf);
        fprintf(Ffi[k]," 0  %s\r\n",Tbuf+9);
        fprintf(Ffi[k],"#0  Rate\r\n");
        fprintf(Ffi[k],"T2             Time\r\n");

        if (Rfi[k])
        {
            fprintf(Rfi[k],"Binary\r\n");
            hrs=AvgInt*Avgcnt[k]/3600.;
            if (hrs < 24.)
                fprintf(Rfi[k]," %5.2lf-hr Avg rates and fluxes\r\n",hrs);
            else
                fprintf(Rfi[k]," %5.2lf-day Avg rates and fluxes\r\n",hrs/24.);

            sprintime(pan[cw].Ax_lim[0].l[0],Tbuf);
            fprintf(Rfi[k]," %s\r\n",Tbuf);
            sprintime(pan[cw].Ax_lim[0].l[1],Tbuf);
            fprintf(Rfi[k]," %s\r\n",Tbuf);
            sprintime((long)(AvgInt*Avgcnt[k]+.5),Tbuf);
            fprintf(Rfi[k]," 0  %s\r\n",Tbuf+9);
            fprintf(Rfi[k],"#0  Rate\r\n");
            fprintf(Rfi[k],"T2             Time\r\n");
            fprintf(Rfi[k],"#1  Rate\r\n");
            fprintf(Rfi[k],"F8    0.01   1.e6      HWrate A (c/s)\r\n");
            fprintf(Rfi[k],"F8    0.01   1.e6      HWrate B (c/s)\r\n");
            fprintf(Rfi[k],"F8    0.01   1.e6      HWrate C (c/s)\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nspins A\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nspins B\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nspins C\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nprot proc A (c/s)\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nprot proc B (c/s)\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nprot proc C (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Nhe   proc A (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Nhe   proc B (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Nhe   proc C (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Nhvy  proc A (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Nhvy  proc B (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Nhvy  proc C (c/s)\r\n");
            fprintf(Rfi[k],"F16   0.01   1.e6      Dome Thr Rate A (c/s)\r\n");
            fprintf(Rfi[k],"F16   0.01   1.e6      Dome Thr Rate B (c/s)\r\n");
            fprintf(Rfi[k],"F16   0.01   1.e6      Dome Thr Rate C (c/s)\r\n");
            fprintf(Rfi[k],"F16   0.01   1.e6      WinA Rate A (c/s)\r\n");
            fprintf(Rfi[k],"F16   0.01   1.e6      WinA Rate B (c/s)\r\n");
            fprintf(Rfi[k],"F16   0.01   1.e6      WinA Rate C (c/s)\r\n");
            fprintf(Rfi[k],"L2    0     16777215   Major Frame Nos.\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nspins Ape A\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nspins Ape B\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nspins IT\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nframes Ape A\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nframes Ape B\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Nframes IT\r\n");
            fprintf(Rfi[k],"F13   0.01   1.e6      HWRates Ape A (c/s)\r\n");
            fprintf(Rfi[k],"F12   0.01   1.e6      HWRates Ape B (c/s)\r\n");
            fprintf(Rfi[k],"F32   0.01   1.e6      HWRates IT (c/s)\r\n");
            fprintf(Rfi[k],"L%2d   0   8000000      SWRates Ape A (c)\r\n",NABINS);
            fprintf(Rfi[k],"L%2d   0   8000000      SWRates Ape B (c)\r\n",NBBINS);

            fprintf(Rfi[k],"F1    .001   1.e6      Dome Thr Sum A (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Dome Thr Sum B (c/s)\r\n");
            fprintf(Rfi[k],"F1    .001   1.e6      Dome Thr Sum C (c/s)\r\n");
            fprintf(Rfi[k],"F1    1.e-8  1.        N Ox A (c/s)\r\n");
            fprintf(Rfi[k],"F1    1.e-8  1.        N Ox B (c/s)\r\n");
            fprintf(Rfi[k],"F1    1.e-8  1.        N Ox C (c/s)\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Spins\r\n");
            fprintf(Rfi[k],"F1    0.01   1.e6      Live spins\r\n");
/* ABC SW rates */
            fprintf(Rfi[k],"L150   0   8000000     A SW Rates\r\n");
            fprintf(Rfi[k],"L150   0   8000000     B SW Rates\r\n");
            fprintf(Rfi[k],"L150   0   8000000     C SW Rates\r\n");

            fprintf(Rfi[k],"#2  Flux  Protons, Win_A 1-5\r\n");
            fprintf(Rfi[k],"R2    0.01   1.e6      Win A 1-5 (c/s)\r\n");
            fprintf(Rfi[k],"L1     0   8000000     Nprot proc\r\n");
            fprintf(Rfi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
            fprintf(Rfi[k],";  Geom Scaling for 2.1-2.5 8 is 8/5 before 95 Jun 30\r\n");
            fprintf(Rfi[k],";                                8/33 after 95 Jul  6\r\n");
            fprintf(Rfi[k],"L1       1.75     2.1         6.      Hxxx\r\n");
            fprintf(Rfi[k],"L1       2.1      2.5        .2424    H\r\n");
            fprintf(Rfi[k],"L1       2.5      3.2         8.      Hxxx\r\n");
            fprintf(Rfi[k],"#3  Flux  Protons, Ape B\r\n");
            fprintf(Rfi[k],"R2    0.01   1.e6      Ape B 2D (c/s)\r\n");
            fprintf(Rfi[k],"L1     0   8000000     Sum box\r\n");
            fprintf(Rfi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
            fprintf(Rfi[k],"L1       18.9    21.9       1.3      H\r\n");
            fprintf(Rfi[k],"L1       21.9    24.5       1.3      H\r\n");
            fprintf(Rfi[k],"L1       24.5    27.9       1.3      H\r\n");
            fprintf(Rfi[k],"L1       18.1    22.0       1.3      H\r\n");
            fprintf(Rfi[k],"L1       22.0    24.5       1.3      H\r\n");
            fprintf(Rfi[k],"L1       24.5    27.9       1.3      H\r\n");
            fprintf(Rfi[k],"#End\r\n");
            fclose(Rfi[k]);
            Rfi[k]=0;
        }
        fprintf(Ffi[k],"#2  Flux  Protons, Win_A 1-5\r\n");
        fprintf(Ffi[k],"R2    0.01   1.e6      Win A 1-5 (c/s)\r\n");
        fprintf(Ffi[k],"L1     0   8000000     Nprot proc\r\n");
        fprintf(Ffi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
        fprintf(Ffi[k],";  Geom Scaling for 2.1-2.5 8 is 8/5 before 95 Jun 30\r\n");
        fprintf(Ffi[k],";                                8/33 after 95 Jul  6\r\n");
        fprintf(Ffi[k],"L1       1.75     2.1         6.      Hxxx\r\n");
        fprintf(Ffi[k],"L1       2.1      2.5        .3       H\r\n");
        fprintf(Ffi[k],"L1       2.5      3.2         8.      Hxxx\r\n");
        fprintf(Ffi[k],"L1       1.0     10.0        17.      Bkg0\r\n");
        fprintf(Ffi[k],"L1       2.5      4.0        .3       H frontQ\r\n");
        fprintf(Ffi[k],"L1       2.5      4.0        .3       H backQ\r\n");
        fprintf(Ffi[k],"#3  Flux  Helium , Win_A 6-12\r\n");
        fprintf(Ffi[k],"R2    0.01   1.e6      Win A 6-12 (c/s)\r\n");
        fprintf(Ffi[k],"L1     0   8000000     Nhe proc\r\n");
        fprintf(Ffi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
        fprintf(Ffi[k],"L1       1.75     2.1        17.      He3\r\n");
        fprintf(Ffi[k],"L1       2.1      2.5        17.      He3\r\n");
        fprintf(Ffi[k],"L1       2.5      3.2        17.      He3\r\n");
        fprintf(Ffi[k],"L1       3.2      4.0        17.      He3\r\n");
        fprintf(Ffi[k],"L1       4.0      5.0        17.      He3\r\n");
        fprintf(Ffi[k],"L1       5.0      6.2        17.      He3\r\n");
        fprintf(Ffi[k],"L1       6.2      8.0        17.      He3\r\n");
        fprintf(Ffi[k],"L1       8.0     10.0        17.      He3\r\n");
        fprintf(Ffi[k],"L1      10.0     14.0        17.      He3\r\n");
        fprintf(Ffi[k],"L1       1.65     2.0        17.      He4\r\n");
        fprintf(Ffi[k],"L1       2.0      2.4        17.      He4\r\n");
        fprintf(Ffi[k],"L1       2.4      3.0        17.      He4\r\n");
        fprintf(Ffi[k],"L1       3.0      3.7        17.      He4\r\n");
        fprintf(Ffi[k],"L1       3.7      4.53       17.      He4\r\n");
        fprintf(Ffi[k],"L1       4.53     6.0        17.      He4\r\n");
        fprintf(Ffi[k],"L1       6.0      7.4        17.      He4\r\n");
        fprintf(Ffi[k],"L1       7.4      9.64        9.      He4\r\n");
        fprintf(Ffi[k],"L1      10.0     14.0         9.      He4x\r\n");
        fprintf(Ffi[k],"L1       1.75     2.1        17.      b25\r\n");
        fprintf(Ffi[k],"L1       2.1      4.0        17.      b26\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      He frontQ\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      He backQ\r\n");
        fprintf(Ffi[k],"L1       5.0      8.0        17.      He frontQ\r\n");
        fprintf(Ffi[k],"L1       5.0      8.0        17.      He backH\r\n");
        fprintf(Ffi[k],"#4  Flux  Heavies , Win_A 13-15\r\n");
        fprintf(Ffi[k],"R2    0.01   1.e6      Win A 13-15 (c/s)\r\n");
        fprintf(Ffi[k],"L1     0   8000000     Nhvy proc\r\n");
        fprintf(Ffi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
        fprintf(Ffi[k],"L1       2.57     3.19       17.      C\r\n");  /* 68 8*/
        fprintf(Ffi[k],"L1       3.19     4.75       17.      C\r\n");
        fprintf(Ffi[k],"L1       4.75     9.1        17.      C\r\n");
        fprintf(Ffi[k],"L1       9.1     20.1        17.      C\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      N\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      N\r\n");
        fprintf(Ffi[k],"L1       2.56     3.17       17.      O\r\n");
        fprintf(Ffi[k],"L1       3.17     4.68       17.      O\r\n");
        fprintf(Ffi[k],"L1       4.68     9.2        17.      O\r\n");
        fprintf(Ffi[k],"L1       9.2     28.0        17.      O\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      F\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      F\r\n");
        fprintf(Ffi[k],"L1       3.27     4.72       17.      Ne\r\n");
        fprintf(Ffi[k],"L1       4.72     9.96       17.      Ne\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      Ne\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Na\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      Na\r\n");
        fprintf(Ffi[k],"L1       2.1      5.0        17.      Mg\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Mg\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      Mg\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Al\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      Al\r\n");
        fprintf(Ffi[k],"L1       3.2      5.0        17.      Si\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Si\r\n");
        fprintf(Ffi[k],"L1      10.0     30.0        17.      Si\r\n");
        fprintf(Ffi[k],"L1       3.2      5.0        17.      S \r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      S \r\n");
        fprintf(Ffi[k],"L1      10.0     50.0        17.      S \r\n");
        fprintf(Ffi[k],"L1       3.2      5.0        17.      Ar\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Ar\r\n");
        fprintf(Ffi[k],"L1      10.0     50.0        17.      Ar\r\n");
        fprintf(Ffi[k],"L1       3.2      5.0        17.      Ca\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Ca\r\n");
        fprintf(Ffi[k],"L1      10.0     50.0        17.      Ca\r\n");
        fprintf(Ffi[k],"L1       2.5      3.2        17.      Fe\r\n");  /* 135 10*/
        fprintf(Ffi[k],"L1       3.2      5.0        17.      Fe\r\n");
        fprintf(Ffi[k],"L1       5.0     10.0        17.      Fe\r\n");
        fprintf(Ffi[k],"L1      10.0     50.0        17.      Fe\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      b59\r\n");  /* 59 2*/
        fprintf(Ffi[k],"L1       5.0     20.0        17.      b60\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      B11\r\n");  /* 62 2*/
        fprintf(Ffi[k],"L1       5.0     20.0        17.      B11\r\n");
#if 0
        fprintf(Ffi[k],"L8       2.5      3.2        17.      C\r\n");  /* 68 8*/
        fprintf(Ffi[k],"L10      2.1      2.5        17.      O\r\n");  /* 82 10*/
        fprintf(Ffi[k],"L9       2.1      3.2        17.      Ne\r\n");  /* 95 9*/
        fprintf(Ffi[k],"L10      2.5      3.2        17.      Si\r\n");  /* 113 10*/
        fprintf(Ffi[k],"L10      2.5      3.2        17.      Fe\r\n");  /* 135 10*/
#endif
/* Individual spectral points  */
        fprintf(Ffi[k],"L1       2.57   3.19         17.      C\r\n");
        fprintf(Ffi[k],"L1       3.19   3.85         17.      C\r\n");
        fprintf(Ffi[k],"L1       3.85   4.8          17.      C\r\n");
        fprintf(Ffi[k],"L1       4.8    5.8          17.      C\r\n");
        fprintf(Ffi[k],"L1       5.8    7.2          17.      C\r\n");
        fprintf(Ffi[k],"L1       7.2    9.1          17.      C\r\n");
        fprintf(Ffi[k],"L1       9.1   13.7          17.      C\r\n");
        fprintf(Ffi[k],"L1      13.7   20.1          17.      C\r\n");
        fprintf(Ffi[k],"L1       2.56   3.17         17.      O\r\n");
        fprintf(Ffi[k],"L1       3.17   3.88         17.      O\r\n");
        fprintf(Ffi[k],"L1       3.88   4.68         17.      O\r\n");
        fprintf(Ffi[k],"L1       4.68   6.0          17.      O\r\n");
        fprintf(Ffi[k],"L1       6.0    7.4          17.      O\r\n");
        fprintf(Ffi[k],"L1       7.4    9.2          17.      O\r\n");
        fprintf(Ffi[k],"L1       9.2   13.4          17.      O\r\n");
        fprintf(Ffi[k],"L1      13.4   20.0          17.      O\r\n");
        fprintf(Ffi[k],"L1        3.27   3.98        17.      Ne\r\n");
        fprintf(Ffi[k],"L1        3.98   4.72        17.      Ne\r\n");
        fprintf(Ffi[k],"L1        4.72   5.92        17.      Ne\r\n");
        fprintf(Ffi[k],"L1        5.92   7.87        17.      Ne\r\n");
        fprintf(Ffi[k],"L1        7.87   9.96        17.      Ne\r\n");
        fprintf(Ffi[k],"L1        9.96  12.7         17.      Ne\r\n");
        fprintf(Ffi[k],"L1       12.7   18.4         17.      Ne\r\n");
        fprintf(Ffi[k],"L1        2.5    3.2         17.      Si\r\n");
        fprintf(Ffi[k],"L1        3.2    4.0         17.      Si\r\n");
        fprintf(Ffi[k],"L1        4.0    4.9         17.      Si\r\n");
        fprintf(Ffi[k],"L1        4.9    6.0         17.      Si\r\n");
        fprintf(Ffi[k],"L1        6.0    7.9         17.      Si\r\n");
        fprintf(Ffi[k],"L1        7.9    9.7         17.      Si\r\n");
        fprintf(Ffi[k],"L1        9.7   13.6         17.      Si\r\n");
        fprintf(Ffi[k],"L1       13.6   18.2         17.      Si\r\n");
        fprintf(Ffi[k],"L1         2.4    3.0        17.      Fe\r\n");
        fprintf(Ffi[k],"L1         3.0    3.95       17.      Fe\r\n");
        fprintf(Ffi[k],"L1         3.95   4.8        17.      Fe\r\n");
        fprintf(Ffi[k],"L1         4.8    5.9        17.      Fe\r\n");
        fprintf(Ffi[k],"L1         5.9    7.8        17.      Fe\r\n");
        fprintf(Ffi[k],"L1         7.8    9.3        17.      Fe\r\n");
        fprintf(Ffi[k],"L1         9.3   12.5        17.      Fe\r\n");
        fprintf(Ffi[k],"L1        12.5   18.2        17.      Fe\r\n");

        fprintf(Ffi[k],"L1       1.0     10.0        17.      Bkg1\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      O frontQ\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      O backQ\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      Fe frontQ\r\n");
        fprintf(Ffi[k],"L1       2.5      5.0        17.      Fe backQ\r\n");

        fprintf(Ffi[k],"L1       3.2      9.2        17.      O\r\n");

    if(!short_file)
    {
        fprintf(Ffi[k],"#5  Flux  Helium , Win_A 6-12\r\n");
        fprintf(Ffi[k],"R2    0.01   1.e6      Win A 6-12 (c/s)\r\n");
        fprintf(Ffi[k],"L1     0   8000000     NPHs win 6-12\r\n");
        for (i=3; i<12; i++)
            fprintf(Ffi[k],"L1      %5.2lf   %5.2lf        17.      He3 PH\r\n",
                Ebin[i], Ebin[i+1]);
        for (i=3; i<12; i++)
            fprintf(Ffi[k],"L1      %5.2lf   %5.2lf        17.      He4 PH\r\n",
                Ebin[i], Ebin[i+1]);
        fprintf(Ffi[k],"L1      %5.2lf   %5.2lf        17.      He4 PH\r\n",
              Ebin[7], Ebin[10 ]);
        fprintf(Ffi[k],"#6  Flux  Heavies , Win_A 15\r\n");
        fprintf(Ffi[k],"R2    0.01   1.e6      Win A 15 (c/s)\r\n");
        fprintf(Ffi[k],"L1     0   8000000     Nhvy PH\r\n");
        fprintf(Ffi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
        for (kp=7; kp<NPART; kp++)
        {
            if (!spec[kp].resp)
                    continue;
            for (i=6; i<15; i++)
            {
                fprintf(Ffi[k],"L1      %5.2lf   %5.2lf        17.      %3.3s PH\r\n",
                    Ebin[i], Ebin[i+1],spec[kp].name);
            }
        }
        for (kp=7; kp<NPART; kp++)
        {
            if (!spec[kp].resp)
                    continue;
            fprintf(Ffi[k],"L1      %5.2lf   %5.2lf        17.      %3.3s PH\r\n",
                Ebin[7], Ebin[10 ],spec[kp].name);
            fprintf(Ffi[k],"L1      %5.2lf   %5.2lf        17.      %3.3s PH\r\n",
                Ebin[10], Ebin[13],spec[kp].name);
        }
    }
            fprintf(Ffi[k],"#7  Flux           Ape B\r\n");
            fprintf(Ffi[k],"R2    0.01   1.e6      Ape B 2D (c/s)\r\n");
            fprintf(Ffi[k],"L1     0   8000000     Sum box\r\n");
            fprintf(Ffi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
            fprintf(Ffi[k],"L1       18.9    21.9       1.3      H\r\n");
            fprintf(Ffi[k],"L1       18.1    22.0       1.3     4He\r\n");
            fprintf(Ffi[k],"L1       22.0    32.0       1.3     4He\r\n");
            fprintf(Ffi[k],"L1       39.0    49.0       1.3      C\r\n");
            fprintf(Ffi[k],"L1       39.0    49.0       1.3      N\r\n");
            fprintf(Ffi[k],"L1       49.0    71.0       1.3      N\r\n");
            fprintf(Ffi[k],"L1       39.0    49.0       1.3      O\r\n");
            fprintf(Ffi[k],"L1       49.0    71.0       1.3      O\r\n");

            fprintf(Ffi[k],"#8  Flux           Ape A\r\n");
            fprintf(Ffi[k],"R2    0.01   1.e6      Ape A 2D (c/s)\r\n");
            fprintf(Ffi[k],"L1     0   8000000     Sum box\r\n");
           fprintf(Ffi[k],";TYPE E MIN E MAX GEOM PARTICLE\r\n");
/*         fprintf(Ffi[k],"L1    4.6     5.6  .06    bkg  0  \r\n");    */
           fprintf(Ffi[k],"L1    4.6     5.6  .06    H    1  \r\n");
           fprintf(Ffi[k],"L1    5.6     6.9  .06    H    2  \r\n");
/*
           fprintf(Ffi[k],"L1    6.9     8.9  1.2    H    3  \r\n");
           fprintf(Ffi[k],"L1    8.9    12.1  1.2    H    4  \r\n");
           fprintf(Ffi[k],"L1   12.1    14.1  1.2    H    5  \r\n");
           fprintf(Ffi[k],"L1   14.1    17.2  1.2    H    6  \r\n");
           fprintf(Ffi[k],"L1   17.2    21.2  1.2    H    7  \r\n");
           fprintf(Ffi[k],"L1   21.2    24.8  1.2    H    8  \r\n");
           fprintf(Ffi[k],"L1    4.4    16.8  1.2    H2   9  \r\n");
           fprintf(Ffi[k],"L1    5.7     7.0  1.2    He3 10  \r\n");
           fprintf(Ffi[k],"L1    7.0     9.0  1.2    He3 11  \r\n");
           fprintf(Ffi[k],"L1    9.0    12.1  1.2    He3 12  \r\n");
           fprintf(Ffi[k],"L1   12.1    17.2  1.2    He3 13  \r\n");
           fprintf(Ffi[k],"L1   17.2    25.9  1.2    He3 14  \r\n");
           fprintf(Ffi[k],"L1   25.9    29.2  1.2    He3 15  \r\n");
*/
           fprintf(Ffi[k],"L1    4.6     5.6  .06    He4 16  \r\n");
           fprintf(Ffi[k],"L1    5.6     6.9  .06    He4 17  \r\n");
           fprintf(Ffi[k],"L1    6.9     8.9  .06    He4 18  \r\n");
           fprintf(Ffi[k],"L1    8.9    12.1  .06    He4 19  \r\n");
           fprintf(Ffi[k],"L1   12.1    14.1  .06    He4 20  \r\n");
           fprintf(Ffi[k],"L1   14.1    17.2  .06    He4 21  \r\n");
           fprintf(Ffi[k],"L1   17.2    21.2  .06    He4 22  \r\n");
           fprintf(Ffi[k],"L1   21.2    23.3  .06    He4 23  \r\n");   /* was 24.6 */
/*
           fprintf(Ffi[k],"L1    4.6    6.4   1.2    bkg 24  \r\n");
           fprintf(Ffi[k],"L1    8.7    12.0  1.2    C   25  \r\n");
           fprintf(Ffi[k],"L1   12.0    17.2  1.2    C   26  \r\n");
           fprintf(Ffi[k],"L1   17.2    26.0  1.2    C   27  \r\n");
           fprintf(Ffi[k],"L1   26.0    33.8  1.2    C   28  \r\n");
           fprintf(Ffi[k],"L1   33.8    44.0  1.2    C   29  \r\n");
           fprintf(Ffi[k],"L1    8.7    12.0  1.2    N   30  \r\n");
           fprintf(Ffi[k],"L1   12.0    17.2  1.2    N   31  \r\n");
           fprintf(Ffi[k],"L1   17.2    26.0  1.2    N   32  \r\n");
           fprintf(Ffi[k],"L1   26.0    33.8  1.2    N   33  \r\n");
           fprintf(Ffi[k],"L1   33.8    44.0  1.2    N   34  \r\n");
           fprintf(Ffi[k],"L1    8.7    12.0  1.2    O   35  \r\n");
           fprintf(Ffi[k],"L1   12.0    17.2  1.2    O   36  \r\n");
           fprintf(Ffi[k],"L1   17.2    26.0  1.2    O   37  \r\n");
           fprintf(Ffi[k],"L1   26.0    33.8  1.2    O   38  \r\n");
           fprintf(Ffi[k],"L1   33.8    44.0  1.2    O   39  \r\n");
           fprintf(Ffi[k],"L1    8.7    12.0  1.2    bkg 40  \r\n");
           fprintf(Ffi[k],"L1   10.0    17.2  1.2    Ne  41  \r\n");
           fprintf(Ffi[k],"L1   17.2    26.0  1.2    Ne  42  \r\n");
           fprintf(Ffi[k],"L1   26.0    44.0  1.2    Ne  43  \r\n");
           fprintf(Ffi[k],"L1   17.2    26.0  1.2    Fe  44  \r\n");
           fprintf(Ffi[k],"L1   26.0    44.0  1.2    Fe  45  \r\n");
           fprintf(Ffi[k],"L1   44.0    80.0  1.2    Fe  46  \r\n");
*/
    if(!short_file)
    {
        fprintf(Ffi[k],"#9  Rate  Mag sectored rates\r\n");
        fprintf(Ffi[k],"L16     0   8000000      2.5  4  H   mag\r\n");
        fprintf(Ffi[k],"L16     0   8000000      2.5  5  4He mag\r\n");
        fprintf(Ffi[k],"L16     0   8000000      5    8  4He mag\r\n");
        fprintf(Ffi[k],"L16     0   8000000      2.5  5  O   mag\r\n");
        fprintf(Ffi[k],"L16     0   8000000      2.5  5  Fe  mag\r\n");
        fprintf(Ffi[k],"F2     .9       20.      2.5  4  H   F/B\r\n");
        fprintf(Ffi[k],"F2     .9       20.      2.5  5  4He F/B\r\n");
        fprintf(Ffi[k],"F2     .9       20.      5    8  4He F/B\r\n");
        fprintf(Ffi[k],"F2     .9       20.      2.5  5  O   F/B\r\n");
        fprintf(Ffi[k],"F2     .9       20.      2.5  5  Fe  F/B\r\n");
  }

        fprintf(Ffi[k],"#10 Flux  Z>=33 , Bin149  \r\n");
        fprintf(Ffi[k],"R2    1e-6   1.e6      Bin149  (c/s)\r\n");
        fprintf(Ffi[k],"L1     0   8000000     NBin149\r\n");
        fprintf(Ffi[k],";TYPE   E MIN    E MAX     GEOM     PARTICLE\r\n");
        for (i=0; i<NPART; i++)
        {
                if (spec[i].z<33.)
                {
                        LastlowZ=i;
                        continue;
                }
                fprintf(Ffi[k],"I1  3.3   10   17   %3.0lf %3.3s %3.0lf \r\n",
                         spec[i].z,spec[i].name, spec[i].a);
        }
        fprintf(Ffi[k],"I1  3.3   10   17    Z34-40  \r\n");
        fprintf(Ffi[k],"I1  3.3   10   17    Z41-49  \r\n");
        fprintf(Ffi[k],"I1  3.3   10   17    Z50-56  \r\n");
        fprintf(Ffi[k],"I1  3.3   10   17    Z57-61  \r\n");
        fprintf(Ffi[k],"I1  3.3   10   17    Z76-82  \r\n");
        for (j=6; j<12; j++)
        {
                fprintf(Ffi[k],"I1 %5.1lf %5.1lf 17  Z34-40  \r\n",
                        Ebin[j], Ebin[j+1]);
        }
        for (j=6; j<12; j++)
        {
                fprintf(Ffi[k],"I1 %5.1lf %5.1lf 17  Z50-56  \r\n",
                        Ebin[j], Ebin[j+1]);
        }

        fprintf(Ffi[k],"#End\r\n");
        fclose(Ffi[k]);
        Ffi[k]=0;

}

int daynum;

OutFlx(int k)
{
        long t1[2], sum[3];
        short vnum[1];
        int i, kp;
        float rate[2];
        long fptr, lptr;
        double strip_factor;
        float magfb[2];
        long sum_front, sum_back;

        if (k<0 || k>=NAVG)
        {
                printf("\n (OutFlx) Illegal k=%2d\n",k);
                fprintf(Efi,"\n (OutFlx) Illegal k=%2d\n",k);
                file_close(F_in[0]);
                exit(1);
        }
        Avg[k].W15=Avg[k].WinA[0][15]+Avg[k].WinA[1][15]+Avg[k].WinA[2][15];
        for (i=0; i<8; i++)
        {
                if (Avg[k].Spind[0])
                    Avg[k].HWrate[0][i]/=SpinPer*Avg[k].Spind[0];
                if (Avg[k].Spind[1])
                    Avg[k].HWrate[1][i]/=SpinPer*Avg[k].Spind[1];
                if (Avg[k].Spind[2])
                    Avg[k].HWrate[2][i]/=SpinPer*Avg[k].Spind[2];
        }
        for (i=0; i<16; i++)
        {
                if (Avg[k].Spind[0])
                {
                    Avg[k].DomeThr[0][i]*=16./(SpinPer*Avg[k].Spind[0]);
                    Avg[k].DomeSum[0]+=Avg[k].DomeThr[0][i];
                    Avg[k].WinA[0][i]/=SpinPer*Avg[k].Spind[0];
                }
                if (Avg[k].Spind[1])
                {
                    Avg[k].DomeThr[1][i]*=16./(SpinPer*Avg[k].Spind[1]);
                    Avg[k].DomeSum[1]+=Avg[k].DomeThr[1][i];
                    Avg[k].WinA[1][i]/=SpinPer*Avg[k].Spind[1];
                }
                if (Avg[k].Spind[2])
                {
                    Avg[k].DomeThr[2][i]*=16./(SpinPer*Avg[k].Spind[2]);
                    Avg[k].DomeSum[2]+=Avg[k].DomeThr[2][i];
                    Avg[k].WinA[2][i]/=SpinPer*Avg[k].Spind[2];
                }
                if (Avg[k].ApeSpins[0])
                    Avg[k].HWApeA[i]/=SpinPer*Avg[k].ApeSpins[0];
                if (Avg[k].ApeSpins[1])
                    Avg[k].HWApeB[i]/=SpinPer*Avg[k].ApeSpins[1];
                if (Avg[k].ApeSpins[2])   /* IT */
                {
                    Avg[k].HWIT[i]/=SpinPer*Avg[k].ApeSpins[2];
                    Avg[k].HWIT[i+16]/=SpinPer*Avg[k].ApeSpins[2];
                }
        }
        if (k==0)
        {
                /* Correct spins for deadtime of Dsum and Ep in each tel */
                /*  Before 9/8/98
                Avg[0].Live_spins=Avg[0].Spind[0]
                                *(exp(-50.e-6*Avg[0].DomeSum[0]
                                         -20.e-6*Avg[0].HWrate[0][5])  )
                                  +Avg[0].Spind[1]
                                *(exp(-35.e-6*Avg[0].DomeSum[1]
                                         - 5.e-6*Avg[0].HWrate[1][5])  )
                                  +Avg[0].Spind[2]
                                *(exp(-20.e-6*Avg[0].DomeSum[2]
                                         -20.e-6*Avg[0].HWrate[2][5])  );
                */
                /*  After  9/8/98
                if (Avg[0].Tint[0]>Striptime[1])
                        strip_factor=0.8;
                else
                        strip_factor=1.0;
                Avg[0].Live_spins=Avg[0].Spind[0]
                                *(0.30*exp(- 6.e-6*Avg[0].DomeSum[0])
                                  +0.70*exp(-40.e-5*Avg[0].DomeSum[0]))
                                        *exp(-12.e-6*Avg[0].HWrate[0][5]);
                Avg[0].Live_spins+=Avg[0].Spind[1]
                                *(0.20*exp(- 6.e-6*Avg[0].DomeSum[1])
                                  +0.80*exp(-80.e-6*Avg[0].DomeSum[1]))
                                        *exp(-20.e-6*Avg[0].HWrate[1][5]);
                Avg[0].Live_spins+=Avg[0].Spind[2]*strip_factor
                                *(0.50*exp(- 6.e-6*Avg[0].DomeSum[2])
                                  +0.50*exp(-20.e-6*Avg[0].DomeSum[2]))
                                        *exp(- 8.e-6*Avg[0].HWrate[2][5]);
                */
                /*  After  1/8/01 */
                /*  5/4/07  * COIN/Pks OK   not added - i.e. no change */
                if (Avg[0].Tint[0]>Striptime[1])
                        strip_factor=0.8;
                else
                        strip_factor=1.0;
                if (Avg[0].Tint[0]>Striptime[2]) /* turnoff C7 and C8 */
                        strip_factor=0.7;
                Avg[0].Live_spins=Avg[0].Spind[0]
                                *(0.40*exp(- 9.e-6*Avg[0].DomeSum[0])
                                  +0.60*exp(-40.e-5*Avg[0].DomeSum[0]))
                                        *exp(-7.0e-6*Avg[0].HWrate[0][5]);
                Avg[0].Live_spins+=Avg[0].Spind[1]
                                *(0.40*exp(- 9.e-6*Avg[0].DomeSum[1])
                                  +0.60*exp(-80.e-6*Avg[0].DomeSum[1]))
                                        *exp(-9.0e-6*Avg[0].HWrate[1][5]);
                Avg[0].Live_spins+=Avg[0].Spind[2]*strip_factor
                                *(0.50*exp(- 8.e-6*Avg[0].DomeSum[2])
                                  +0.50*exp(- 8.e-6*Avg[0].DomeSum[2]))
                                        *exp(-20.e-6*Avg[0].HWrate[2][5]);
                if (Thrstate==0)
                        Avg[0].He_spins=Avg[0].Live_spins;
                else if (Thrstate==1)
                        Avg[0].He_spins=Avg[0].Live_spins;
                else
                        Avg[0].He_spins=0;
                Avg[0].H_spins=Avg[0].He_spins;
                if (Thrstate==0)
                        Avg[0].H_spins=6.6*Avg[0].He_spins;
                /* Spread to other average periods */
                for (i=1; i<NAVG-6; i++)
                {
                        Avg[i].Live_spins+=Avg[0].Live_spins;
                        Avg[i].He_spins+=Avg[0].He_spins;
                        Avg[i].H_spins+=Avg[0].H_spins;
                }


        }
        if (k==3)  /* Special daily deadtime output */
        {
             /* if (Avg[k].DomeSum[1]<200.) */
                {
                    for (i=0;  i<3;  i++)
                    {
                        fprintf(Ofi,"0  %3d   %5.1f  %5.1f  %5.0f  %1d\n",
                            daynum, Avg[k].DomeSum[i],Avg[k].HWrate[i][5],Avg[k].Noxy[i],i);
                        sumd[i]+=Avg[k].DomeSum[i];
                        sume[i]+=Avg[k].HWrate[i][5];
                        sumn[i]+=Avg[k].Noxy[i];
                    sumi++;
                    }
                    fprintf(Ofi,"0   -1      -1       -1   -1\n");
                }
                daynum++;
        }
        for (i=0; i<3; i++)
        {
                if (!Avg[k].Spind[i])
                        continue;
                Avg[k].Nprots[i]/=SpinPer*Avg[k].Spind[i];
                Avg[k].Nhes[i]/=SpinPer*Avg[k].Spind[i];
                Avg[k].Nhvys[i]/=SpinPer*Avg[k].Spind[i];
                Avg[k].Noxy[i]/=SpinPer*Avg[k].Spind[i];

        }

        if (!*Fna[k])
                return;
        if (Ffi[k]==0)
        {
            if ((Ffi[k]=fopen(Fna[k],"ab"))==0)
            {
                    puts("\n (Outflx) Cannot open file ");
                    puts(Fna[k]);
                    file_close(F_in[0]);
                    exit(1);
            }
        }
/*
        sprintime(Avg[k].Tint[0],Tbuf);
        printf("\n %s",Tbuf);
        sprintime(Avg[k].Tint[1],Tbuf);
        printf("- %s ",Tbuf);
       printf(" k=%d  loc=%lx\n",k,fptr=ftell(Ffi[k]));
*/

        *vnum=0;
        fwrite(vnum,2,1,Ffi[k]);
        fwrite(Avg[k].Tint,4,2,Ffi[k]); /* actual interval contributing */
        if  (*Rna[k] )
        {
            if (Rfi[k]==0)
                Rfi[k]=fopen(Rna[k],"ab");
            fwrite(vnum,2,1,Rfi[k]);
            fwrite(Avg[k].Tint,4,2,Rfi[k]); /* actual interval contributing */
            *vnum=1;
            fwrite(vnum,2,1,Rfi[k]);
            fwrite(Avg[k].HWrate,4,24,Rfi[k]);
            fwrite(Avg[k].Spind,4,3,Rfi[k]);
            fwrite(Avg[k].Nprots,4,3,Rfi[k]);
            fwrite(Avg[k].Nhes,4,3,Rfi[k]);
            fwrite(Avg[k].Nhvys,4,3,Rfi[k]);
            fwrite(Avg[k].DomeThr,4,48,Rfi[k]);
            fwrite(Avg[k].WinA,4,48,Rfi[k]);
            fwrite(Avg[k].Maj,4,2,Rfi[k]);
            fwrite(Avg[k].ApeSpins,4,3,Rfi[k]);
            fwrite(Avg[k].ApeFrames,4,3,Rfi[k]);
            fwrite(Avg[k].HWApeA,4,13,Rfi[k]);
            fwrite(Avg[k].HWApeB,4,12,Rfi[k]);
            fwrite(Avg[k].HWIT,4,32,Rfi[k]);
            fwrite(Avg[k].ApeAbox,4,NABINS,Rfi[k]);
            fwrite(Avg[k].ApeBbox,4,NBBINS,Rfi[k]);

            fwrite(Avg[k].DomeSum,4,3,Rfi[k]);
            fwrite(Avg[k].Noxy,4,3,Rfi[k]);
            rate[0]=Avg[k].Spins;
            rate[1]=Avg[k].Live_spins;
            fwrite(rate,4,2,Rfi[k]);
/* ABC SW rates */
            fwrite(Avg[k].ASWbox,4,150,Rfi[k]);
            fwrite(Avg[k].BSWbox,4,150,Rfi[k]);
            fwrite(Avg[k].CSWbox,4,150,Rfi[k]);

            ov.vnbr=2;
            ov.rcnt=Avg[k].Sum1_5;
            ov.rtim=SpinPer*Avg[k].H_spins;        /* Thresh corr */
            ov.mcnt=Avg[k].Sprot;
            fwrite((char *) &ov.vnbr,1,14,Rfi[k]);
            fwrite(Avg[k].SWbox+5,4,3,Rfi[k]);

            ov.vnbr=3;
            ov.rcnt=Avg[k].HWApeB[3]*SpinPer*Avg[k].ApeSpins[1];
            ov.rtim=SpinPer*Avg[k].ApeSpins[1];
            ov.mcnt=Avg[k].ApeBbox[0]+Avg[k].ApeBbox[1]+Avg[k].ApeBbox[2]+
                    Avg[k].ApeBbox[3]+
                    Avg[k].ApeBbox[13]+Avg[k].ApeBbox[18]+Avg[k].ApeBbox[19]+
                    Avg[k].ApeBbox[27]+Avg[k].ApeBbox[43]+Avg[k].ApeBbox[37];
            if (ov.rtim>1. && ov.rcnt/ov.rtim <100.)
                ov.rcnt=ov.mcnt;
            fwrite((char *) &ov.vnbr,1,14,Rfi[k]);
            fwrite(Avg[k].ApeBbox+1,4,3,Rfi[k]);
            fwrite(Avg[k].ApeBbox+18,4,3,Rfi[k]);
/*
            fclose(Rfi[k]);
            Rfi[k]=0;
*/
        }
        ov.vnbr=2;
        ov.rcnt=Avg[k].Sum1_5;
        ov.rtim=SpinPer*Avg[k].H_spins;        /* Thresh corr */
        ov.mcnt=Avg[k].Sprot;
/*printf(" cnt=%8.2f tim=%8.2f matrc=%8ld\n",ov.rcnt,ov.rtim,ov.mcnt); */
        fwrite((char *) &ov.vnbr,1,14,Ffi[k]);
        fwrite(Avg[k].SWbox+5,4,3,Ffi[k]);
        fwrite(Avg[k].SWbox,4,1,Ffi[k]);     /* Bkg 0 */
        sum_front =Avg[k].Magsec[0][0]+Avg[k].Magsec[0][15];
        sum_front+=Avg[k].Magsec[0][1]+Avg[k].Magsec[0][14];
/*fb    sum_front+=Avg[k].Magsec[0][2]+Avg[k].Magsec[0][13];
        sum_front+=Avg[k].Magsec[0][3]+Avg[k].Magsec[0][12]; */
        sum_back  =Avg[k].Magsec[0][7]+Avg[k].Magsec[0][ 8];
        sum_back +=Avg[k].Magsec[0][6]+Avg[k].Magsec[0][ 9];
/*fb    sum_back +=Avg[k].Magsec[0][5]+Avg[k].Magsec[0][10];
        sum_back +=Avg[k].Magsec[0][4]+Avg[k].Magsec[0][11]; */
        fwrite(&sum_front,4,1,Ffi[k]);
        fwrite(&sum_back ,4,1,Ffi[k]);

        ov.vnbr=3;
        ov.rcnt=Avg[k].Sum6_12;
        ov.rtim=SpinPer*Avg[k].He_spins;        /* Thresh corr */
        ov.mcnt=Avg[k].She;
        fwrite((char *) &ov.vnbr,1,14,Ffi[k]);
        fwrite(Avg[k].SWbox+28,4,9,Ffi[k]);       /* 3He bins 3-11  */
        fwrite(Avg[k].SWbox+38,4,9,Ffi[k]);       /* 4He bins 3-11  */
        fwrite(Avg[k].SWbox+25,4,2,Ffi[k]);       /* bkg bins 25-26 */
        sum_front =Avg[k].Magsec[1][0]+Avg[k].Magsec[1][15];
        sum_front+=Avg[k].Magsec[1][1]+Avg[k].Magsec[1][14];
/*fb    sum_front+=Avg[k].Magsec[1][2]+Avg[k].Magsec[1][13];
        sum_front+=Avg[k].Magsec[1][3]+Avg[k].Magsec[1][12]; */
        sum_back  =Avg[k].Magsec[1][7]+Avg[k].Magsec[1][ 8];
        sum_back +=Avg[k].Magsec[1][6]+Avg[k].Magsec[1][ 9];
/*fb    sum_back +=Avg[k].Magsec[1][5]+Avg[k].Magsec[1][10];
        sum_back +=Avg[k].Magsec[1][4]+Avg[k].Magsec[1][11]; */
        fwrite(&sum_front,4,1,Ffi[k]);
        fwrite(&sum_back ,4,1,Ffi[k]);
        sum_front =Avg[k].Magsec[2][0]+Avg[k].Magsec[2][15];
        sum_front+=Avg[k].Magsec[2][1]+Avg[k].Magsec[2][14];
/*fb    sum_front+=Avg[k].Magsec[2][2]+Avg[k].Magsec[2][13];
        sum_front+=Avg[k].Magsec[2][3]+Avg[k].Magsec[2][12]; */
        sum_back  =Avg[k].Magsec[2][7]+Avg[k].Magsec[2][ 8];
        sum_back +=Avg[k].Magsec[2][6]+Avg[k].Magsec[2][ 9];
/*fb    sum_back +=Avg[k].Magsec[2][5]+Avg[k].Magsec[2][10];
        sum_back +=Avg[k].Magsec[2][4]+Avg[k].Magsec[2][11]; */
        fwrite(&sum_front,4,1,Ffi[k]);
        fwrite(&sum_back ,4,1,Ffi[k]);

        ov.vnbr=4;
        ov.rcnt=Avg[k].Sum13_15;
        ov.rtim=SpinPer*Avg[k].Live_spins;
        ov.mcnt=Avg[k].Shvy;
        ov.lfbin[0]=Avg[k].SWbox[68];          /* C bin 5      */
        ov.lfbin[1]=Avg[k].SWbox[69];          /* C bin 6-7    */
        ov.lfbin[1]+=Avg[k].SWbox[70];
        ov.lfbin[2]=Avg[k].SWbox[71];          /* C bin 8-10   */
        ov.lfbin[2]+=Avg[k].SWbox[72];
        ov.lfbin[2]+=Avg[k].SWbox[73];
        ov.lfbin[3]=Avg[k].SWbox[74];          /* C bin 11-12   */
        ov.lfbin[3]+=Avg[k].SWbox[75];
        ov.lfbin[4]=Avg[k].SWbox[80];          /* N bin 8-10   */
        ov.lfbin[5]=Avg[k].SWbox[81];          /* N bin 11-13   */
        ov.lfbin[6]=Avg[k].SWbox[83];          /* O bin 5      */
        ov.lfbin[7]=Avg[k].SWbox[84];          /* O bin 6-7    */
        ov.lfbin[7]+=Avg[k].SWbox[85];
        ov.lfbin[8]=Avg[k].SWbox[86];          /* O bin 8-10   */
        ov.lfbin[8]+=Avg[k].SWbox[87];
        ov.lfbin[8]+=Avg[k].SWbox[88];
        ov.lfbin[9]=Avg[k].SWbox[89];          /* O bin 11-13   */
        ov.lfbin[9]+=Avg[k].SWbox[90];
        ov.lfbin[9]+=Avg[k].SWbox[91];
        ov.lfbin[10]=Avg[k].SWbox[93];          /* F bin 8-10   */
        ov.lfbin[11]=Avg[k].SWbox[94];          /* F bin 11-13   */
        ov.lfbin[12]=Avg[k].SWbox[96];          /* Ne bin 6-7    */
        ov.lfbin[12]+=Avg[k].SWbox[97];
        ov.lfbin[13]=Avg[k].SWbox[98];          /* Ne bin 8-10   */
        ov.lfbin[13]+=Avg[k].SWbox[99];
        ov.lfbin[13]+=Avg[k].SWbox[100];
        ov.lfbin[14]=Avg[k].SWbox[101];          /* Ne bin 11-13   */
        ov.lfbin[14]+=Avg[k].SWbox[102];
        ov.lfbin[14]+=Avg[k].SWbox[103];
        ov.lfbin[15]=Avg[k].SWbox[105];          /* Na bin 8-10   */
        ov.lfbin[16]=Avg[k].SWbox[106];          /* Na bin 11-13   */
        ov.lfbin[17]=Avg[k].SWbox[107];          /* Mg bin 4-7    */
        ov.lfbin[18]=Avg[k].SWbox[108];          /* Mg bin 8-10   */
        ov.lfbin[19]=Avg[k].SWbox[109];          /* Mg bin 11-13   */
        ov.lfbin[20]=Avg[k].SWbox[111];          /* Al bin 8-10   */
        ov.lfbin[21]=Avg[k].SWbox[112];          /* Al bin 11-13   */
        ov.lfbin[22]=Avg[k].SWbox[114];          /* Si bin 6-7    */
        ov.lfbin[22]+=Avg[k].SWbox[115];
        ov.lfbin[23]=Avg[k].SWbox[116];          /* Si bin 8-10   */
        ov.lfbin[23]+=Avg[k].SWbox[117];
        ov.lfbin[23]+=Avg[k].SWbox[118];
        ov.lfbin[24]=Avg[k].SWbox[119];          /* Si bin 11-13   */
        ov.lfbin[24]+=Avg[k].SWbox[120];
        ov.lfbin[24]+=Avg[k].SWbox[121];
        ov.lfbin[25]=Avg[k].SWbox[124];          /* S  bin 6-7     */
        ov.lfbin[26]=Avg[k].SWbox[125];          /* S  bin 8-10   */
        ov.lfbin[27]=Avg[k].SWbox[126];          /* S  bin 11-13   */
        ov.lfbin[28]=Avg[k].SWbox[128];          /* Ar bin 6-7     */
        ov.lfbin[29]=Avg[k].SWbox[129];          /* Ar bin 8-10   */
        ov.lfbin[30]=Avg[k].SWbox[130];          /* Ar bin 11-13   */
        ov.lfbin[31]=Avg[k].SWbox[132];          /* Ca bin 6-7     */
        ov.lfbin[32]=Avg[k].SWbox[133];          /* Ca bin 8-10   */
        ov.lfbin[33]=Avg[k].SWbox[134];          /* Ca bin 11-13   */
        ov.lfbin[34]=Avg[k].SWbox[135];          /* Fe bin 5      */
        ov.lfbin[35]=Avg[k].SWbox[136];          /* Fe bin 6-7    */
        ov.lfbin[35]+=Avg[k].SWbox[137];
        ov.lfbin[36]=Avg[k].SWbox[138];          /* Fe bin 8-10   */
        ov.lfbin[36]+=Avg[k].SWbox[139];
        ov.lfbin[36]+=Avg[k].SWbox[140];
        ov.lfbin[37]=Avg[k].SWbox[141];          /* Fe bin 11-13   */
        ov.lfbin[37]+=Avg[k].SWbox[142];
        ov.lfbin[37]+=Avg[k].SWbox[143];
        ov.lfbin[37]+=Avg[k].SWbox[144];
        ov.lfbin[38]=Avg[k].SWbox[59];          /* b59            */
        ov.lfbin[39]=Avg[k].SWbox[60];          /* b60            */
        ov.lfbin[40]=Avg[k].SWbox[62];          /* b62 B11        */
        ov.lfbin[41]=Avg[k].SWbox[63];          /* b63 B11        */
/*        ov.lfbin[42]=ov.lfbin[7]+ov.lfbin[8];     3.2-9.2 O see later   */
       fwrite((char *) &ov.vnbr,1,182,Ffi[k]);

        fwrite((char *) &Avg[k].SWbox[68],4,8,Ffi[k]);
        fwrite((char *) &Avg[k].SWbox[83],4,8,Ffi[k]);
        fwrite((char *) &Avg[k].SWbox[96],4,7,Ffi[k]);
        fwrite((char *) &Avg[k].SWbox[113],4,8,Ffi[k]);
        fwrite((char *) &Avg[k].SWbox[135],4,8,Ffi[k]);

/*     printf(" k=%d  loc=%lx",k,lptr=ftell(Ffi[k]));
       printf("   diff=%lx or %ld \n",lptr-fptr,lptr-fptr);  */
        fwrite(Avg[k].SWbox+1,4,1,Ffi[k]);     /* Bkg 1 */
        sum_front =Avg[k].Magsec[3][0]+Avg[k].Magsec[3][15];
        sum_front+=Avg[k].Magsec[3][1]+Avg[k].Magsec[3][14];
/*fb    sum_front+=Avg[k].Magsec[3][2]+Avg[k].Magsec[3][13];
        sum_front+=Avg[k].Magsec[3][3]+Avg[k].Magsec[3][12]; */
        sum_back  =Avg[k].Magsec[3][7]+Avg[k].Magsec[3][ 8];
        sum_back +=Avg[k].Magsec[3][6]+Avg[k].Magsec[3][ 9];
/*fb    sum_back +=Avg[k].Magsec[3][5]+Avg[k].Magsec[3][10];
        sum_back +=Avg[k].Magsec[3][4]+Avg[k].Magsec[3][11]; */
        fwrite(&sum_front,4,1,Ffi[k]);
        fwrite(&sum_back ,4,1,Ffi[k]);
        sum_front =Avg[k].Magsec[4][0]+Avg[k].Magsec[4][15];
        sum_front+=Avg[k].Magsec[4][1]+Avg[k].Magsec[4][14];
/*fb    sum_front+=Avg[k].Magsec[4][2]+Avg[k].Magsec[4][13];
        sum_front+=Avg[k].Magsec[4][3]+Avg[k].Magsec[4][12]; */
        sum_back  =Avg[k].Magsec[4][7]+Avg[k].Magsec[4][ 8];
        sum_back +=Avg[k].Magsec[4][6]+Avg[k].Magsec[4][ 9];
/*fb    sum_back +=Avg[k].Magsec[4][5]+Avg[k].Magsec[4][10];
        sum_back +=Avg[k].Magsec[4][4]+Avg[k].Magsec[4][11]; */
        fwrite(&sum_front,4,1,Ffi[k]);
        fwrite(&sum_back ,4,1,Ffi[k]);

        ov.lfbin[42]=ov.lfbin[7]+ov.lfbin[8];    /* 3.2-9.2 O      */
       fwrite((char *) &ov.lfbin[42],1,4,Ffi[k]);

    if(!short_file)
    {
        ov.vnbr=5;                              /* He PH bins */
        ov.rcnt=Avg[k].Sum6_12;
        ov.rtim=SpinPer*Avg[k].He_spins;        /* Thresh corr */
        ov.mcnt=Avg[k].PHwin6_12;
        fwrite((char *) &ov.vnbr,1,14,Ffi[k]);
        fwrite(Avg[k].PHbox[4]+3,4,9,Ffi[k]);       /* 3He bins 3-11  */
        fwrite(Avg[k].PHbox[5]+3,4,9,Ffi[k]);       /* 4He bins 3-11  */
        ov.lfbin[0]= Avg[k].PHbox[5][7]+Avg[k].PHbox[5][8]+Avg[k].PHbox[5][9];
        fwrite(ov.lfbin,4,1,Ffi[k]);       /* 4He bins 7-9 for quiet  */
        ov.vnbr=6;                              /* Hvy PH bins */
        ov.rcnt=Avg[k].W15;
        ov.rtim=SpinPer*Avg[k].Live_spins;        /* Thresh corr */
        ov.mcnt=Avg[k].PHwina[1][15];
        fwrite((char *) &ov.vnbr,1,14,Ffi[k]);
        for (kp=7; kp<NPART; kp++)
        {
            if (!spec[kp].resp)
                    continue;
            fwrite(Avg[k].PHbox[kp]+6,4,9,Ffi[k]);
        }
        for (kp=7; kp<NPART; kp++)
        {
            if (!spec[kp].resp)
                    continue;
            ov.lfbin[0]= Avg[k].PHbox[kp][7]+Avg[k].PHbox[kp][8]+Avg[k].PHbox[kp][9];
            ov.lfbin[1]= Avg[k].PHbox[kp][10]+Avg[k].PHbox[kp][11]+Avg[k].PHbox[kp][12];
            fwrite(ov.lfbin,4,2,Ffi[k]);        /* sums for quiet times */
        }

    }
/*  Ape B  */
            ov.vnbr=7;
            ov.rcnt=Avg[k].HWApeB[3]*SpinPer*Avg[k].ApeSpins[1];
            ov.rtim=SpinPer*Avg[k].ApeSpins[1];
            ov.mcnt=Avg[k].ApeBbox[0]+Avg[k].ApeBbox[1]+Avg[k].ApeBbox[2]+
                    Avg[k].ApeBbox[3]+
                    Avg[k].ApeBbox[13]+Avg[k].ApeBbox[18]+Avg[k].ApeBbox[19]+
                    Avg[k].ApeBbox[27]+Avg[k].ApeBbox[43]+Avg[k].ApeBbox[37];
            if (ov.rtim>1. && ov.rcnt/ov.rtim <100.)
                ov.rcnt=ov.mcnt;
            ov.lfbin[0]=Avg[k].ApeBbox[1];          /* H */
            ov.lfbin[1]=Avg[k].ApeBbox[13]+Avg[k].ApeBbox[18];   /* He */
            ov.lfbin[2]=Avg[k].ApeBbox[14]+Avg[k].ApeBbox[15];
            ov.lfbin[2]+=Avg[k].ApeBbox[19]+Avg[k].ApeBbox[20]+Avg[k].ApeBbox[21];
            ov.lfbin[3]=Avg[k].ApeBbox[28];         /* C */
            ov.lfbin[4]=Avg[k].ApeBbox[32];         /* N */
            ov.lfbin[5]=Avg[k].ApeBbox[29];         /* N */
            ov.lfbin[6]=Avg[k].ApeBbox[37];         /* O */
            ov.lfbin[7]=Avg[k].ApeBbox[33];         /* O */
            fwrite((char *) &ov.vnbr,1,46,Ffi[k]);
/*  Ape A  */
            ov.vnbr=8;
            ov.rcnt=Avg[k].HWApeA[8]*SpinPer*Avg[k].ApeSpins[1];
            ov.rtim=SpinPer*Avg[k].ApeSpins[1];
            ov.mcnt=Avg[k].ApeAbox[0]+Avg[k].ApeAbox[1]+Avg[k].ApeAbox[2];
            ov.lfbin[0]=Avg[k].ApeAbox[1];
            ov.lfbin[1]=Avg[k].ApeAbox[2];
            ov.lfbin[2]=Avg[k].ApeAbox[16];
            ov.lfbin[3]=Avg[k].ApeAbox[17];
            ov.lfbin[4]=Avg[k].ApeAbox[18];
            ov.lfbin[5]=Avg[k].ApeAbox[19];
            ov.lfbin[6]=Avg[k].ApeAbox[20];
            ov.lfbin[7]=Avg[k].ApeAbox[21];
            ov.lfbin[8]=Avg[k].ApeAbox[22];
            ov.lfbin[9]=Avg[k].ApeAbox[23];
            fwrite((char *) &ov.vnbr,1,54,Ffi[k]);
/* Mag sector data */

    if(!short_file)
    {
            ov.vnbr=9;
            fwrite((char *) &ov.vnbr,1,2,Ffi[k]);
            fwrite((char *) &Avg[k].Magsec[0][0],1,320,Ffi[k]);
            for (i=0; i<5; i++)
            {
                double num, den, pct;
                magfb[0]=0.;
                magfb[1]=0.;
                num=Avg[k].Magsec[i][0]+Avg[k].Magsec[i][15];
                den=Avg[k].Magsec[i][7]+Avg[k].Magsec[i][8];
                if (num>0.1 && den>0.1)
                {
                        pct= sqrt(1./num+1/den);
                        magfb[0]=num/den;
                        if (num < den)
                            magfb[0]=den/num;
                        magfb[1]=magfb[0]*pct;
                }
                if (num>0.1 && den<0.1)
                {
                        magfb[0]=num;
                        magfb[1]=sqrt(num)+1.;
                }
                if (num<0.1 && den>0.1)
                {
                        magfb[0]=den;
                        magfb[1]=sqrt(den)+1.;
                }
                fwrite((char *) magfb,1,8,Ffi[k]);
            }
    }

/* Z>33  */

        {
            short *shbin;
            int j, ii, iii;
            shbin= (short *) ov.lfbin;
            iii=NPART-LastlowZ-1;
            shbin[iii]=shbin[iii+1]=shbin[iii+2]=shbin[iii+3]=shbin[iii+4]=0;
            for (j=0; j<17; j++)
                shbin[iii+j]=0;
            for (i=LastlowZ+1, ii=0; i<NPART; i++, ii++)
            {
                    shbin[ii]=0;
                    for (j=6; j<11; j++)
                    {
                            shbin[ii]+=Avg[k].PHbox[i][j];
                    }
                    if (spec[i].z>33. && spec[i].z<41.)
                    {
                        shbin[iii]+=shbin[ii];
                        for (j=6; j<12; j++)
                        {
                            shbin[iii+j-1]+=Avg[k].PHbox[i][j];
                        }
                    }
                    if (spec[i].z>=41. && spec[i].z<=49.)
                        shbin[iii+1]+=shbin[ii];
                    if (spec[i].z>49. && spec[i].z<57.)
                    {
                        shbin[iii+2]+=shbin[ii];
                        for (j=6; j<12; j++)
                        {
                            shbin[iii+j+5]+=Avg[k].PHbox[i][j];
                        }
                    }
                    if (spec[i].z>=57. && spec[i].z<61.)
                        shbin[iii+3]+=shbin[ii];
                    if (spec[i].z>75. && spec[i].z<83.)
                        shbin[iii+4]+=shbin[ii];

            }
/*
            if (k==1)
            {
                sprintime(Avg[k].Tint[0],Tbuf);
                fprintf(Efi," %s",Tbuf);
                for (ii=0; ii<iii+5; ii++)
                    fprintf(Efi," %2hd",shbin[ii]);
                fprintf(Efi,"\n");
            }
*/
            ov.vnbr=10;                              /* Hvy PH bins */
            ov.rcnt=Avg[k].SWbox[149];
            ov.rtim=SpinPer*Avg[k].Live_spins;        /* Thresh corr */
            ov.mcnt=Avg[k].PH149;
/*
            if (k==1)
            {
                sprintime(Avg[k].Tint[0],Tbuf);
                fprintf(Efi," %s Box149=%4ld, Box146=%4ld, dt=%6.1hf n149=%5ld"
                    ,Tbuf, Avg[k].SWbox[149], Avg[k].SWbox[146], ov.rtim,
                    ov.mcnt);
                fprintf (Efi, " N= %4hd %4hd %4hd PHbox= %4ld\n",
                    shbin[0],shbin[1],shbin[2],
                    Avg[k].PHbox[LastlowZ+1][7]);

            }
*/
            fwrite((char *) &ov.vnbr,1,14+2*(iii+5+12),Ffi[k]);
        }

/*
        fclose(Ffi[k]);
        Ffi[k]=0;
*/
}
Clear_avg(int k)
{
        int i, j;

        Avg[k].Allbox=0.;
        for (i=0; i<150; i++)
        {
            Avg[k].SWbox[i]=0;
            Avg[k].ASWbox[i]=0;
            Avg[k].BSWbox[i]=0;
            Avg[k].CSWbox[i]=0;
        }
        for (j=0; j<5; j++)
        {
                for (i=0; i<16; i++)
                        Avg[k].Magsec[j][i]=0;
        }
        for (j=0; j<NPART; j++)   /* binned PH data */
        {
                for (i=0; i<16; i++)
                        Avg[k].PHbox[j][i]=0;
        }
        for (j=0; j<NFINEZ; j++)   /* binned PH Z data */
        {
                for (i=0; i<8; i++)
                        Avg[k].PHzbox[j][i]=0;
        }
        for (i=0; i<16; i++)
        {
                Avg[k].PHwina[0][i]=0;
                Avg[k].PHwina[1][i]=0;
        }
        Avg[k].PH149=0.;
        Avg[k].PHwin6_12=0.;
        for (i=0; i<8; i++)
        {
            Avg[k].HWrate[0][i]=0.;
            Avg[k].HWrate[1][i]=0.;
            Avg[k].HWrate[2][i]=0.;
        }
        for (i=0; i<16; i++)
        {
            Avg[k].DomeThr[0][i]=0.;
            Avg[k].DomeThr[1][i]=0.;
            Avg[k].DomeThr[2][i]=0.;
            Avg[k].WinA[0][i]=0.;
            Avg[k].WinA[1][i]=0.;
            Avg[k].WinA[2][i]=0.;
            Avg[k].HWApeA[i]=0.;
            Avg[k].HWApeB[i]=0.;
            Avg[k].HWIT[i]=0.;
            Avg[k].HWIT[i+16]=0.;
        }
        for (i=0; i<NABINS; i++)
        {
            Avg[k].ApeAbox[i]=0;
        }
        for (i=0; i<NBBINS; i++)
        {
            Avg[k].ApeBbox[i]=0;
        }
        for (i=0; i<3; i++)
        {
            Avg[k].Nprots[i]=0.;
            Avg[k].Nhes[i]=0.;
            Avg[k].Nhvys[i]=0.;
            Avg[k].Spind[i]=0.;
            Avg[k].DomeSum[i]=0.;
            Avg[k].Noxy[i]=0.;
            if (i>2)
                continue;
            Avg[k].ApeFrames[i]=0.;
            Avg[k].ApeSpins[i]=0.;
        }

        Avg[k].Sprot=0.;
        Avg[k].She=0.;
        Avg[k].Shvy=0.;
        Avg[k].Spins=0.;
        Avg[k].Live_spins=0.;
        Avg[k].He_spins=0.;
        Avg[k].H_spins=0.;
        Avg[k].Sum1_5=0.;
        Avg[k].Sum6_12=0.;
        Avg[k].Sum13_15=0.;
        Avg[k].Tint[0]=0;
        Avg[k].Maj[0]=0;
        Avg[k].Tint[1]=0;
        Avg[k].Maj[1]=0;
        Avg[k].Maxthr=0;
        Firstt=0;
}
Sum_flux()
{
        int i, k;
        Inbelts=0;
        for (i=0; i<NPASS; i++)
        {
                if (Avg[0].Tint[1] > BeltTime[i][0]
                    && Avg[0].Tint[0] < BeltTime[i][1] )
                {
                        Inbelts=1;
                        Npha=0;
                        Ipha=1;
/*                      printf("\n In belts\n"); */
                }
        }
        if (Inbelts)
                return;
        if (Avg[0].Tint[0]>Threshtime[1])
                Thrstate=1;          /* "Final" Dref=1 on tel A & C */
        else if (Avg[0].Tint[1]<Threshtime[0])
                Thrstate=0;          /* initial Dref=0n on all tels */
        else
        {
                Thrstate=2;          /* Temp Dref=2 on tel A & C */
                Thrstate=1;
/*
                for (i=2; i<47; i++)
                {
                    Avg[0].SWbox[i]=0;
                }
                Avg[0].She=0.;
*/
                Avg[0].Sprot=0.;
        }
        for (k=0; k<NPART; k++)   /* Get binned PH data */
        {
                for (i=0; i<16; i++)
                {
                        Avg[0].PHbox[k][i]=Binbuf[k][i];
                        Binbuf[k][i]=0;
                }
        }
        for (k=0; k<NFINEZ; k++)   /* Get binned PH Z data */
        {
                for (i=0; i<8; i++)
                {
                        Avg[0].PHzbox[k][i]=Binzbuf[k][i];
                        Binzbuf[k][i]=0;
                }
        }
        for (i=0; i<16; i++)
        {
                Avg[0].PHwina[0][i]=Win[0][i];
                Avg[0].PHwina[1][i]=Win[1][i];
                if (i>5 && i<13)
                        Avg[0].PHwin6_12+=Win[1][i];
                Win[0][i]=0;
                Win[1][i]=0;
        }
        Avg[0].PH149=Bin149;
        Bin149=0;
        for (k=1; k<7     ; k++)  /* all but quiet */
        {
                Sum_each(k,0);
        }
}
Sum_each(int k,int l)
{
        int i, j;
        Avg[k].Allbox+=Avg[l].Allbox;
        for (i=0; i<150; i++)
        {
            Avg[k].SWbox[i]+=Avg[l].SWbox[i];
            Avg[k].ASWbox[i]+=Avg[l].ASWbox[i];
            Avg[k].BSWbox[i]+=Avg[l].BSWbox[i];
            Avg[k].CSWbox[i]+=Avg[l].CSWbox[i];
        }
        for(j=0; j<5; j++)
        {
                for (i=0; i<16; i++)
                        Avg[k].Magsec[j][i]+=Avg[l].Magsec[j][i];
        }
        for (j=0; j<NPART; j++)   /* Get binned PH data */
        {
                for (i=0; i<16; i++)
                        Avg[k].PHbox[j][i]+=Avg[l].PHbox[j][i];
        }
        for (j=0; j<NFINEZ; j++)   /* Get binned PH Z data */
        {
                for (i=0; i<8; i++)
                        Avg[k].PHzbox[j][i]+=Avg[l].PHzbox[j][i];
        }
        for (i=0; i<16; i++)
        {
                Avg[k].PHwina[0][i]+=Avg[l].PHwina[0][i];
                Avg[k].PHwina[1][i]+=Avg[l].PHwina[1][i];
        }
        Avg[k].PHwin6_12+=Avg[l].PHwin6_12;
        Avg[k].PH149+=Avg[l].PH149;
        for (i=0; i<8; i++)
        {
            Avg[k].HWrate[0][i]+=Avg[l].HWrate[0][i];
            Avg[k].HWrate[1][i]+=Avg[l].HWrate[1][i];
            Avg[k].HWrate[2][i]+=Avg[l].HWrate[2][i];
        }
        for (i=0; i<16; i++)
        {
            Avg[k].DomeThr[0][i]+=Avg[l].DomeThr[0][i];
            Avg[k].DomeThr[1][i]+=Avg[l].DomeThr[1][i];
            Avg[k].DomeThr[2][i]+=Avg[l].DomeThr[2][i];
            Avg[k].WinA[0][i]+=Avg[l].WinA[0][i];
            Avg[k].WinA[1][i]+=Avg[l].WinA[1][i];
            Avg[k].WinA[2][i]+=Avg[l].WinA[2][i];
            Avg[k].HWApeA[i]+=Avg[l].HWApeA[i];
            Avg[k].HWApeB[i]+=Avg[l].HWApeB[i];
            Avg[k].HWIT[i]+=Avg[l].HWIT[i];
            Avg[k].HWIT[i+16]+=Avg[l].HWIT[i+16];
        }
        for (i=0; i<3; i++)
        {
            Avg[k].Nprots[i]+=Avg[l].Nprots[i];
            Avg[k].Nhes[i]+=  Avg[l].Nhes[i];
            Avg[k].Nhvys[i]+= Avg[l].Nhvys[i];
            Avg[k].Spind[i]+= Avg[l].Spind[i];
            Avg[k].DomeSum[i]+=Avg[l].DomeSum[i];
            Avg[k].Noxy[i]+=  Avg[l].Noxy[i];
            if (i>2)
                continue;
            Avg[k].ApeFrames[i]+=Avg[l].ApeFrames[i];
            Avg[k].ApeSpins[i]+=Avg[l].ApeSpins[i];
        }
        for (i=0; i<NABINS; i++)
        {
                Avg[k].ApeAbox[i]+=Avg[l].ApeAbox[i];
        }
        for (i=0; i<NBBINS; i++)
        {
                Avg[k].ApeBbox[i]+=Avg[l].ApeBbox[i];
        }

        Avg[k].Sprot+=Avg[l].Sprot;
        Avg[k].She+=Avg[l].She;
        Avg[k].Shvy+= Avg[l].Shvy;
        Avg[k].Spins+=Avg[l].Spins;
/*      Avg[k].Live_spins+=Avg[l].Live_spins; */
        if (l==2)
        {
                Avg[k].Live_spins+=Avg[l].Live_spins;
                Avg[k].He_spins+=Avg[l].He_spins;
                Avg[k].H_spins+=Avg[l].H_spins;
        }
        Avg[k].Sum1_5+=Avg[l].Sum1_5;
        Avg[k].Sum6_12+=Avg[l].Sum6_12;
        Avg[k].Sum13_15+=Avg[l].Sum13_15;
        Avg[k].Tint[1]=Avg[l].Tint[1];
        Avg[k].Maj[1]=Avg[l].Maj[1];
        if (!Avg[k].Tint[0])
            Avg[k].Tint[0]=Avg[l].Tint[0];
        if (!Avg[k].Maj[0])
            Avg[k].Maj[0]=Avg[l].Maj[0];
        if (Thrstate>Avg[k].Maxthr)
            Avg[k].Maxthr=Thrstate;

}
int IsQuiet()   /* Generates k=2 (6 hr) 1.7-3.7 MeV/amu 4He flux and tests
Qcrit */ {
        double flxcon;
        if ( Avg[2].She>.1 && Avg[2].He_spins>.1)
        {
            flxcon=Avg[2].Sum6_12/(SpinPer*Avg[2].He_spins*Avg[2].She*2.0*17.);
            Fquiet[0]=(Avg[2].SWbox[39]+Avg[2].SWbox[40]+Avg[2].SWbox[41]
                        +Avg[2].SWbox[38]
                        +Avg[2].SWbox[29] /* 3He */  );

            if (Fquiet[0]<0.)
                Fquiet[0]=0.;
            else
            {
                Fquiet[1]=sqrt(Fquiet[0])*flxcon;
                Fquiet[0]*=flxcon;
            }
        }
        else
                Fquiet[0]=Fquiet[1]=0.;

        /*  Base on 2.5-3.2 MeV/n O, box 83 (change O to C, box 68,
                                             no effect) */
         if ( Avg[2].Shvy>.1 && Avg[2].Live_spins>.1)
        {
         flxcon=Avg[2].Sum13_15/(Avg[2].Shvy*SpinPer*Avg[2].Live_spins*17.*.7);
            FquietO[0]=Avg[2].SWbox[83];
            if (FquietO[0]<0.)
                FquietO[0]=0.;
            else
            {
                FquietO[1]=sqrt(FquietO[0])*flxcon;
                FquietO[0]*=flxcon;
            }
        }
        else
                FquietO[0]=FquietO[1]=0.;
        if (Fquiet[0]<QcritHe && FquietO[0]<QcritO)
                return 1;
        return 0;
}

OutSpectHdr(int k)
{
        double hrs;
        if (!Sfi[k])
                return;
        fprintf(Sfi[k],"ASCII\r\n");
        hrs=AvgInt*Avgcnt[k]/3600.;
        if (hrs < 24. && k<7)
            fprintf(Sfi[k]," %5.2lf-hr Avg LEMT Spectra\r\n",hrs);
        else if (k<7)
            fprintf(Sfi[k]," %5.2lf-day Avg LEMT Spectra\r\n",hrs/24.);
        else if (k==7)
            fprintf(Sfi[k],"Quiet time spectrum\r\n");
        else if (k==8)
            fprintf(Sfi[k],"Low-Fe Spectrum (no solar events)\r\n");
        else if (k>8)
            fprintf(Sfi[k]," %5.2lf-day Avg Quiet-time LEMT Spectra\r\n",hrs/24.);
//        sprintime(pan[cw].Ax_lim[0].l[0],Tbuf);
        fprintf(Sfi[k]," %s\r\n",Starttime);
//        sprintime(pan[cw].Ax_lim[0].l[1],Tbuf);
        fprintf(Sfi[k]," %s\r\n",Endtime);
/*      sprintime((long)(AvgInt*Avgcnt[k]),Tbuf);
        fprintf(Sfi[k]," 0  %s\r\n",Tbuf+10);   */
        fprintf(Sfi[k],"#2  Rate\r\n");
        fprintf(Sfi[k],"T2                      Time\r\n");
        fprintf(Sfi[k],"E2      1       50      Energy MeV/amu\r\n");
        fprintf(Sfi[k],"F2   9.9e-10    2e3     Particles/(cm2 sr s MeV/amu)\r\n");
        fprintf(Sfi[k],"F1      1       10      Species ID\r\n");
        fprintf(Sfi[k],"F1      0       60      A\r\n");
        fprintf(Sfi[k],"S4      1        5      Species Name\r\n");
        fprintf(Sfi[k],"F1      1        2      Source \r\n");
        fprintf(Sfi[k],"#End\r\n");
}
OutSpect(int k)
{
        double flxcon1, flxcon, flx, flxx, err;
        char tbuf2[30];
        int i, kp;
        double valid3;
        double diff1, diff2, corr[4];
        double ee0;

        if (Avg[k].Live_spins<.1)
                return;
        if (!*Sna[k])
                return;
        if (k<0 || k>=NAVG)
        {
                printf("\n (OutSpect) Illegal k=%2d\n",k);
                fprintf(Efi,"\n (OutSpect) Illegal k=%2d\n",k);
                file_close(F_in[0]);
                exit(1);
        }
        if (Sfi[k])
        {
/*
                printf("\n Spectrum[%2d] %20.20s already open\n",k,Sna[k]);
                fprintf(Efi,"\n Spectrum[%2d] %20.20s already open\n",k,Sna[k]);
*/
        }
        else if ((Sfi[k]=fopen(Sna[k],"ab"))==0)
        {
                puts("\n (OutSpect) Cannot open file ");
                puts(Sna[k]);
                file_close(F_in[0]);
                exit(1);
        }
        pan[cw].Dout=1;
        sprintime(Avg[k].Tint[0],Tbuf);
        sprintime(Avg[k].Tint[1],tbuf2);
/*      printf("\n H_spins=%7.0lf a=%7.0f  Ape=%7.0f \n",
                Avg[k].H_spins,Avg[k].Spind[0],Avg[k].ApeSpins[0]);
*/
        if (Avg[k].Sprot>.1 && Avg[k].H_spins>1)    /* LEMT Protons */
        {
            fprintf(Sfi[k],";LEMT H Sum1-5=%10.4lg,  Sprot=%10.4lg,  Spins=%10.4lg  H_spins=%10.4lg \r\n",
                Avg[k].Sum1_5,Avg[k].Sprot,Avg[k].Spins,Avg[k].H_spins);
            flxcon1=Avg[k].Sum1_5/(Avg[k].Sprot*SpinPer*Avg[k].H_spins);
            flxcon=0.1*flxcon1/(.35*Hgeom[3]);
            flx=flxcon*Avg[k].SWbox[0];
            err=flxcon*sqrt( (double) Avg[k].SWbox[0]);
            if (Fullspec)
            fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0    0   Bkg0 \r\n",
                    Tbuf,tbuf2,En[3].E0,En[3].E1,flx,err);
            if (Fullspec)
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -     \r\n",
                    Tbuf,tbuf2);
            for (i=0; i<3; i++)
            {
                flxcon=flxcon1/((En[i+3].E1-En[i+3].E0)*Hgeom[i+3]);
                flx=flxcon*Avg[k].SWbox[i+5];
                err=flxcon*sqrt( (double) Avg[k].SWbox[i+5]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  1     1  H    \r\n",
                        Tbuf,tbuf2,En[i+3].E0,En[i+3].E1,flx,err);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0     0  -     \r\n",
                    Tbuf,tbuf2);
        }
        if (Fullspec && Avg[k].ApeSpins[0]>.1)      /* Ape A */
        {
            fprintf(Sfi[k],";APE A  Sum=       ,  Smatr=       ,  Spins=%10.4lg \r\n",
                                          Avg[k].ApeSpins[0]);

            flxcon1=1.            /(           SpinPer*Avg[k].ApeSpins[0]*1.24);
            for (i=2; i<9; i++)         /* Ape A  H */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  1.3    1  H      0\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0 \r\n",
                    Tbuf,tbuf2);
            for (i=17; i<24; i++)         /* Ape A  He4 */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 3.3    4  He4     0\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0\r\n",
                    Tbuf,tbuf2);
            for (i=26; i<30; i++)         /* Ape A  C   */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 4.3    12  C      0   %3ld\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err,Avg[k].ApeAbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
            for (i=30; i<35; i++)         /* Ape A  N   */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 9.3    14  N      0   %3ld\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err,Avg[k].ApeAbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
            for (i=36; i<40; i++)         /* Ape A  O   */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 5.3    16  O      0   %3ld\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err,Avg[k].ApeAbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
            for (i=42; i<44; i++)         /* Ape A  Ne  */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 6.3    20  Ne     0   %3ld\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err,Avg[k].ApeAbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
            for (i=44; i<47; i++)         /* Ape A  Fe  */
            {
                flxcon=flxcon1/(EnA[i].E1-EnA[i].E0);
                flx=flxcon*Avg[k].ApeAbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeAbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 8.3    56  Fe     0   %3ld\r\n",
                        Tbuf,tbuf2,EnA[i].E0,EnA[i].E1,flx,err,Avg[k].ApeAbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
        }
        if (Fullspec && Avg[k].ApeSpins[1]>.1)    /* Ape B */
        {
            fprintf(Sfi[k],";APE B  Sum=       ,  Smatr=       ,  Spins=%10.4lg \r\n",
                                          Avg[k].ApeSpins[1]);
            flxcon1=1.            /(           SpinPer*Avg[k].ApeSpins[1]*1.33);
            for (i=2; i<10; i++)         /* Ape B  H */
            {
                flxcon=flxcon1/(EnB[i].E1-EnB[i].E0);
                flx=flxcon*Avg[k].ApeBbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeBbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  1.5    1  H      0\r\n",
                        Tbuf,tbuf2,EnB[i].E0,EnB[i].E1,flx,err);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0 \r\n",
                    Tbuf,tbuf2);
            for (i=19; i<27; i++)         /* Ape B  He4 */
            {
                flxcon=flxcon1/(EnB[i].E1-EnB[i].E0);
                flx=flxcon*Avg[k].ApeBbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeBbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 3.5    4  He4     0\r\n",
                        Tbuf,tbuf2,EnB[i].E0,EnB[i].E1,flx,err);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0 \r\n",
                    Tbuf,tbuf2);
            for (i=29; i<32; i++)         /* Ape B  C   */
            {
                flxcon=flxcon1/(EnB[i].E1-EnB[i].E0);
                flx=flxcon*Avg[k].ApeBbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeBbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 4.5    12  C      0   %3ld\r\n",
                        Tbuf,tbuf2,EnB[i].E0,EnB[i].E1,flx,err,Avg[k].ApeBbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
            for (i=32; i<37; i++)         /* Ape B  N   */
            {
                flxcon=flxcon1/(EnB[i].E1-EnB[i].E0);
                flx=flxcon*Avg[k].ApeBbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeBbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 9.5    14  N      0   %3ld\r\n",
                        Tbuf,tbuf2,EnB[i].E0,EnB[i].E1,flx,err,Avg[k].ApeBbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
            for (i=38; i<43; i++)         /* Ape B  O   */
            {
                flxcon=flxcon1/(EnB[i].E1-EnB[i].E0);
                flx=flxcon*Avg[k].ApeBbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeBbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 5.5    16  O      0   %3ld\r\n",
                        Tbuf,tbuf2,EnB[i].E0,EnB[i].E1,flx,err,Avg[k].ApeBbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0    0\r\n",
                    Tbuf,tbuf2);
            for (i=44; i<47; i++)         /* Ape B  Fe  */
            {
                flxcon=flxcon1/(EnB[i].E1-EnB[i].E0);
                flx=flxcon*Avg[k].ApeBbox[i];
                err=flxcon*sqrt( (double) Avg[k].ApeBbox[i]);
                fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg 8.5    56  Fe     0   %3ld\r\n",
                        Tbuf,tbuf2,EnB[i].E0,EnB[i].E1,flx,err,Avg[k].ApeBbox[i]);
            }
            fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0     0\r\n",
                    Tbuf,tbuf2);
        }
        if (Avg[k].She>.1 && Avg[k].He_spins>1)      /* LEMT He */
        {
            fprintf(Sfi[k],";LEMT He Sum6-12=%10.4lg,  She=%10.4lg,  Spins=%10.4lg  He_spins=%10.4lg \r\n",
                Avg[k].Sum6_12,Avg[k].She,Avg[k].Spins,Avg[k].He_spins);
              flxcon1=Avg[k].Sum6_12/(Avg[k].She*SpinPer*Avg[k].He_spins*17.);
              flxcon=flxcon1/(En[3].E1-En[3].E0);
              flx=flxcon*Avg[k].SWbox[25];
              err=flxcon*sqrt( (double) Avg[k].SWbox[25]);
            if (Fullspec)
              fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0     0  b25       0\r\n",
                      Tbuf,tbuf2,En[3].E0,En[3].E1,flx,err);
              flxcon=flxcon1/(En[6].E1-En[4].E0);
              flx=flxcon*Avg[k].SWbox[26];
              err=flxcon*sqrt( (double) Avg[k].SWbox[26]);
            if (Fullspec)
              fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0     0  b26       0\r\n",
                      Tbuf,tbuf2,En[4].E0,En[6].E1,flx,err);
            if (Fullspec)
              fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -     0\r\n",
                      Tbuf,tbuf2);
              valid3=0.2;
              corr[0]=Avg[k].SWbox[25];
              diff1=max(Avg[k].SWbox[28]-corr[0],0.);
              diff2=max(Avg[k].SWbox[38]-corr[0],0.);
              if (diff1 > 0.1*diff2 && diff1 > corr[0])
                      valid3=2.;
              corr[1]=.4*Avg[k].SWbox[26];
              corr[2]=.3*Avg[k].SWbox[26];
              corr[3]=.3*Avg[k].SWbox[26];
              for (i=0; i<9; i++)             /* 3He */
              {
                  if (!Fullspec)
                        continue;
                  flxcon=flxcon1/(En[i+3].E1-En[i+3].E0);
                  diff1=Avg[k].SWbox[i+28];
                  if (i<4 && k>1 && Fquiet[0]>0.)
                      diff1=max(diff1-corr[i],0.);
                  flx=flxcon*diff1;
                  if (i<4 && diff1<corr[i] && k>1 && Fquiet[0]>0.)
                      err=flx;
                  else
                      err=flxcon*sqrt(diff1);
                  fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg %3.1lf    3   3He    0\r\n",
                          Tbuf,tbuf2,En[i+3].E0,En[i+3].E1,flx,err,valid3);
              }
              fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -       0\r\n",
                      Tbuf,tbuf2);
              for (i=0; i<8; i++)             /* 4He */
              {
                      /* Note bin 11 (46), 10-14 MeV/n has serious efficiency problem
                         because of high dome thresholds. The 7 -10 bin is corrected
                         by a factor of 1.8 */
                  flxcon=flxcon1/(EnHe[i+3].E1-EnHe[i+3].E0);
                  if (i==7)
                      flxcon*=1.8;
                  diff1=Avg[k].SWbox[i+38];
                  if (i<4 && k>1 && Fquiet[0]>0.)
                      diff1=max(diff1-corr[i],0.);
                  flx=flxcon*diff1;
                  if (i<4 && diff1<corr[i] && k>1 && Fquiet[0]>0.)
                      err=flx;
                  else
                      err=flxcon*sqrt(diff1);
                  fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  3     4  4He     0\r\n",
                          Tbuf,tbuf2,EnHe[i+3].E0,EnHe[i+3].E1,flx,err);
              }
              fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0\r\n",
                      Tbuf,tbuf2);
/* PH spectral data  */
            if (Avg[k].PHwin6_12 > .1)
            {
                  fprintf(Sfi[k],";  PH: PHwin6-12=%8.4lg  \r\n",Avg[k].PHwin6_12);
                  flxcon1=Avg[k].Sum6_12/(Avg[k].PHwin6_12*SpinPer*Avg[k].He_spins*17.);
                  for (kp=4; kp<6; kp++)  /* 3He & 4He */
                  {
                      if (!Fullspec && kp==4)
                            continue;
                      if (!spec[kp].resp)
                              continue;
                      for (i=3; i<12; i++)
                      {
                          flxcon=flxcon1/(Ebin[i+1]-Ebin[i]);
                          flx=flxcon*Avg[k].PHbox[kp][i];
                          err=flxcon*sqrt( (double) Avg[k].PHbox[kp][i]);
                          fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  %2hd  %2.0lf %3.3s      2  %3ld\r\n",
                              Tbuf,tbuf2,Ebin[i],Ebin[i+1],flx,err,spec[kp].resp,spec[kp].a,spec[kp].name,Avg[k].PHbox[kp][i]);
                      }
                      fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                          Tbuf,tbuf2);
                  }
             }
        }
        if (Avg[k].Shvy<.1)
        {
                fclose(Sfi[k]);
                Sfi[k]=0;
                return;
        }
        fprintf(Sfi[k],";LEMT Hvy Sum13-15=%10.4lg,  Shvy=%10.4lg,  Spins=%10.4lg  Live_spins=%10.4lg \r\n",
                Avg[k].Sum13_15,Avg[k].Shvy,Avg[k].Spins,Avg[k].Live_spins);
        flxcon1=Avg[k].Sum13_15/(Avg[k].Shvy*SpinPer*Avg[k].Live_spins*17.);
        flxcon=0.1*flxcon1/(En[5].E1-En[5].E0);
        flx=flxcon*Avg[k].SWbox[1];
        err=flxcon*sqrt( (double) Avg[k].SWbox[1]);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0     0  Bkg1     0\r\n",
                Tbuf,tbuf2,En[5].E0,En[5].E1,flx,err);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -     0 \r\n",
                Tbuf,tbuf2);
        flxcon=flxcon1/(En[7].E1-En[5].E0);
        flx=flxcon*Avg[k].SWbox[59];
        err=flxcon*sqrt( (double) Avg[k].SWbox[59]);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0     0  b59       0  %3ld\r\n",
                Tbuf,tbuf2,En[5].E0,En[7].E1,flx,err,Avg[k].SWbox[59]);
        flxcon=flxcon1/(En[12].E1-En[8].E0);
        flx=flxcon*Avg[k].SWbox[60];
        err=flxcon*sqrt( (double) Avg[k].SWbox[60]);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0     0  b60       0   %3ld\r\n",
                Tbuf,tbuf2,En[8].E0,En[12].E1,flx,err,Avg[k].SWbox[59]);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -     0     0\r\n",
                Tbuf,tbuf2);
        flxcon=flxcon1/(En[7].E1-En[5].E0);
        diff1=Avg[k].SWbox[62]-Avg[k].SWbox[59];
        diff1=max(diff1,0.);
        flx=flxcon*diff1;
        err=flxcon*sqrt(diff1);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0    11  B11        0 %3ld\r\n",
                Tbuf,tbuf2,En[5].E0,En[7].E1,flx,err,Avg[k].SWbox[62]);
        flxcon=flxcon1/(En[12].E1-En[8].E0);
        diff1=Avg[k].SWbox[63]-Avg[k].SWbox[60];
        diff1=max(diff1,0.);
        flx=flxcon*diff1;
        err=flxcon*sqrt(diff1);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  0    11  B11        0 %3ld\r\n",
                Tbuf,tbuf2,En[8].E0,En[12].E1,flx,err,Avg[k].SWbox[63]);
            if (Fullspec)
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -      0    0\r\n",
                Tbuf,tbuf2);
        corr[0]=.9*Avg[k].SWbox[59];
        corr[1]=.4*Avg[k].SWbox[59];
        corr[2]=.3*Avg[k].SWbox[59];
        corr[3]=.3*Avg[k].SWbox[60];
        for (i=0; i<8; i++)             /* C */
        {
            flxcon=flxcon1/(EnC[i+5].E1-EnC[i+5].E0);
            diff1=Avg[k].SWbox[i+68];
            if (i<4)
                diff1=max(diff1-corr[i],0.);
            flx=flxcon*diff1;
            if (i<4 && diff1<corr[i])
                err=flx;
            else
                err=flxcon*sqrt(diff1);
            fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  4    12  C        0  %3ld \r\n",
                    Tbuf,tbuf2,EnC[i+5].E0,EnC[i+5].E1,flx,err,Avg[k].SWbox[i+68]);
        }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -       0  0\r\n",
                Tbuf,tbuf2);
        {                       /* N */
            flx=flxcon1*Avg[k].SWbox[80]/5.0;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[80])/5.0;
            fprintf(Sfi[k],"2   %s %s      5.0   10.0  %10.4lg %10.4lg   9    14  N         0 %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[80]);
            flx=flxcon1*Avg[k].SWbox[81]/20.;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[81])/20.;
            fprintf(Sfi[k],"2   %s %s     10.0   30.0  %10.4lg %10.4lg   9    14  N         0 %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[81]);
         }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -       0  0\r\n",
                Tbuf,tbuf2);
        for (i=1; i<10; i++)             /* O */
        {
            ee0=En[i+4].E0;
            if (i==0)              /* Bin 4 has efficiency problems */
                ee0=2.23;
            flxcon=flxcon1/(En[i+4].E1-ee0);
            if (i==9)
                flxcon*=1.5;   /* Efficiency */
            flx=flxcon*Avg[k].SWbox[i+82];
            err=flxcon*sqrt( (double) Avg[k].SWbox[i+82]);
            fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  5    16  O         0  %3ld\r\n",
                    Tbuf,tbuf2,ee0,En[i+4].E1,flx,err,Avg[k].SWbox[i+82]);
        }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
        for (i=0; i<8; i++)             /* Ne */
        {
            flxcon=flxcon1/(EnNe[i+6].E1-EnNe[i+6].E0);
            if (i==7)
                flxcon*=1.5;   /* Efficiency */
            flx=flxcon*Avg[k].SWbox[i+96];
            err=flxcon*sqrt( (double) Avg[k].SWbox[i+96]);
            fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  6    20  Ne        0  %3ld\r\n",
                    Tbuf,tbuf2,EnNe[i+6].E0,EnNe[i+6].E1,flx,err,Avg[k].SWbox[i+96]);
        }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
        {                       /* Mg */
            flx=flxcon1*Avg[k].SWbox[107]/1.8;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[107])/1.8;
            fprintf(Sfi[k],"2   %s %s      2.1    5.0  %10.4lg %10.4lg  10    24  Mg         0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[107]);
            flx=flxcon1*Avg[k].SWbox[108]/5.0;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[108])/5.0;
            fprintf(Sfi[k],"2   %s %s      5.0   10.0  %10.4lg %10.4lg  10    24  Mg          0 %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[108]);
            flx=flxcon1*Avg[k].SWbox[109]/20.;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[109])/20.;
            fprintf(Sfi[k],"2   %s %s     10.0   30.0  %10.4lg %10.4lg  10    24  Mg          0 %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[109]);
         }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
        for (i=0; i<10; i++)             /* Si */
        {
            flxcon=flxcon1/(EnSi[i+5].E1-EnSi[i+5].E0);
            flx=flxcon*Avg[k].SWbox[i+113];
            err=flxcon*sqrt( (double) Avg[k].SWbox[i+113]);
            fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  7    28  Si        0 %3ld\r\n",
                Tbuf,tbuf2,EnSi[i+5].E0,EnSi[i+5].E1,flx,err,Avg[k].SWbox[i+113]);
        }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
        {                       /* S  */
            flx=flxcon1*Avg[k].SWbox[124]/1.8;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[124])/1.8;
            fprintf(Sfi[k],"2   %s %s      3.2    5.0  %10.4lg %10.4lg 11    32  S          0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[124]);
            flx=flxcon1*Avg[k].SWbox[125]/5.0;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[125])/5.0;
            fprintf(Sfi[k],"2   %s %s      5.0   10.0  %10.4lg %10.4lg 11    32  S          0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[125]);
            flx=flxcon1*Avg[k].SWbox[126]/40.;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[126])/40.;
            fprintf(Sfi[k],"2   %s %s     10.0   50.0  %10.4lg %10.4lg 11    32  S           0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[126]);
         }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
        {                       /* Ar */
            flx=flxcon1*Avg[k].SWbox[128]/1.8;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[128])/1.8;
            fprintf(Sfi[k],"2   %s %s      3.2    5.0  %10.4lg %10.4lg 12    36  Ar         0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[128]);
            flx=flxcon1*Avg[k].SWbox[129]/5.0;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[129])/5.0;
            fprintf(Sfi[k],"2   %s %s      5.0   10.0  %10.4lg %10.4lg 12    36  Ar         0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[129]);
            flx=flxcon1*Avg[k].SWbox[130]/40.;
            err=flxcon1*sqrt( (double) Avg[k].SWbox[130])/40.;
            fprintf(Sfi[k],"2   %s %s     10.0   50.0  %10.4lg %10.4lg 12    36  Ar         0  %3ld\r\n",
                    Tbuf,tbuf2,flx,err,Avg[k].SWbox[130]);
         }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
        for (i=0; i<10; i++)             /* Fe */
        {
            flxcon=flxcon1/(EnFe[i+5].E1-EnFe[i+5].E0);
            flx=flxcon*Avg[k].SWbox[i+135];
            err=flxcon*sqrt( (double) Avg[k].SWbox[i+135]);
            fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg  8    56  Fe        0  %3ld\r\n",
                    Tbuf,tbuf2,EnFe[i+5].E0,EnFe[i+5].E1,flx,err,Avg[k].SWbox[i+135]);
         }
        fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                Tbuf,tbuf2);
/* PH spectral data  */

        if (Avg[k].PHwina[1][15] > .1)
        {
            if (Avg[k].W15 < 0.1)  /* Not calc by OutFlx */
                Avg[k].W15=Avg[k].WinA[0][15]+Avg[k].WinA[1][15]+Avg[k].WinA[2][15];
            fprintf(Sfi[k],";  PH: Win15=%8.4lg  PHWin15=%6d \r\n",Avg[k].W15,Avg[k].PHwina[1][15]);
            flxcon1=Avg[k].W15/(Avg[k].PHwina[1][15]*SpinPer*Avg[k].Live_spins*17.);
            for (kp=7; kp<NPART; kp++)        /***xxx*****/
            {
                if (!spec[kp].resp)
                        continue;
                for (i=5; i<15; i++)
                {
                    flxcon=flxcon1/(Ebin[i+1]-Ebin[i]);
                    flxx=Avg[k].PHbox[kp][i];
                    if (spec[kp].a==12.)
                        flxx=Avg[k].PHbox[kp][i]-Avg[k].PHbox[kp-1][i];
                    if (spec[kp].a==14.)
                        flxx=Avg[k].PHbox[kp][i]-Avg[k].PHbox[kp-2][i];
                    if (flxx<0.)
                        flxx=0.;
                    flx=flxcon*flxx;
                    err=flxcon*sqrt( (double) Avg[k].PHbox[kp][i]);
                    fprintf(Sfi[k],"2   %s %s    %5.2lf  %5.2lf  %10.4lg %10.4lg %2hd    %2.0lf  %3.3s      1  %3ld %4.1lf\r\n",
                        Tbuf,tbuf2,Ebin[i],Ebin[i+1],flx,err,spec[kp].resp,spec[kp].a,spec[kp].name,Avg[k].PHbox[kp][i],flxx);
                }
                fprintf(Sfi[k],"2   %s %s      -1      -1          -1        -1   0      0  -        0  0\r\n",
                    Tbuf,tbuf2);
            }
            if (k==6 || k==7 || k==8 || k==12)
            {
                fprintf(Sfi[k],";    ");
                for (i=5; i<15; i++)
                {
                    fprintf(Sfi[k],"%8.2lf",Ebin[i]);
                }
                fprintf(Sfi[k],"\r\n");
                for (kp=7; kp<NPART; kp++)
                {
                    fprintf(Sfi[k],";%4s",spec[kp].name);
                    for (i=5; i<15; i++)
                    {
                        fprintf(Sfi[k],"%8ld",Avg[k].PHbox[kp][i]);
                    }
                    fprintf(Sfi[k],"\r\n");
                }
                fprintf(Sfi[k],";    ");
                for (i=5; i<15; i++)
                {
                    fprintf(Sfi[k],"%8.2lf",Ebin[i]);
                }
                fprintf(Sfi[k],"\r\n");
                for (kp=0; kp<NFINEZ; kp++)
                {
                    double z;
                    z=0.2*kp+5.;
                    fprintf(Sfi[k],";%4.1lf",z);
                    for (i=0; i<8; i++)
                    {
                        fprintf(Sfi[k],"%8ld",Avg[k].PHzbox[kp][i]);
                    }
                    fprintf(Sfi[k],"\r\n");
                }
            }
        }
/*
        fclose(Sfi[k]);
        Sfi[k]=0;
*/
}

OutPha()  /* Write (append to) Pha file & directory */
{
        short vnum[1];
        long chap[2];
        int i;

        if (!*Phna)
                return;
        if (!Npha)
                return;
        if (Phfi==0)
        {
            if ((Phfi=fopen(Phna,"ab"))==0)
            {
                    puts("Cannot open file ");
                    puts(Phna);
                    exit(1);
            }
        }
        if (*Dna[0] )
        {
            if (Dirfi[0]==0)
            {
                if ((Dirfi[0]=fopen(Dna[0],"ab"))==0)
                {
                        puts("\n(OutPha) Cannot open file ");
                        puts(Dna[0]);
                        puts(_strerror(NULL));
                        printf("Net open-close=%d Npha=%d\n",Noutdir, Npha);
                        for (i=0; i<NAVG; i++)
                            if (*Fna[i] && Ffi[i])
                                printf("  %15.15s  %8X\n",Fna[i], Ffi[i]);
                        for (i=0; i<NAVG; i++)
                            if (*Rna[i] && Rfi[i])
                                printf("  %15.15s  %8X\n",Rna[i], Rfi[i]);
                        for (i=0; i<NAVG; i++)
                            if (*Sna[i] && Sfi[i])
                                printf("  %15.15s  %8X\n",Sna[i], Sfi[i]);
                        printf("fdir=%8X  %15.15s\n",fdir[0],F_name[0]);
                        file_close(Phfi);
                        exit(1);
                }
            }
        }
        Noutdir++;
        fseek(Phfi,0L,SEEK_END); /* position for ftell */
        chap[1]=ftell(Phfi);
        chap[0]=Avg[0].Tint[0];
        *vnum=0;
        fwrite(vnum,2,1,Phfi);
        fwrite(Avg[0].Tint,4,2,Phfi);

        *vnum=0x002a;
        fwrite(vnum,2,1,Phfi);
        Phabuf[0]=10*Npha;
        fwrite(Phabuf,5*Npha+1,2,Phfi);

/*
        sprintime(Avg[0].Tint[0],Tbuf);
        printf("0  %18s ",Tbuf);
        sprintime(Avg[0].Tint[1],Tbuf);
        printf(" %18s ",Tbuf);
        printf("  %4d ",Npha);
        printf("  %8x \n",chap[1]);
*/
/*
        fclose(Phfi);
        Phfi=0;
*/
        if (*Dna[0])
        {
            fwrite(chap,4,2,Dirfi[0]);
            Nchapt[0]++;
/*
            fclose(Dirfi[0]);
            Dirfi[0]=0;
*/
            Noutdir--;
        }
        if (Ipush<NPUSH)
        {
                Pchap[Ipush][0]=chap[0];
                Pchap[Ipush][1]=chap[1];
                Ipush++;
        }
        Npha=0;
        Ipha=1;
}
OutQuiet()      /* Append to quiet pha directory */
{

        if (!*Dna[1] || !*Phna)
                return;
        if (!Ipush)
                return;         /* no data */
        if (Dirfi[1]==0)
        {
            if ((Dirfi[1]=fopen(Dna[1],"ab"))==0)
            {
                    puts("(OutQuiet) Cannot open file ");
                    puts(Dna[1]);
                    exit(1);
            }
        }
        fwrite(Pchap,2*Ipush,4,Dirfi[1]);
        Nchapt[1]+=Ipush;
/*
        fclose(Dirfi[1]);
        Dirfi[1]=0;
*/
}
QList()
{
        int isq;

        if (Qfi==0)
        {
            if ((Qfi=fopen(Qna,"a"))==0)    /* Quiet time list */
            {
                    puts("Cannot open file ");
                    puts(Qna);
                    exit(1);
            }
        }
        sprintime(Avg[2].Tint[0],Tbuf);
        fprintf(Qfi,"0  %18s ",Tbuf);
        sprintime(Avg[2].Tint[1],Tbuf);
        fprintf(Qfi," %18s ",Tbuf);
        if (Fquiet[0]<QcritHe)
                isq=1;
        else
                isq=0;
        fprintf(Qfi," %10.3lg %10.3lg %4d %4d\n"
                ,Fquiet[0],Fquiet[1],isq,LowFeVal);
/*
         fclose(Qfi);
         Qfi=0;
*/
}
OutFe()      /* Append to Fe-quiet pha directory */
{
        if (!*Dna[2] || !*Phna)
                return;
        if (!Ipush)
                return;         /* no data */
        if (Dirfi[2]==0)
        {
            if ((Dirfi[2]=fopen(Dna[2],"ab"))==0)
            {
                    puts("(OutFe) Cannot open file ");
                    puts(Dna[2]);
                    exit(1);
            }
        }
        fwrite(Pchap,2*Ipush,4,Dirfi[2]);
        Nchapt[2]+=Ipush;
/*
        fclose(Dirfi[2]);
        Dirfi[2]=0;
*/
}
CloseDirs()
{
        int i;
        for (i=0; i<3; i++)
        {
                if (!*Dna[i] || !*Phna)
                        continue;
                if (Dirfi[i])
                    fclose(Dirfi[i]);
                if ((Dirfi[i]=fopen(Dna[i],"r+b"))==0) /* reopen in r+b mode */
                {
                        puts("(CloseDirs) Cannot open file ");
                        puts(Dna[i]);
                        exit(1);
                }
                puts("\nCloseDirs ");
                puts(Dna[i]);
                fseek(Dirfi[i],Dirpos[i],SEEK_SET);
                fprintf(Dirfi[i]," %10ld %10ld\r\n",0L,Nchapt[i]);
                fclose(Dirfi[i]);
                Dirfi[i]=0;
        }
        fclose(Efi);
        Efi=0;
          _strtime(tim);                   /* current time */
          printf("\nTime= %s\n",tim);
        printf("\nClose\n");
}
int LowFe()
{
        int i;
        long low;
        LowFeVal=0;
#if 0
        for (i=0; i<NEVENT; i++)
        {
                if (Avg[2].Tint[1] > EventTime[i][0]
                    && Avg[2].Tint[0] < EventTime[i][1] )
                {
                        return 0;  /* time during a known SEP period */
                }
        }
#endif
        low=(Avg[2].SWbox[135]+Avg[2].SWbox[136]+Avg[2].SWbox[137]  /* Fe */
            +Avg[2].SWbox[113]+Avg[2].SWbox[114]+Avg[2].SWbox[115]  /* Si */
            +Avg[2].SWbox[107]);                                    /* Mg */
        LowFeVal=low;
        if (low< 4)    /* was 16 */
                return 1;
        return 0;
}
int n27d;
NewTimeBin(long time)
{
        long tbin;

        if (Firstt)                     /* 15 min */
        {
            Sum_flux();
            OutFlx(0);
            OutPha();
            sprintime(Avg[0].Tint[0],Tbuf);
            if (errors > maxerrs)
                maxerrs=errors;
/*            printf("%s    errors=%2ld  max=%3ld \r",Tbuf,errors,maxerrs); */
        }
        tbin=time/(Avgcnt[1]*AvgInt);
        if ( Close || tbin != TimeBin[1] )
        {                              /*  1 hr */
            if (Inbelts )
                printf("%s belts \r",Tbuf);
            else
                printf("%s       \r",Tbuf);
            if (Avg[1].Tint[0])
            {
                OutFlx(1);
            }
            Clear_avg(1);
            TimeBin[1]=tbin;
        }
        tbin=time/(Avgcnt[2]*AvgInt);
        if ( Close || tbin != TimeBin[2] )
        {                              /*  6 hr */
            if (Avg[2].Tint[0])
            {
                if (LowFe())  /* Low Fe */
                {
                        if (IsQuiet())  /* Quiet time */
                        {
                                OutQuiet();
                                Sum_each(7,2);
                                Sum_each(9,2);
                                Sum_each(10,2);
                                Sum_each(11,2);
                                Sum_each(12,2);
                        }
                        if (Fquiet[0]<FeCrit) /* even Fe-free should be quiet */
                        {
                                QList();
                                OutFe();
                                Sum_each(8,2);
                        }
                }
                Ipush=0;
                OutFlx(2);
                OutSpect(2);
            }
            Clear_avg(2);
            TimeBin[2]=tbin;
        }
        tbin=time/(Avgcnt[3]*AvgInt);
        if ( Close || tbin != TimeBin[3] )
        {                              /*  1 da */
            if (Avg[3].Tint[0])
            {
                OutFlx(3);
                OutSpect(3);
            }
            Clear_avg(3);
            if (Avg[9].Tint[0])
            {
                OutFlx(9);
                OutSpect(9);
            }
            Clear_avg(9);
            TimeBin[3]=tbin;
        }
        tbin=(time-172800L)/(Avgcnt[4]*AvgInt);
        if ( Close || tbin != TimeBin[4] )
        {                              /* 6.75 da */
            if (Avg[4].Tint[0])
            {
                OutFlx(4);
                OutSpect(4);
            }
            Clear_avg(4);
            if (Avg[10].Tint[0])
            {
                OutFlx(10);
                OutSpect(10);
            }
            Clear_avg(10);
            TimeBin[4]=tbin;
        }
        tbin=(time-172800L)/(Avgcnt[5]*AvgInt);
        if ( Close || tbin != TimeBin[5] )
        {                              /*  27 da */
            if (Avg[5].Tint[0])
            {
                OutFlx(5);
                OutSpect(5);
            }
            Clear_avg(5);
            if (Avg[11].Tint[0])
            {
                OutFlx(11);
                OutSpect(11);
            }
            Clear_avg(11);
            n27d++;
            if ((n27d%4)==0 || Close)      /* 6 "mo" or 108 d */
            {
                if (Avg[12].Tint[0])
                {
                    OutFlx(12);
                    OutSpect(12);
                }
                Clear_avg(12);
            }
            TimeBin[5]=tbin;
        }
        if ( Close )                  /* summed */
        {
            if (Avg[6].Tint[0])    /* Summed */
            {
             /* OutSpect(6)  */ ;
            }
            if (Avg[7].Tint[0])    /* Quiet times */
            {
                OutSpect(7);
            }
            if (Avg[8].Tint[0])    /* Fe-Quiet times */
            {
                OutSpect(8);
            }
            CloseDirs();
        }
        TimeBin[0]=time/AvgInt;
        Clear_avg(0);
}

compare(double *x)
{
        double  min, max;
        min=x[0];
        max=x[1];
        if (min > max)
        {
                min=x[1];
                max=x[0];
        }
        if (x[2] > max)
        {
                max=x[2];
        }
        else if (x[2] < min)
        {
                min=x[2];
        }
/*      if (min >= 10. && max > 70.*min)
        if (min >= 10. && max > 500.*min)
        {
                fprintf(Efi,"Compare Error  A=%8.0lf  B=%8.0lf  C=%8.0lf\n",
                    x[0],x[1],x[2]);
                return 0;
        }
        if (min < 10. && max > 60.)
                return 0;
*/
        return 1;
}
