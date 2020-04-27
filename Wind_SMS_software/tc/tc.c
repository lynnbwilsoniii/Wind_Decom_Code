/* ---------------------------------------------------------------------------
   tc.c --- time converter
   (c) 1997-1999 Simon Hefti (hefti@umich.edu)
   $Id: tc.c,v 1.1.1.1 2000/04/04 15:53:18 jraines Exp $
   ------------------------------------------------------------------------ */

/* Idea:
** there are two ways of storing dates:
** (1) by writing down all components of the "human readable" date
**     like 1-Jan-1999:12:15, or
** (2) by storing the seconds since a well-defined starting point.
**     (Other apllicatins may actually store miliseconds. Assuming
**      that man measures time since 4000 years, or 2^47 miliseconds,
**      we see that 64 bits to store miliseconds is more than enough.)
**     tc uses 1-Jan-1970, 00:00 as the starting point.
**     Thus, 1-Jan-1999 has time 915148800.
** tc converts between format (1) and (2), while using format (2)
** internally. All conversions are therefore processed as follows:
** a) convert input to format (2)
** b) report input in format (1)
*/

/* Compilation (use copy and paste):

--- on HP ---

loadable lib for IDL:

cc +z -Aa -c tc.c
ld -b -o tc.sl tc.o -lm

normal executable:
cc -Aa -o tc tc.c -lm

--- on pooh (Linux) ---

gcc -o tc tc.c -lm

--- on SUN (Bern) ---

cc -K pic -DSPARC -G -c tc.c
cc -G -DSOLARIS -o tc.so tc.o

s70 = long(systime(1))
aa  = lonarr(14)

tst = call_external('tc.so','sec70_timeifu',aa,s70)
print, aa,s70


*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>


#define IGREG2 2299161L
#define IGREG1 (15L + 31L*(10L + 12L * 1582L))


#define SEC70POS 0
#define JULDYPOS 1
#define SECDYPOS 2
#define DOY__POS 3
#define DAY__POS 4
#define MONTHPOS 5
#define YEAR_POS 6
#define HOUR_POS 7
#define MIN__POS 8
#define SEC__POS 9
#define WDAY_POS 10
#define DUM1_POS 11
#define DUM2_POS 12
#define DUM3_POS 13

#define SECOFDAY 86400L

/* long g_alltime[14]; */

void sec70_time(long alltime[14], long *sec70);
int  sec70_timeifu(int argc, void *argv[]);
void caldat(long *julian, long *mm, long *id, long *iyyy);
void julday(long *mm, long *id, long *iyyy,long *julday);
void doy_time(long alltime[14], long *doy, long *year);
void date_time(long alltime[14], long *mday, long *mon, long *year);
int  doy_timeifu(int argc, void *argv[]);


/* ---------------------------------------------------------------------------
   main --- Interface to command line (no arguments result in help)
   ------------------------------------------------------------------------ */
int main(int argc, char *argv[])
{
  long sec70 = 0L;
  long alltime[14];
  int i = 0;
  
  long doy = 0L, year = 0L;

  long ltmp1 = 0L;
  long ltmp2 = 0L;
  long ltmp3 = 0L;
  
  static char rcsid[] =\
  "$Id: tc.c,v 1.1.1.1 2000/04/04 15:53:18 jraines Exp $";

  for(i = 0;i < 14; i++) alltime[i] = 0L;
    
  /* -------------------------------------------------------------------------
  ** check command line
  ** ---------------------------------------------------------------------- */
  
  if( (argc != 2) && (argc != 3) && (argc != 4) && (argc != 7))
  {
    printf("%s\n",rcsid);
    printf("Usage: tc time\n");
    printf("one argument --> (time) in sec since 1-1-1970:00:00\n");
    printf(" 2 arguments --> (time) as 'year doy'\n");
    printf(" 3 arguments --> (time) as 'day-month-year'\n");
    printf(" 6 arguments --> (time) as 'day month year hr min sec'\n");
    printf("\n");
    printf("Or use it as a callable function (eg from IDL)\n");
  }
    
  if( argc == 2 )
  {
    sec70 = atol(argv[1]);
  
    sec70_time(alltime,&sec70);
  
    for(i = 0;i < 14; i++) printf("%ld ",alltime[i]);
    printf("\n");
  }
    
  if( argc == 3 )
  {
    year = atol(argv[1]);
    doy  = atol(argv[2]);

    doy_time(alltime,&doy,&year);
  
    for(i = 0;i < 14; i++) printf("%ld ",alltime[i]);
    printf("\n");
  }

  if( argc == 4 )
  {
    ltmp1 = atol(argv[1]);
    ltmp2 = atol(argv[2]);
    ltmp3 = atol(argv[3]);

    date_time(alltime,&ltmp1,&ltmp2,&ltmp3);
  
    for(i = 0;i < 14; i++) printf("%ld ",alltime[i]);
    
    printf("\n");
  }

  if( argc == 7 )
  {
    ltmp1 = atol(argv[1]);
    ltmp2 = atol(argv[2]);
    ltmp3 = atol(argv[3]);

    date_time(alltime,&ltmp1,&ltmp2,&ltmp3);
  
    
    alltime[0] += atol(argv[4]) * 3600L;
    alltime[0] += atol(argv[5]) *   60L;
    alltime[0] += atol(argv[6]);
    
    for(i = 0;i < 14; i++) printf("%ld ",alltime[i]);
    printf("\n");
  }

  return(0);
}
  

/* ---------------------------------------------------------------------------
   date_time --- convert "normal" date
   ------------------------------------------------------------------------ */
void date_time(long alltime[14], long *mday, long *mon, long *year)
{
  
  long ltmp1 = 0L;
  long ltmp2 = 0L;
  long ltmp3 = 0L;
  
  long sec70;

  long jd70 = 0L;  /* julday of 1-Jan-1970 */
  long djul = 0L;  /* julday of this day */

  ltmp1 = 1L;
  ltmp2 = 1L;
  ltmp3 = 1970L;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&jd70);

  ltmp1 = *mon;
  ltmp2 = *mday;
  ltmp3 = *year;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&djul);
  
  sec70 = (djul - jd70) * SECOFDAY;
  
  sec70_time(alltime,&sec70);
  
  return;
}

/* ---------------------------------------------------------------------------
   date_timeifu --- Interface to date_time for SUN.
   ------------------------------------------------------------------------ */
int date_timeifu(int argc, void *argv[])
{
  long *alltime;
  long *mday;
  long *mon;   
  long *year;
   
  if( argc != 4 ) {
    printf("E:date_timeifu: Only %d (instead of 4) arguments given.\n",
      argc);
    return(-1);
  }
  
  alltime = (long *) argv[0];
  mday    = (long *) argv[1];
  mon     = (long *) argv[2];
  year    = (long *) argv[3];
  
  date_time(alltime,mday,mon,year);
  
  argv[0] = alltime;
  
  return(0);
}



/* ---------------------------------------------------------------------------
   doy_time --- convert year/doy
   ------------------------------------------------------------------------ */
void doy_time(long alltime[14], long *doy, long *year)
{
  
  long ltmp1 = 0L;
  long ltmp2 = 0L;
  long ltmp3 = 0L;
  
  long sec70;

  long jd70 = 0L;  /* julday of 1-Jan-1970 */
  long jdyr = 0L;  /* julday of 1-Jan-of_current_year */

  ltmp1 = 1L;
  ltmp2 = 1L;
  ltmp3 = 1970L;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&jd70);

  ltmp1 = 1L;
  ltmp2 = 1L;
  ltmp3 = *year;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&jdyr);
  
  sec70 = (jdyr - jd70 + *doy - 1L) * SECOFDAY;
  
  sec70_time(alltime,&sec70);
  
  return;
}

/* ---------------------------------------------------------------------------
   doy_timeifu --- Interface to doy_time for SUN.
   ------------------------------------------------------------------------ */
int doy_timeifu(int argc, void *argv[])
{
  long *alltime;
  long *doy;
  long *year;
  
  if( argc != 3 ) {

    printf("E:doy_timeifu: Only %d (instead of 3) arguments given.\n",
      argc);
    return(-1);
  }
  
  alltime = (long *) argv[0];
  doy     = (long *) argv[1];
  year    = (long *) argv[2];
  
  doy_time(alltime,doy,year);
  
  argv[0] = alltime;
  
  return(0);
}

/* ---------------------------------------------------------------------------
   sec70_time --- convert seconds since 1-1-1970:00:00
   ------------------------------------------------------------------------ */
void sec70_time(long alltime[14], long *sec70)
{
  long d70 = 0L;    /* days since 1-Jan-1970 */
  long secday = 0L; /* sec of day 0..86400-1 */
  long djul = 0L;   /* julian day number */
  
  long hr = 0L;  /* hour of day */
  long min = 0L; /* min of hour */
  long sec = 0L; /* sec of min */
  
  long year = 0L;
  long month = 0L; /* month of year (1..12) */
  long day = 0L;   /* day of month (1..31) */
  
  long doy = 0L;  /* day of year (1..366) */
  long wday = 0L; /* day of week (0..6)->(Sun..Sat) */
  
  long ltmp1 = 0L;
  long ltmp2 = 0L;
  long ltmp3 = 0L;
  long ltmp4 = 0L;
  
  long jd70 = 0L;  /* julday of 1-Jan-1970 */
  long jdyr = 0L;  /* julday of 1-Jan-of_current_year */
  
  d70    = *sec70 / SECOFDAY;
  secday = *sec70 % SECOFDAY;
  
  ltmp1 = 1L;
  ltmp2 = 1L;
  ltmp3 = 1970L;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&jd70);
  
  djul = d70 + jd70;
  
  hr  = secday / 3600L;
  min = (secday % 3600L) / 60L;
  sec = secday - hr * 3600L - min * 60L;
  
  caldat(&djul,&ltmp1,&ltmp2,&ltmp3);
  
  year  = ltmp3;
  month = ltmp1;
  day   = ltmp2;
  
  ltmp1 = month;
  ltmp2 = day;
  ltmp3 = year;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&ltmp4);
  
  ltmp1 = 1L;
  ltmp2 = 1L;
  ltmp3 = year;
  
  julday(&ltmp1,&ltmp2,&ltmp3,&jdyr);

  doy = ltmp4 - jdyr + 1L;
  
  wday = (djul + 1L) % 7;
  
  alltime[SEC70POS] = *sec70;
  alltime[JULDYPOS] = djul;
  alltime[SECDYPOS] = secday;
  alltime[DOY__POS] = doy;
  alltime[DAY__POS] = day;
  alltime[MONTHPOS] = month;
  alltime[YEAR_POS] = year;
  alltime[HOUR_POS] = hr;
  alltime[MIN__POS] = min;
  alltime[SEC__POS] = sec;
  alltime[WDAY_POS] = wday;
  alltime[DUM1_POS] = 0L;
  alltime[DUM2_POS] = 0L;
  alltime[DUM3_POS] = 0L;
  
  return;
}

/* ---------------------------------------------------------------------------
   sec70_timeifu --- Interface to sec70_time for SUN.
   ------------------------------------------------------------------------ */
int sec70_timeifu(int argc, void *argv[])
{
  long *alltime;
  long *sec70;
  
  if( argc != 2 ) {

    printf("Err (sec70_timeifu): Only %d (instead of 2) arguments given.\n",
      argc);
    return(-1);
  }
  
  alltime = (long *) argv[0];
  sec70   = (long *) argv[1];
  
  /* printf("DBG (sec70_timeifu): sec70 = %ld\n",*sec70); */
  
  sec70_time(alltime,sec70);
  
  argv[0] = alltime;
  
  return(0);
}

/* ---------------------------------------------------------------------------
   julday --- convert date to julian day number (after Numerical Recipies)
   ------------------------------------------------------------------------ */
void julday(long *mm, long *id, long *iyyy, long *julday)
{
  long jul = 0L;
  long ja = 0L;
  long jy = *iyyy;
  long jm = 0L;

  if (jy == 0L)
  {
    printf("War: year %ld encountered. Returning 0\n",jy);
    *julday = 0;
    return;
  }
  

  if (jy < 0L) ++jy;

  if (*mm > 2L)
  {
    jm = *mm + 1L;
  }
  else
  {
    --jy;
    jm = *mm + 13L;
  }
  
  jul = (long) (floor(365.25*jy) + floor(30.6001*jm) + *id + 1720995L);
  if( *id + 31L * (*mm + 12L*(*iyyy) ) >= IGREG1)
  {
    ja   = (long)(0.01*jy);
    jul += 2L - ja + (long)(0.25 * ja);
  }
  
  *julday = jul;
  return;
}



/* ---------------------------------------------------------------------------
   caldat --- convert julian day number to date (after Numerical Recipies)
   ------------------------------------------------------------------------ */
void caldat(long *julian, long *mm, long *id, long *iyyy)
{
  long ja = 0L,jalpha = 0L,jb = 0L,jc = 0L,jd = 0L,je = 0L;

  if (*julian >= IGREG2)
  {
    jalpha = (long)(((float) (*julian-1867216L)-0.25)/36524.25);
    ja = *julian+1+jalpha-(long) (0.25*jalpha);
  }
  else
  {
    ja=*julian;
  }

  jb = ja + 1524L;
  jc = (long)(6680.0+((float) (jb-2439870L)-122.1)/365.25);
  jd = (long)(365*jc+(0.25*jc));
  je = (long)((jb-jd)/30.6001);
  *id = jb - jd -(long)(30.6001 * je);
  *mm = je - 1L;

  if (*mm > 12L) *mm -= 12L;

  *iyyy = jc - 4715L;

  if (*mm > 2L) --(*iyyy);
  if (*iyyy <= 0L) --(*iyyy);
}



/* ---------------------------------------------------------------------------
** history
**
**  7-Dec-96 hef first version. Test:
**               heffalump:~/scr/timfunc $ ~/scr/fmttime.pl -t
**               TEST MODE
**               today is: 849971652.
**               result: 849971652  7-Dec-96:15:14:12 (342)
**               TEST done
**               heffalump:~/scr/timfunc $ tc
**               0 849971652
**               1 2450425
**               2 54852
**               3 342
**               4 7
**               5 12
**               6 1996
**               7 15
**               8 14
**               9 12
**               10 6
**               11 0
**               12 0
**               13 0
**  7-Dec-96 hef doy_time added
**               now works with IDL:
**  IDL> at = lonarr(14)
**  IDL> sec70 = 849971652L
**  IDL> status = call_external('tc','sec70_time',at,sec70,$
**  IDL>   default='disk$scratch:[shefti]tc.exe')
**  IDL> print,at
**  849971652     2450425       54852         342           7          12
**  1996          15          14          12           6           0
**  0           0
**  7-Dec-96 hef date_time added
**               considered productive.
** heffalump:~/scr/timfunc $ tc
** 849971652 2450425 54852 342 7 12 1996 15 14 12 6 0 0 0
** 849916800 2450425 0 342 7 12 1996 0 0 0 6 0 0 0
** 849916800 2450425 0 342 7 12 1996 0 0 0 6 0 0 0
**               linking on VMS:
cc tc.c
link tc.obj,sys$input/opt/share/exe=tc.exe
symbol_vector=(sec70_time=PROCEDURE,doy_time=PROCEDURE,date_time=PROCEDURE)
** ------------------------------------------------------------------------ */

/* ---------------------------------------------------------------------------
** $Id: tc.c,v 1.1.1.1 2000/04/04 15:53:18 jraines Exp $
** ------------------------------------------------------------------------ */
