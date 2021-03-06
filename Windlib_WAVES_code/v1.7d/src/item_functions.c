/* item_functions.c - this WIND/WAVES GSE module contains functions that
   are indirectly called through the Item Database
*/

#include <stdio.h>
#include "wind_os_def.h"
/*#include "wind_tm_event_def.h"*/
#include "item.h"

#if SYSTEM == SUNOS
#define wind_get_rad_lsmode w_get_rad_lsmode_list_
#define fort_dbms_to_ur8 dbms_to_ur8_
#define fort_wind_tm_version wind_lib_version_c_
#define f_get_scet_in_context w_get_scet_in_context_
#endif

#ifdef USE_MACOSX
#define wind_get_rad_lsmode w_get_rad_lsmode_list_
#define fort_dbms_to_ur8 dbms_to_ur8_
#define fort_wind_tm_version wind_lib_version_c_
#define f_get_scet_in_context w_get_scet_in_context
#endif

#if SYSTEM == VMS
#define wind_get_rad_lsmode w_get_rad_lsmode_list
#define fort_dbms_to_ur8 dbms_to_ur8
#define fort_wind_tm_version wind_lib_version_c
#define f_get_scet_in_context w_get_scet_in_context
#endif

extern int fort_dbms_to_ur8();
extern int fort_wind_tm_version();
extern int _wind_get_rad_lsmode();
#ifdef USE_MACOSX
extern int w_really_get_item_();
#else
extern int w_really_get_item();
#endif
/*
extern int f_get_scet_in_context();
*/

struct rad_list_mode_extract_blk {
     int xlate_table;
     int freq_table;
     int steps;
     int sum_loop;
     int group_loop;
     int group_size;
     int xlat_mask;
     int auto_mask;
     int sum_flag;
     int radio;
     int list_type;
};
#define RAD_CH_LIST 18
#define RAD_HZ_LIST 19
#define RAD_TGL_LIST 20

struct int_or_float {
   union {
      int i;
      float f;
   } u;
};

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*
  Returns an array of RAD[1,2] fixed tune channels (old name frequencies).
  Expects an item defined in the item database like:
	FUNCTION=20
	ARG=1			! 1 for rad1, 2 for rad2
	ARG=FIRST_FREQ_NUM
	ARG=FREQUENCY_STEP
	ARG=ACTUAL_MEAS_CT
	ARG=EVENT_STATE
*/
int rad_fixtune_freqs( ch, event_type, a, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   float  a[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="RAD_FIXTUNE_FREQS";
   int  i, j, k;
   int  first_freq, freq_step, n_freq, event_state;
   int  radio;

   j = size / 4;
   *ret_type = FLOAT_RETURN;

   radio      = aux->u.ival[0];
   aux = aux->next;
   first_freq = aux->u.ival[0];
   aux = aux->next;
   freq_step  = aux->u.ival[0];
   aux = aux->next;
   n_freq     = aux->u.ival[0];
   aux = aux->next;
   event_state= aux->u.ival[0];

   switch(radio)
   {
   case 1:
      for (i=0; i<n_freq && i<j; i++, ++*ret_size)
      {
         k = (first_freq + (freq_step*i)) % 256;
         a[i] = 20000.0 + (k * 4000.0);
      }
      break;
   case 2:
      for (i=0; i<n_freq && i<j; i++, ++*ret_size)
      {
         k = (first_freq + (freq_step*i)) % 256;
         a[i] = 1075000.0 + (k * 50000.0);
      }
      break;
   default:
      printf("%s: item db arg #1 must be 1 or 2 (rad1/rad2).\n", rn);
      return 0;
      break;
   }

   return 1;
}
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*
  Returns an array of RAD[1,2] fixed tune frequencies.
  Expects an item defined in the item database like:
	FUNCTION=0,FIRST_FREQ_NUM,FREQUENCY_STEP,ACTUAL_MEAS_CT,EVENT_STATE
*/
int rad_fixtune_chans( ch, event_type, a, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   int  a[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   int  i, j;
   int  first_freq, freq_step, n_freq, event_state;

   j = size / 4;
   *ret_type = INT_RETURN;

   first_freq = aux->u.ival[0];
   aux = aux->next;
   freq_step  = aux->u.ival[0];
   aux = aux->next;
   n_freq     = aux->u.ival[0];
   aux = aux->next;
   event_state= aux->u.ival[0];

   for (i=0; i<n_freq && i<j; i++, ++*ret_size)
   {
      a[i] = (first_freq + (freq_step*i)) % 256;
   }

   return 1;
}

/*----------------------------------------------------------------------------*/
/*
  Returns an array of RAD[1,2] list mode frequencies.
  Calls wind_get_rad_lsmode to actually make the list.
  Expects an item defined in the item database like:
	FUNCTION=1
	ARG=2			! 1=RAD1, 2=RAD2
	ARG=XLATE_TABLE
	ARG=FREQ_TABLE
	ARG=STEPS
	ARG=SUM_LOOP
	ARG=GROUP_LOOP
	ARG=GROUP_SIZE
	ARG=XLAT_MASK
	ARG=POINTER_FILE_NAME
	ARG=FREQUENCY_FILE_NAME
*/
int rad_list_mode_freqs( ch, event_type, a, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   int  a[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   struct rad_list_mode_extract_blk rlmeb;
   int ok;
   char *pfile;		/* pointer file name */
   char *cfile;		/* channel or frequency file name */

   size = size / 4;
   *ret_type = FLOAT_RETURN;

   rlmeb.radio        = aux->u.ival[0];
   aux = aux->next;
   rlmeb.xlate_table  = aux->u.ival[0];
   aux = aux->next;
   rlmeb.freq_table   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.steps        = aux->u.ival[0];
   aux = aux->next;
   rlmeb.sum_loop     = aux->u.ival[0];
   aux = aux->next;
   rlmeb.group_loop   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.group_size   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.xlat_mask    = aux->u.ival[0];
   rlmeb.sum_flag     = 0;
   rlmeb.auto_mask    = 0;
   rlmeb.list_type    = RAD_HZ_LIST;

   aux = aux->next;
   pfile = aux->u.pc;
   aux = aux->next;
   cfile = aux->u.pc;

#ifdef USE_MACOSX
   ok = wind_get_rad_lsmode(&rlmeb,pfile,cfile,a,&size,ret_size);
#else
   ok = _wind_get_rad_lsmode(&rlmeb,pfile,cfile,a,&size,ret_size);
#endif

   return ok;
}

/*----------------------------------------------------------------------------*/
/*
  Returns an array of RAD[1,2] list mode channel numbers.
  Calls wind_get_rad_lsmode to actually make the list.
  Expects an item defined in the item database like:
	FUNCTION=5
	ARG=2			! 1=RAD1, 2=RAD2
	ARG=XLATE_TABLE
	ARG=FREQ_TABLE
	ARG=STEPS
	ARG=SUM_LOOP
	ARG=GROUP_LOOP
	ARG=GROUP_SIZE
	ARG=XLAT_MASK
	ARG=POINTER_FILE_NAME
	ARG=FREQUENCY_FILE_NAME
*/
int rad_list_mode_channels( ch, event_type, a, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   int  a[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   struct rad_list_mode_extract_blk rlmeb;
   int ok;
   char *pfile;		/* pointer file name */
   char *cfile;		/* channel or frequency file name */

   size = size / 4;
   *ret_type = INT_RETURN;

   rlmeb.radio        = aux->u.ival[0];
   aux = aux->next;
   rlmeb.xlate_table  = aux->u.ival[0];
   aux = aux->next;
   rlmeb.freq_table   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.steps        = aux->u.ival[0];
   aux = aux->next;
   rlmeb.sum_loop     = aux->u.ival[0];
   aux = aux->next;
   rlmeb.group_loop   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.group_size   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.xlat_mask    = aux->u.ival[0];
   rlmeb.sum_flag   = 0;
   rlmeb.auto_mask  = 0;
   rlmeb.list_type    = RAD_CH_LIST;

   aux = aux->next;
   pfile = aux->u.pc;
   aux = aux->next;
   cfile = aux->u.pc;

#ifdef USE_MACOSX
   ok = wind_get_rad_lsmode(&rlmeb,pfile,cfile,a,&size,ret_size);
#else
   ok = _wind_get_rad_lsmode(&rlmeb,pfile,cfile,a,&size,ret_size);
#endif

   return ok;
}

/*----------------------------------------------------------------------------*/
/*
  Returns an array of RAD[1,2] list mode toggle states.
  Calls wind_get_rad_toggle_list_ to actually make the list.
  Expects an item defined in the item database like:
	FUNCTION=2,XLATE_TABLE,SUM_FLAG,STEPS,SUM_LOOP,GROUP_LOOP,GROUP_SIZE,AUTO_MASK
*/
int rad_list_toggle_states( ch, event_type, a, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   int  a[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   struct rad_list_mode_extract_blk rlmeb;
   int ok;
   char *pfile;		/* pointer file name */
   char *cfile;		/* channel or frequency file name */

   size = size / 4;
   *ret_type = INT_RETURN;

   rlmeb.radio        = aux->u.ival[0];
   aux = aux->next;
   rlmeb.xlate_table  = aux->u.ival[0];
   aux = aux->next;
   rlmeb.sum_flag     = aux->u.ival[0];
   aux = aux->next;
   rlmeb.steps        = aux->u.ival[0];
   aux = aux->next;
   rlmeb.sum_loop     = aux->u.ival[0];
   aux = aux->next;
   rlmeb.group_loop   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.group_size   = aux->u.ival[0];
   aux = aux->next;
   rlmeb.auto_mask    = aux->u.ival[0];
   rlmeb.freq_table   = 0;
   rlmeb.xlat_mask    = 0;
   rlmeb.list_type    = RAD_TGL_LIST;

   aux = aux->next;
   pfile = aux->u.pc;

#ifdef USE_MACOSX
   ok = wind_get_rad_lsmode(&rlmeb,pfile,cfile,a,&size,ret_size);
#else
   ok = _wind_get_rad_lsmode(&rlmeb,pfile,cfile,a,&size,ret_size);
#endif

   return ok;
}

/*----------------------------------------------------------------------------*/
/*
   Converts dbms format time scet passed in argument list blocks to
   Ulysses real*8 format time.
*/
int ur8_event_scet ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   double  t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="UR8_EVENT_SCET";
   int i,j,k;
   double scet;
   int ok;

   *ret_size = 0;
   /* next test attempts to guard against alignment errors */
   if (*ret_type != DOUBLE_RETURN) goto real8_buf_err;

   if (aux->data_type != INT_RETURN) goto i4_arg_err;
   i = aux->u.ival[0]; /* event scet yyyymmdd */
   j = aux->u.ival[1]; /* event scet hhmmss */
   aux = aux->next;
   k = aux->u.ival[0]; /* event scet fraction */

   ok = fort_dbms_to_ur8(&i,&j,&k,&scet);
   if (ok != 1) goto scet_get_err;
/*
printf("%s: dbms time is %d %d %d --> %f.\n", rn, i,j,k,scet);
*/

   *ret_size = 1;
   t[0] = scet;

   return 1;

   i4_arg_err:
   printf("%s: Error, internal scet not integer.\n", rn);
   return 0;

   real8_buf_err:
   printf("%s: Error, time buffer must be real*8.\n", rn);
   return 0;

   scet_get_err:
   printf("%s: cannot get ur8 scet from %d %d %d.\n", rn, i,j,k);
   return 0;
}

/*----------------------------------------------------------------------------*/
/*
   Calls w_get_scet_in_context to get a stream position in context,
   that is, if the w_event was called more recently than w_channel_position
   then the current event's scet is returned.  Otherwise the stream position
   most recently established by w_channel_position is returned.
*/
int ur8_context_scet ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   double  t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="UR8_CONTEXT_SCET";
   int i,j,k;
   double scet;
   int ok;
   int context=3;
   int fch;

   *ret_size = 0;
   fch = ch + 1;
   /* next test attempts to guard against alignment errors */
   if (*ret_type != DOUBLE_RETURN) goto real8_buf_err;

   *t = 3.0;
/*
   ok = f_get_scet_in_context(&fch,&context,&scet);
   if (ok != 1) goto scet_get_err;
*/
/*
*/
   *ret_size = 1;
   t[0] = scet;

   return 1;

   real8_buf_err:
   printf("%s: Error, time buffer must be real*8.\n", rn);
   return 0;

   scet_get_err:
   printf("%s: cannot get ur8 scet in context.\n", rn);
   return 0;
}

/*----------------------------------------------------------------------------*/
/*
   Universal SpaceCraft Event Time list generator for all observations of
   all WAVES subexperiments.  (An observation is one measurement).
   In addition to the SCET of the frame pulse contained in the first
   two auxilliary arguments there are six additional arguments describing
   the temporal distribution of the observational times to be returned.

     The 6 arguments are as follows:

            a = time between EVENT_TIME and the start of the first
                          measurement
            b = time between starts of measurements within a set of
                     measurements. (Can be 0 if there is only 1 measurement
                     in the set)

            c = number of measurements in the set (may be 1)

            d = time between start of one set of measurements and the next.

            e = integration time for one measurement (msecs)

            f = Number by which the return-size must be divided to
                     decide how many sets are in the entire event.
                (...not used...same as arg c, Nov 1994...)


09-DEC-1994: arguments a,b,d,e now in fractional seconds instead of
milliseconds. JK
  OLD:   msec_in_day = 60.0 * 60.0 * 24.0 * 1000.0;

*/
int obs_scet_gen ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   double  t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="OBS_SCET_GEN";
   int i,j,k,m,n;
   int c, f;
   double da, db, dd, de;
   double di, dj;
   double scet;
   double sec_in_day;
   double temp;
   double x,y,z;
   int ok;

   *ret_size = 0;
   if (*ret_type != DOUBLE_RETURN) goto real8_buf_err;
   sec_in_day = 60.0 * 60.0 * 24.0;

   n = size/8; /* get the real*8 size of the buffer */
   i = aux->u.ival[0]; /* event scet yyyymmdd */
   j = aux->u.ival[1]; /* event scet hhmmss */
   aux = aux->next;
   k = aux->u.ival[0]; /* event scet fraction */
   
   ok = fort_dbms_to_ur8(&i,&j,&k,&scet);
   if (ok != 1) goto scet_get_err;

/*
printf("%s: scet args are %d %d %d.\n", rn, i,j,k);
*/

   /* get the spacing arguments from the auxilliary argument list */
   i = 0;
   aux = aux->next;
   while (aux != NULL && i < 6)
   {
      switch (aux->data_type)
      {
      case INT_RETURN:
         temp = (double) aux->u.ival[0];
         break;
      case FLOAT_RETURN:
         temp = (double) aux->u.fval[0];
         break;
      case DOUBLE_RETURN:
         temp = aux->u.dval;
         break;
      default:
         printf("%s: Argument %d, %s is not int, r8, or r4.\n", rn, i, aux->pc);
         return 0;
         break;
      }
      /* convert the millisecond time arguments to fractional days in
         Ulysses time format
      */
      switch(i)
      {
      case 0:  da = temp / sec_in_day;  break;
      case 1:  db = temp / sec_in_day;  break;
      case 2:
         if (aux->data_type == INT_RETURN)
            c  = f = aux->u.ival[0];
         else
         {
            c = (int) temp;
            printf("%s: measurement number, %s, not integer in DB.\n",
               rn, aux->pc);
         }
         break;
      case 3:  dd = temp / sec_in_day;  break;
      case 4:  de = temp / sec_in_day;  break;
      case 5:  f = aux->u.ival[0];      break;
      default: break;
      }
      i++;
      aux = aux->next;
   }
   if (aux != NULL) goto aux_arg_count_err;
   if (c <= 0) goto set_size_err;

/*
printf("a=%lf b=%lf c=%d d=%lf e=%lf f=%d.\n", da, db, c, dd, de, f);
*/

   /* loop and calculate per Cathie's equation */
   de = de / 2.0;	/* use midpoint of integration time */
   x = scet + da + de;  /* time of midpoint of first measurement */
   for (i=1, y=x, m=0; i <= (n / f); i++, y = y + dd)
      for (j=1, z=y; j <= c && m < n; z = z + db, j++, m++)
         t[m]=z;

   *ret_size = m;
   if (m == 0) goto suspect_arg_err;
   return 1;

   real8_buf_err:
   printf("%s: Error: time buffer must be real*8.\n", rn);
   return 0;

   scet_get_err:
   printf("%s: cannot get ur8 scet for %d %d %d.\n", i,j,k);
   return 0;

   aux_arg_count_err:
   printf("%s: wrong number of arguments: %d.\n", rn, i+2);
   return 0;

   set_size_err:
   printf("%s: set size (arg 'c') must be greater than zero.\n", rn);
   return 0;

   suspect_arg_err:
   printf("%s: arguments result in no measurement times:\n", rn);
   printf("\tCaller's buffer size: %d\n", n);
   printf("\tdT0  arg a (seconds): %lE\n", da);
   printf("\tdT   arg b (seconds): %lE\n", db);
   printf("\tN    arg c          : %d\n", c);
   printf("\tdTs  arg d (seconds): %lE\n", dd);
   printf("\tTi   arg e (seconds): %lE\n", de);
   return 0;
}

/*----------------------------------------------------------------------------*/
/*
   Evaluates a polynomial expression over a domain to yeild a range.
   (Originally intended for temperature calibrations).
   Auxilliary Arguments:
   ---------------------
   1.  domain (x values)
   2.  c0 - first coefficient, for x**0
   3.  c1 - 2nd coeeficient, for x**1
   4.  c2 - 3rd coefficient, for x**2
   ...
   8.  c6 - 7th coefficient, for x**(6), max allowed, fewer ok.
*/
#define MAX_COEFF 7
int r4_polynomial_eval ( ch, event_type, a, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   struct int_or_float  a[];	/* user's storage area			*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="R4_POLYNOMIAL_EVAL";
   int i, j;
   int n_coeff;
   float c0, c1, c2, c3, c4, c5, c6;
   float x;
   int my_size=0;
   int ok;
   int bit_start=0;
   int ret_type2;
   int size2;
   struct same_name_item *psni2;

   /* get the domain, which may be an array, named in the first aux arg */
   ret_type2 = INT_RETURN;
   size2 = size / 4;
#ifdef USE_MACOSX
   ok = w_really_get_item_(ch, event_type, aux->pc, 
#else
   ok = w_really_get_item(ch, event_type, aux->pc, 
#endif
        a, size2, &my_size, &bit_start, &ret_type2, &psni2);
   if (ok != 1) 
   {
      printf("%s: cannot get domain item \"%s\".\n", rn, aux->pc);
      return 0;
   }

   /* make sure the domain is integer */
   switch (ret_type2)
   {
   case INT_RETURN:
      break;
   default:
      printf ("%s: domain must be integer.\n", rn);
      return 0;
      break;
   }

   /* get the coefficients, converting data types as necessary */
   i = 0;
   n_coeff = 0;
   aux = aux->next;
   c0 = c1 = c2 = c3 = c4 = c5 = c6 = 0.0;
   while (aux != NULL && i < (MAX_COEFF))
   {
      switch (aux->data_type)
      {
      case INT_RETURN:
         x = aux->u.ival[0];
         break;
      case FLOAT_RETURN:
         x = aux->u.fval[0];
         break;
      case DOUBLE_RETURN:
         x = aux->u.dval;
         break;
      default:
         printf("Argument %d is unknown: 0x%X.\n", i, aux->u.ival[0]);
         return 0;
         break;
      }
      switch (i)
      {
      case 0: c0 = x; break;
      case 1: c1 = x; break;
      case 2: c2 = x; break;
      case 3: c3 = x; break;
      case 4: c4 = x; break;
      case 5: c5 = x; break;
      case 6: c6 = x; break;
      default:
         printf("%s: coefficient count messed up.\n", rn);
         break;
      }
      i++;
      aux = aux->next;
   }
   if (aux != NULL)
   {
      printf("%s: too many coefficients, no more than %d allowed.\n",
         rn, MAX_COEFF);
      return 0;
   }

/*
printf("%s: coeff are: %g %g %g %g %g %g %g\n", rn, c0,c1,c2,c3,c4,c5,c6);
*/

   /* calculate the range */
   n_coeff = i;
   j = (size2 < my_size) ? size2 : my_size;
   for (i=0, x=a[i].u.i; i<j; i++, x=a[i].u.i)
      a[i].u.f = c0 + x *(c1 + x *(c2 + x * (c3 +x *(c4 +x *(c5 + (x * c6))))));
   *ret_size = j;

   return 1;
}
#undef MAX_COEFF
#undef MAX_ELEMENTS

/*----------------------------------------------------------------------------*/
/*
   Data type and size test function
*/
int jk_test_10_func_i4 ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   int  t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="JK_TEST_10_FUNC_I4";
   int i;

   printf("%s: type=%X, byte size=%d.\n", rn, *ret_type, size);
   for (*ret_size=0, i=1; *ret_size<(size/4); ++*ret_size, ++i) t[*ret_size]=i;

   return 1;
}

/*----------------------------------------------------------------------------*/
/*
   Data type and size test function
*/
int jk_test_10_func_r4 ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   float  t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="JK_TEST_10_FUNC_R4";
   float f;

   printf("%s: type=%X, byte size=%d.\n", rn, *ret_type, size);
   for (*ret_size=0, f=1.0; *ret_size<(size/4); ++*ret_size, ++f)
       t[*ret_size]=f;

   return 1;
}

/*----------------------------------------------------------------------------*/
/*
   Data type and size test function
*/
int jk_test_10_func_r8 ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   double  t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="JK_TEST_10_FUNC_R8";
   double f;

   printf("%s: type=%X, byte size=%d.\n", rn, *ret_type, size);
   for (*ret_size=0, f=1.0; *ret_size<(size/4); ++*ret_size, ++f)
       t[*ret_size]=f;

   return 1;
}

/*----------------------------------------------------------------------------*/
/*
   Gets the current wind_lib version number
*/
int get_wind_lib_version ( ch, event_type, t, size, ret_size, ret_type, aux )
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   char t[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area		*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   struct function_arg_list  *aux; /* list of auxilliary arguments	*/
{
   char *rn="GET_WIND_LIB_VERSION";
   int ok;

   *ret_size = size;
   ok = fort_wind_tm_version(t, ret_size);

   return 1;
}



#define NUM_ITEM_FUNCTIONS 32

/* an array of pointers to functions returning int */
static int  (*f[NUM_ITEM_FUNCTIONS])()
={
  rad_fixtune_chans, 		/* 0  */
  rad_list_mode_freqs, 		/* 1  */
  rad_list_toggle_states, 	/* 2  */
  ur8_event_scet,		/* 3  */
  get_wind_lib_version,		/* 4  */
  rad_list_mode_channels,	/* 5  */
  obs_scet_gen,			/* 6  */
  r4_polynomial_eval,		/* 7  */
  ur8_context_scet,		/* 8  */
  NULL,				/* 9  */
  jk_test_10_func_i4,		/* 10 */
  jk_test_10_func_r4,		/* 11 */
  jk_test_10_func_r8,		/* 12 */
  NULL,				/* 13 */
  NULL,				/* 14 */
  NULL,				/* 15 */
  NULL,				/* 16 */
  NULL,				/* 17 */
  NULL,				/* 18 */
  NULL,				/* 19 */
  rad_fixtune_freqs,		/* 20 */
  NULL,				/* 21 */
  NULL,              		/* 22 */
  NULL,				/* 23 */
  NULL,				/* 24 */
  NULL,				/* 25 */
  NULL,				/* 26 */
  NULL,				/* 27 */
  NULL,				/* 28 */
  NULL,				/* 29 */
  NULL,				/* 30 */
  NULL,				/* 31 */
};

/*
  This is the branch routine called by w_really_get_item to invoke
  a function item from the wind/waves item database.
*/
int call_function_item( 
   fnum, ch, event_type, a, size, ret_size, ret_type, auxargs )
   int  fnum;		/* database defined function number		*/
   int  ch;		/* user's channel number			*/
   char *event_type;	/* character string event type			*/
   int  a[];		/* user's storage area				*/
   int  size;		/* byte size of user's storage area (buffer)	*/
   int  *ret_size;	/* # of data type elements written to buffer	*/
   int  *ret_type;	/* data type of [buffer on entry, item on ret]	*/
   int  *auxargs;	/* auxilliary arguments used by some functions	*/
{
   char *rn="CALL_FUNCTION_ITEM";

   if (fnum >= NUM_ITEM_FUNCTIONS || fnum < 0)
   {
      printf("%s: function number %d is not between 0 and %d.\n",
             rn, fnum, (NUM_ITEM_FUNCTIONS - 1) );
      return 0;
   }
   else
      if (f[fnum] == NULL)
      {
         printf("%s: function number %d is not defined.\n", rn, fnum);
         return 0;
      }
      else
         return (*f[fnum])(
            ch,
            event_type,
            a,
            size,
            ret_size,
            ret_type,
            auxargs);
}

/*
*/
#ifdef USE_MACOSX
int wid_set_item_func_addr_( number, fun )
#else
int wid_set_item_func_addr( number, fun )
#endif
   int number;
   int (*fun)();
{
   if (number < 0 || number >= NUM_ITEM_FUNCTIONS) return 0;
/*
   (*fun)();
*/
   f[number] = *fun;
   return 1;
}
