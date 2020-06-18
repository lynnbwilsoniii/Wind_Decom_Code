/* c_interface.c - C language interface to the wind.waves Telemetry access
   library.

   This is the unix version.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wind_os_def.h"
#include "routine_def.h"

#define OK 1
#define ERR 0
#define YES 1
#define NO 0

#ifdef IDL_5_4
struct desc {
   unsigned short len;
   short kind;
   char *str;
};
#endif

struct desc {
   int   len;
   short kind;
   char *str;
};

int w_channel_open_c( ch, str )
   int *ch;
   char *str;
{
   int len;
   len = strlen(str);

#ifdef USE_MACOSX
   return w_channel_open_(ch, str, len);
#else
   return w_channel_open_(ch, str, len);
#endif
}

int w_channel_select_c( ch, str, t1, t2 )
   int *ch;
   char *str;
   double *t1;
   double *t2;
{
   int len;
   len = strlen(str);

#ifdef USE_MACOSX
   return w_channel_select_( ch, str, t1, t2, len);
#else
   return w_channel_select_( ch, str, t1, t2, len);
#endif
}

int w_channel_position_c( ch, t1 )
   int *ch;
   double *t1;
{
#ifdef USE_MACOSX
   return w_channel_position_( ch, t1 );
#else
   return w_channel_position_( ch, t1 );
#endif
}

int w_channel_filename_c( ch, str )
   int *ch;
   char *str;
{
   int len;
   len = strlen(str);

#ifdef USE_MACOSX
   return w_channel_filename_( ch, str, len );
#else
   return w_channel_filename_( ch, str, len );
#endif
}

int w_channel_close_c( ch )
   int *ch;
{
#ifdef USE_MACOSX
   return w_channel_close_( ch );
#else
   return w_channel_close_( ch );
#endif
}

/*****************************************************************************/

int w_messages_off_c( ch )
   int *ch;
{
#ifdef USE_MACOSX
   return w_messages_off_( ch );
#else
   return w_messages_off_( ch );
#endif
}

int w_messages_on_c( ch )
   int *ch;
{
#ifdef USE_MACOSX
   return w_messages_on_( ch );
#else
   return w_messages_on_( ch );
#endif
}

int w_version_cc( str )
   char *str;
{
   int len;
   len = strlen(str);

#ifdef USE_MACOSX
   return w_version_( str, len );
#else
   return w_version_( str, len );
#endif
}

/*****************************************************************************/

int w_item_i4_c( ch, item, ai4, sz, ret_sz )
   int *ch;
   char *item;
   int *ai4;
   int *sz;
   int *ret_sz;
{
#ifdef USE_MACOSX
   return w_item_i4_( ch, item, ai4, sz, ret_sz );
#else
   return w_item_i4_( ch, item, ai4, sz, ret_sz );
#endif
}

int w_item_r4_c( ch, item, ar4, sz, ret_sz )
   int *ch;
   char *item;
   float *ar4;
   int *sz;
   int *ret_sz;
{
#ifdef USE_MACOSX
   return w_item_r4_( ch, item, ar4, sz, ret_sz );
#else
   return w_item_r4_( ch, item, ar4, sz, ret_sz );
#endif
}

int w_item_r8_c( ch, item, ar8, sz, ret_sz )
   int *ch;
   char *item;
   double *ar8;
   int *sz;
   int *ret_sz;
{
#ifdef USE_MACOSX
   return w_item_r8_( ch, item, ar8, sz, ret_sz );
#else
   return w_item_r8_( ch, item, ar8, sz, ret_sz );
#endif
}

int w_item_char_c( ch, item, achar, sz, ret_sz )
   int *ch;
   char *item;
   char *achar;
   int *sz;
   int *ret_sz;
{
   int len_item, len_achar;
   int ok;
   int i,j,k;
   char *pc;

   len_item = strlen(item);
   len_achar = strlen(achar) + 1;
   pc = achar;

#ifdef USE_MACOSX
   ok = w_item_char_( ch, item, achar, sz, ret_sz, len_item, len_achar );
#else
   ok = w_item_char_( ch, item, achar, sz, ret_sz, len_item, len_achar );
#endif

   /* supply the terminating null characters */
   k = (*ret_sz < *sz) ? *ret_sz : *sz;
   for (i=0; i < k; i++)
   {
      pc = &(achar[(i * len_achar) + len_achar - 1]);
      if (*pc > ' ')
         *pc = '\0';
      else
         for (j=len_achar; j > 0 && *pc <= ' '; j--, pc--) *pc = '\0';
   }

   return ok;
}

int w_item_xlate_c( ch, event, item, pval, xlate )
   int *ch;
   char *item;
   char *event;
   int *pval;
   char *xlate;
{
   int lenev, lenit, lenxl;
   lenev = strlen(event);
   lenit = strlen(item);
   lenxl = strlen(xlate);

#ifdef USE_MACOSX
   return w_item_xlate_(ch, event, item, pval, xlate, lenev, lenit, lenxl);
#else
   return w_item_xlate_(ch, event, item, pval, xlate, lenev, lenit, lenxl);
#endif
}

int w_event_c( ch, event )
   int *ch;
   char *event;
{
   int len;
   len = strlen(event);
#ifdef USE_MACOSX
   return w_event_( ch, event, len);
#else
   return w_event_( ch, event, len);
#endif
}

int w_status_c( ch )
   int *ch;
{
#ifdef USE_MACOSX
   return w_status_(ch);
#else
   return w_status_(ch);
#endif
}

/*****************************************************************************/

int w_ur8_from_ydoy_c(pur8, pyear, pdoy, pmsec)
   double *pur8;
   int *pyear;
   int *pdoy;
   int *pmsec;
{
#ifdef USE_MACOSX
   return w_ur8_from_ydoy_(pur8, pyear, pdoy, pmsec);
#else
   return w_ur8_from_ydoy_(pur8, pyear, pdoy, pmsec);
#endif
}

int w_ur8_from_ymd_c(pur8, pyear, pmonth, pday, phour, pminute, psecond, pmsec)
   double *pur8;
   int *pyear;
   int *pmonth;
   int *pday;
   int *phour;
   int *pminute;
   int *psecond;
   int *pmsec;
{
#ifdef USE_MACOSX
   return w_ur8_from_ymd_(pur8, 
                          pyear, pmonth, pday, 
                          phour, pminute, psecond, pmsec);
#else
   return w_ur8_from_ymd_(pur8, 
                          pyear, pmonth, pday, 
                          phour, pminute, psecond, pmsec);
#endif
}

int w_ur8_to_string_c(pur8, str)
   double *pur8;
   char *str;
{
   int len;
   len = strlen(str);

#ifdef USE_MACOSX
   return w_ur8_to_string_(pur8, str, len);
#else
   return w_ur8_to_string_(pur8, str, len);
#endif
}

int w_ur8_to_string_fr_c(pur8, str)
   double *pur8;
   char *str;
{
   int len;
   len = strlen(str);

#ifdef USE_MACOSX
   return w_ur8_to_string_fr_(pur8, str, len);
#else
   return w_ur8_to_string_fr_(pur8, str, len);
#endif
}

int w_ur8_to_ydoy_c(pur8, pyear, pdoy, pmsec)
   double *pur8;
   int *pyear;
   int *pdoy;
   int *pmsec;
{
#ifdef USE_MACOSX
   return w_ur8_to_ydoy_(pur8, pyear, pdoy, pmsec);
#else
   return w_ur8_to_ydoy_(pur8, pyear, pdoy, pmsec);
#endif
}

int w_ur8_to_ymd_c(pur8, pyear, pmonth, pday, phour, pminute, psecond, pmsec)
   double *pur8;
   int *pyear;
   int *pmonth;
   int *pday;
   int *phour;
   int *pminute;
   int *psecond;
   int *pmsec;
{
#ifdef USE_MACOSX
   return w_ur8_to_ymd_(pur8, 
                          pyear, pmonth, pday, 
                          phour, pminute, psecond, pmsec);
#else
   return w_ur8_to_ymd_(pur8, 
                          pyear, pmonth, pday, 
                          phour, pminute, psecond, pmsec);
#endif
}

int w_ur8_to_ymd_i_c(pur8, pymd, phms)
   double *pur8;
   int *pymd;
   int *phms;
{
#ifdef USE_MACOSX
   return w_ur8_to_ymd_i_(pur8, pymd, phms);
#else
   return w_ur8_to_ymd_i_(pur8, pymd, phms);
#endif
}

int w_ur8_from_ymd_i_c(pur8, pymd, phms)
   double *pur8;
   int *pymd;
   int *phms;
{
#ifdef USE_MACOSX
   return w_ur8_from_ymd_i_(pur8, pymd, phms);
#else
   return w_ur8_from_ymd_i_(pur8, pymd, phms);
#endif
}

int w_ur8_to_epoch_c(pur8, pepoch)
   double *pur8;
   double *pepoch;
{
#ifdef USE_MACOSX
   return w_ur8_to_epoch_(pur8, pepoch);
#else
   return w_ur8_to_epoch_(pur8, pepoch);
#endif
}

int w_ur8_from_epoch_c(pur8, pepoch)
   double *pur8;
   double *pepoch;
{
#ifdef USE_MACOSX
   return w_ur8_to_epoch_(pur8, pepoch);
#else
   return w_ur8_to_epoch_(pur8, pepoch);
#endif
}
