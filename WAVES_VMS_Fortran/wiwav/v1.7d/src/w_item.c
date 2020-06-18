/* w_item.c - source for w_item_i4, w_item_r4, w_item_r8, and w_item_ch
   Under VAX/VMS these w_item_* routines expect character strings passed by 
   string descriptor.  These routines are intended to be called from fortran.
*/
#include "wind_os_def.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "item.h"

#define	max_channels 8
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
   int len;
   short kind;
   char *str;
};

#if SYSTEM == SUNOS
#include <memory.h>
#define FSTR char
#define FSTRLEN(somestr) strlen(somestr)
#define XPTR aitem
#define YPTR achar
#define wind_supprs_int_msg wind_suppress_internal_msgs_
#ifndef SUPPLY_TRAILING_UNDERSCORE
#define SUPPLY_TRAILING_UNDERSCORE
#endif
#define MY_W_ITEM_CHAR w_item_char_
#endif

#ifdef USE_MACOSX
#include <memory.h>
#define FSTR char
#define FSTRLEN(somestr) strlen(somestr)
#define XPTR aitem
#define YPTR achar
#define wind_supprs_int_msg wind_suppress_internal_msgs_
#define MY_W_ITEM_CHAR w_item_char_
#endif

#if SYSTEM==VMS
#define FSTR struct desc
#define FSTRLEN(somestr) somestr->len
#define XPTR aitem->str
#define YPTR achar->str
#define wind_supprs_int_msg wind_suppress_internal_msgs
#undef SUPPLY_TRAILING_UNDERSCORE
#define MY_W_ITEM_CHAR w_item_char
#endif

#define MAX_ITEM_NAME_SIZE 64
#define MAX_I4_RET_SIZE_OF_ITEM 4096*2

#ifdef USE_MACOSX
extern int w_really_get_item_();           /* C function       */
extern int wid_do_xlate_cpy_();            /* C function       */
#else
extern int w_really_get_item();           /* C function       */
extern int wid_do_xlate_cpy();            /* C function       */
#endif
extern int wind_supprs_int_msg();         /* FORTRAN function */

struct same_name_item *apsni_saved[max_channels]=
       {NULL,NULL, NULL,NULL, NULL,NULL, NULL,NULL};
char *ev_names[max_channels]=
       {NULL,NULL, NULL,NULL, NULL,NULL, NULL,NULL};

int w_get_last_psni(ch, ppsni)		/* called from FORTRAN, mostly */
   int *ch;				/* channel numbers: 1..max_channels */
   struct same_name_item **ppsni;
{
   int i;

   i = *ch - 1;
   *ppsni = NULL;
   if (i < 0 || i >= max_channels) return 0;

   *ppsni = apsni_saved[i];
   return (*ppsni == NULL) ? 0 : 1;
}

#ifdef USE_MACOSX
int w_save_this_psni_(ch, psni)		/* called from C, mostly */
#else
int w_save_this_psni(ch, psni)		/* called from C, mostly */
#endif
   int ch;				/* channel numbers: 1..max_channels */
   struct same_name_item *psni;
{
   if (ch < 0 || ch >= max_channels) return 0;

   apsni_saved[ch] = psni;
   return 1;
}

#ifdef USE_MACOSX
int w_set_event_name_(ch, ename)	/* called from FORTRAN, mostly */
#else
int w_set_event_name(ch, ename)		/* called from FORTRAN, mostly */
#endif
   int *ch;
   char *ename;
{
   int i;

   i = *ch - 1;
   if (i < 0 || i >= max_channels) return 0;

  ev_names[i] = ename;
  return 1;
}

int 
#ifdef SUPPLY_TRAILING_UNDERSCORE

w_item_i4_

#else

#ifdef USE_MACOSX
w_item_i4_
#else
w_item_i4
#endif

#endif
(ach, aitem, ai4, size, ret_size)
   int *ach;
   FSTR *aitem;
   int *ai4;
   int *size;
   int *ret_size;
{
   char *rn="W_ITEM_I4";
   char item[MAX_ITEM_NAME_SIZE];
   int i,j,k;
   int bit_start;
   int ret_type;
   int ch;
   int sz;
   int ok;
   struct same_name_item *psni;

   k = FSTRLEN(aitem);

   for (j=0; j<k && XPTR[j] > ' ' && j<MAX_ITEM_NAME_SIZE; j++)
   {
      item[j] = (XPTR[j] >= 'a' && XPTR[j] <= 'z' ) ? 
                XPTR[j] - ('a'-'A') : XPTR[j];
   }
   if (j >= MAX_ITEM_NAME_SIZE) goto item_name_length_err;
   item[j] = '\0';


   if (*ach < 1 || *ach > max_channels) goto channel_value_err;
   bit_start = 0;
   ch = *ach - 1;
   sz = *size;
   ret_type = INT_RETURN;
   *ret_size = 0;
   for (i=0; i<*size; i++) ai4[i] = 0;

#ifdef USE_MACOSX
   ok = w_really_get_item_(
#else
   ok = w_really_get_item(
#endif
        ch,
        ev_names[ch],
        item,
        ai4,                       /* address of caller's buffer */
	sz,		           /* size in input data type units */
        ret_size,
        &bit_start,
        &ret_type,
        &(psni));                  /* ptr to item, for auxiliary info */
#ifdef USE_MACOSX
   w_save_this_psni_(ch, psni);
#else
   w_save_this_psni(ch, psni);
#endif
   if (ok != 1) goto item_get_err;
   if (*ret_size < 1) goto ret_size_err;
   if (ret_type != INT_RETURN) goto ret_type_err;

   return ok;

channel_value_err:
   printf("%s: invalid channel number or %d.\n", rn, *ach);
   return ERR;

item_name_length_err:
   if (!wind_supprs_int_msg())
      printf("%s: item name too long: %20.20s.\n", rn, item);
   return ERR;

item_get_err:
   if (!wind_supprs_int_msg())
      printf("%s: error getting item %s, ok=%d.\n", rn, item, ok);
   return ok;

ret_size_err:
   if (!wind_supprs_int_msg())
      printf("%s: return size of item is zero: %s.\n", rn, item);
   return ERR;

ret_type_err:
   if (!wind_supprs_int_msg())
      printf("%s: item is not INTEGER: %s.\n", rn, item);
   return ERR;
}

int 
#ifdef SUPPLY_TRAILING_UNDERSCORE

w_item_r4_

#else

#ifdef USE_MACOSX
w_item_r4_
#else
w_item_r4
#endif

#endif
(ach, aitem, ar4, size, ret_size)
   int *ach;
   FSTR *aitem;
   float *ar4;
   int *size;
   int *ret_size;
{
   char *rn="W_ITEM_R4";
   char item[MAX_ITEM_NAME_SIZE];
   int i,j,k;
   int bit_start;
   int ret_type;
   int ch;
   int sz;
   int ok;
   int *pi;
   float *pf;
   struct same_name_item *psni;

   k = FSTRLEN(aitem);

   for (j=0; j<k && XPTR[j] > ' ' && j<MAX_ITEM_NAME_SIZE; j++)
   {
      item[j] = (XPTR[j] >= 'a' && XPTR[j] <= 'z' ) ? 
                XPTR[j] - ('a'-'A') : XPTR[j];
   }
   if (j >= MAX_ITEM_NAME_SIZE) goto item_name_length_err;
   item[j] = '\0';

   if (*ach < 1 || *ach > max_channels) goto channel_value_err;
   bit_start = 0;
   ch = *ach - 1;
   sz = *size;
   ret_type = FLOAT_RETURN;
   *ret_size = 0;
   for (i=0; i<*size; i++) ar4[i] = 0.0;

#ifdef USE_MACOSX
   ok = w_really_get_item_(
#else
   ok = w_really_get_item(
#endif
        ch,
        ev_names[ch],
        item,
        ar4,                       /* address of caller's buffer */
	sz,		           /* size in input data type units */
        ret_size,
        &bit_start,
        &ret_type,
        &(psni));                  /* ptr to item, for auxiliary info */
#ifdef USE_MACOSX
   w_save_this_psni_(ch, psni);
#else
   w_save_this_psni(ch, psni);
#endif
   if (ok != 1) goto item_get_err;
   if (*ret_size < 1) goto ret_size_err;
   if (ret_type != FLOAT_RETURN)
      if (ret_type == INT_RETURN)
      {
         pi = (int *) ar4;
         pf = ar4;
         k = (*size < *ret_size) ? *size : *ret_size;
         for (j=0; j<k; j++, pi++, pf++) {i = *pi; *pf = (float) i; }
      }
      else goto ret_type_err;

   return ok;

channel_value_err:
   printf("%s: invalid channel number or %d.\n", rn, *ach);
   return ERR;

item_name_length_err:
   if (!wind_supprs_int_msg())
      printf("%s: item name too long: %20.20s.\n", rn, item);
   return ERR;

item_get_err:
   if (!wind_supprs_int_msg())
      printf("%s: error getting item %s, ok=%d.\n", rn, item, ok);
   return ok;

ret_size_err:
   if (!wind_supprs_int_msg())
      printf("%s: return size of item is zero: %s.\n", rn, item);
   return ERR;

ret_type_err:
   if (!wind_supprs_int_msg())
      printf("%s: item not convertable to REAL*4: %s.\n", rn, item);
   return ERR;
}

int 
#ifdef SUPPLY_TRAILING_UNDERSCORE

w_item_r8_

#else

#ifdef USE_MACOSX
w_item_r8_
#else
w_item_r8
#endif

#endif
(ach, aitem, ar8, size, ret_size)
   int *ach;
   FSTR *aitem;
   double *ar8;
   int *size;
   int *ret_size;
{
   char *rn="W_ITEM_R8";
   char item[MAX_ITEM_NAME_SIZE];
   int i,j,k;
   int bit_start;
   int ret_type;
   int ch;
   int sz;
   int ok;
   int *pi;
   float *pf;
   double *pd;
   struct same_name_item *psni;

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\nw_item_r8:  f= %s  l= %d\n", __FILE__, __LINE__) ;
fflush (stdout) ;
printf ("   ach= %lx   aitem= %lx   ar8= %lx   size= %lx",
   ach, aitem, ar8, size) ;
fflush (stdout) ;
printf ("   ret_siz= %lx\n", ret_size) ;
fflush (stdout) ;
/* end diags */
if (ach)
    {
    printf ("   *ach= %d\n", *ach) ;
    fflush (stdout) ;
    }
if (aitem)
    {
    printf ("   aitem= %s\n", aitem) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

   k = FSTRLEN(aitem);

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   strlen(aitem) == k= %d\n", __LINE__, k) ;
/* end diags */
#endif

   for (j=0; j<k && XPTR[j] > ' ' && j<MAX_ITEM_NAME_SIZE; j++)
   {
      item[j] = (XPTR[j] >= 'a' && XPTR[j] <= 'z' ) ? 
                XPTR[j] - ('a'-'A') : XPTR[j];

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("      l= %d   j= %d   item[%d]= %c (%2.2x)\n", __LINE__,
    j, j, item[j], (unsigned char *) &item[j]) ;
fflush (stdout) ;
/* end diags */
#endif

   }

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   j= %d   MAX_ITEM_NAME_SIZE= %d\n", __LINE__,
    j, (int) MAX_ITEM_NAME_SIZE) ;
fflush (stdout) ;
/* end diags */
#endif

   if (j >= MAX_ITEM_NAME_SIZE) goto item_name_length_err;
   item[j] = '\0';

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   item= %s\n", __LINE__, item) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   *ach= %d   max_channels= %d\n", __LINE__,
    *ach, max_channels) ;
fflush (stdout) ;
/* end diags */
#endif

   if (*ach < 1 || *ach > max_channels) goto channel_value_err;
   bit_start = 0;
   ch = *ach - 1;
   sz = *size;
   ret_type = DOUBLE_RETURN;

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   ch= %d   sz= %d   ret_type= %d (DOUBLE_RETURN)\n",  
    __LINE__, ch, sz, ret_type) ;
fflush (stdout) ;
/* end diags */
#endif

   *ret_size = 0;
   for (i=0; i<*size; i++) ar8[i] = 0.0;

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   before w_really_get_time():\n", __LINE__) ;
fflush (stdout) ;
printf ("      ch= %d\n", ch) ;
fflush (stdout) ;
printf ("      ev_names[ch]= %lx\n", ev_names[ch]) ;
fflush (stdout) ;
if (ev_names[ch])
    {
    printf ("      ev_names[ch]= %s\n", ev_names[ch]) ;
    fflush (stdout) ;
    }
printf ("      item= %s\n", item) ;
fflush (stdout) ;
printf ("      sz= %d\n", sz) ;
fflush (stdout) ;
printf ("      *ret_size= %d\n", *ret_size) ;
fflush (stdout) ;
printf ("      psni= %lx\n", psni) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef USE_MACOSX
   ok = w_really_get_item_(
#else
   ok = w_really_get_item(
#endif
        ch,
        ev_names[ch],
        item,
        ar8,                       /* address of caller's buffer */
	sz,		           /* size in input data type units */
        ret_size,
        &bit_start,
        &ret_type,
        &(psni));                  /* ptr to item, for auxiliary info */

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   w_really_get_time() ret:   ok= %d\n",
     __LINE__, ok) ;
fflush (stdout) ;
printf ("      bit_start= %d\n", bit_start) ;
fflush (stdout) ;
printf ("      ret_type= %d\n", ret_type) ;
fflush (stdout) ;
printf ("      *ret_size= %d\n", *ret_size) ;
fflush (stdout) ;
printf ("      psni= %lx\n", psni) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef USE_MACOSX
   w_save_this_psni_(ch, psni);
#else
   w_save_this_psni(ch, psni);
#endif

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   after w_save_this_psni:\n", __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   if (ok != 1) goto item_get_err;

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   *ret_size= %d\n", __LINE__, *ret_size) ;
fflush (stdout) ;
/* end diags */
#endif

   if (*ret_size < 1) goto ret_size_err;

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("\n   l= %d   ret_type= %d   DOUBLE_RETURN= %d\n", __LINE__,
     ret_type, DOUBLE_RETURN) ;
fflush (stdout) ;
/* end diags */
#endif

   if (ret_type != DOUBLE_RETURN)
   {
      switch(ret_type & TYPE_RETURN_MASK)
      {
      case INT_RETURN:
         pi = (int *) ar8;
         pd = ar8;
         j = (*size > *ret_size) ? *ret_size : *size;
         *ret_size = j;
         for (--j; j >= 0; j--) { pd[j] = (double) pi[j]; }
         break;
      case FLOAT_RETURN:
         pf = (float *) ar8;
         pd = ar8;
         j = (*size > *ret_size) ? *ret_size : *size;
         *ret_size = j;
         for (--j; j >= 0; j--) { pd[j] = (double) pf[j]; }
         break;
      default:
         goto ret_type_err;
         break;
      }
   }

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("exit w_item_r8:  r= %d  l= %d\n", ok, __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return ok;

channel_value_err:
   printf("%s: invalid channel number or %d.\n", rn, *ach);

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("exit w_item_r8:  r= %d  l= %d\n", ERR, __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return ERR;

item_name_length_err:
   if (!wind_supprs_int_msg())
      printf("%s: item name too long: %20.20s.\n", rn, item);

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("exit w_item_r8:  r= %d  l= %d\n", ERR, __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return ERR;

item_get_err:
   if (!wind_supprs_int_msg())
      printf("%s: error getting item %s, ok=%d.\n", rn, item, ok);

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("exit w_item_r8:  r= %d  l= %d\n", ok, __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return ok;

ret_size_err:
   if (!wind_supprs_int_msg())
      printf("%s: return size of item is zero: %s.\n", rn, item);

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("exit w_item_r8:  r= %d  l= %d\n", ERR, __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return ERR;

ret_type_err:
   if (!wind_supprs_int_msg())
      printf("%s: item not convertable to REAL*8: %s.\n", rn, item);

#ifdef DBG_W_ITEM_R8
/* diags */
printf ("exit w_item_r8:  r= %d  l= %d\n", ERR, __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return ERR;
}

int 
#ifdef SUPPLY_TRAILING_UNDERSCORE

w_item_char_

#else

#ifdef USE_MACOSX
w_item_char_
#else
w_item_char
#endif

#endif
(ach, aitem, achar, size, ret_size, len_aitem, len_achar)
   int *ach;
   FSTR *aitem;
   FSTR *achar;
   int *size;
   int *ret_size;
   int len_aitem;
   int len_achar;
{
   char *rn="W_ITEM_CHAR";
   char item[MAX_ITEM_NAME_SIZE];
   int i,j,k,n;
   int bit_start;
   int ret_type;
   int ch;
   int sz;
   int sz2;
   int ok;
   int *pi;
   char *pc;
   int pi4[MAX_I4_RET_SIZE_OF_ITEM];
   int return_code;
   int sz_one_element;
   struct same_name_item *psni;

   i = FSTRLEN(aitem);
   if (i >= MAX_ITEM_NAME_SIZE) goto item_name_length_err;

   for (j=0; j<i && XPTR[j] > ' '; j++)
   {
      item[j] = (XPTR[j] >= 'a' && XPTR[j] <= 'z' ) ? 
                XPTR[j] - ('a'-'A') : XPTR[j];
   }
   item[j] = '\0';

   if (*ach < 1 || *ach > max_channels) goto channel_value_err;
   bit_start = 0;
   ch = *ach - 1;
   ret_type = CHAR_RETURN;
   *ret_size = 0;
#if SYSTEM==VMS
   sz_one_element = FSTRLEN(achar);
#endif
#if SYSTEM==SUNOS
   sz_one_element = len_achar;
#endif
#ifdef USE_MACOSX
   sz_one_element = len_achar;
#endif
   sz = *size * sz_one_element;

   k = MAX_I4_RET_SIZE_OF_ITEM * 4;
   sz2 = sz > k ? k : sz;

   j = sz2 / 4;
   if ((j%4) != 0) j++;
   for (i=0; i<j; i++) pi4[i] = 0;

   /* replace nulls and other control chars with spaces */
   /* IDL chokes bad on nulls... */
   for (i=0; i<sz; i++) if (YPTR[i] < ' ') YPTR[i] = ' ';

#ifdef USE_MACOSX
   ok = w_really_get_item_(
#else
   ok = w_really_get_item(
#endif
        ch,
        ev_names[ch],
        item,
        pi4,                  /* i4 aligned address of caller's buffer */
	sz2,		      /* size in input data type units (char)  */
        ret_size,
        &bit_start,
        &ret_type,
        &(psni));             /* ptr to item, for auxiliary info */
#ifdef USE_MACOSX
   w_save_this_psni_(ch, psni);
#else
   w_save_this_psni(ch, psni);
#endif
   if (ok != 1) goto item_get_err;
   if (*ret_size < 1) goto ret_size_err;
   return_code = OK;
   switch(ret_type & TYPE_RETURN_MASK)
   {
   case CHAR_RETURN:
      for (i=0, pc = (char *) pi4; i<*ret_size; i++) YPTR[i] = pc[i];
      for (; i < sz; i++) YPTR[i] = ' ';
      break;
   case INT_RETURN:
      k = *ret_size > *size ? *size : *ret_size;
      *ret_size = k;
      for (i=0; i < k; i++)
      {
         j = sz_one_element * i;
#ifdef USE_MACOSX
         ok = wid_do_xlate_cpy_(psni, &(pi4[i]), &(YPTR[j]), 
#else
         ok = wid_do_xlate_cpy(psni, &(pi4[i]), &(YPTR[j]), 
#endif
                 sz_one_element, 0);
         if (ok != 1)
            return_code = ERR;
         else 
            for (n=j; n<(j+ sz_one_element); n++) 
               if (YPTR[n] < ' ') YPTR[n] = ' ';
      }
      break;
   default:
      goto ret_type_err;
      break;
   }

   return return_code;

channel_value_err:
   printf("%s: invalid channel number or %d.\n", rn, *ach);
   return ERR;

item_name_length_err:
   if (!wind_supprs_int_msg())
      printf("%s: item name too long: %s.\n", rn, item);
   return ERR;

item_get_err:
   if (!wind_supprs_int_msg())
      printf("%s: error getting item %s, ok=%d.\n", rn, item, ok);
   return ok;

ret_size_err:
   if (!wind_supprs_int_msg())
      printf("%s: return size of item is zero: %s.\n", rn, item);
   return ERR;

ret_type_err:
   if (!wind_supprs_int_msg())
      printf("%s: item not convertable to REAL*8: %s.\n", rn, item);
   return ERR;
}

/* this routine should only be called from idl, but it should be ok
   from VMS or SunOS
*/
int w_item_char_idl_ary(ach, aitem, achar, size, ret_size, len)
   int *ach;
   struct desc *aitem;
   struct desc *achar;
   int *size;
   int *ret_size;
   int *len;			/* length of one element of the array */
{
   char *rn="W_ITEM_CHAR_IDL_ARY";
   int i,j,k,n;
   struct desc x;
   char item[MAX_ITEM_NAME_SIZE];
   char *buf;

#if SYSTEM==SUNOS
   /* in this case, idl (or an intermediate wind_lib routine)
      has passed in the item name and buffer via
      descriptor, so we convert to normal C arguments to pass
      to w_item_char
   */
   i = aitem->len > MAX_ITEM_NAME_SIZE ? MAX_ITEM_NAME_SIZE : aitem->len;
   strncpy(item,aitem->str,i);
   buf = achar->str;
   j = *len;
   return MY_W_ITEM_CHAR (ach, item, buf, size, ret_size, i, j);
#endif

#ifdef USE_MACOSX
   /* in this case, idl (or an intermediate wind_lib routine)
      has passed in the item name and buffer via
      descriptor, so we convert to normal C arguments to pass
      to w_item_char
   */
   i = aitem->len > MAX_ITEM_NAME_SIZE ? MAX_ITEM_NAME_SIZE : aitem->len;
   strncpy(item,aitem->str,i);
   buf = achar->str;
   j = *len;
   return MY_W_ITEM_CHAR (ach, item, buf, size, ret_size, i, j);
#endif

#if SYSTEM==VMS
   x = *achar;
   x.len = *len;
   return MY_W_ITEM_CHAR (ach, aitem, &x, size, ret_size);
#endif

}
