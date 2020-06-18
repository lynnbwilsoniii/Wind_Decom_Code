
/* sun_idl_interface.c - shared object library interface for wind_lib
under SunOS with IDL
*/
#include <stdio.h>

#ifdef IDL_VERSION_5_4
typedef struct {
   unsigned short slen;
   short stype;
   char *s;
} STRING;
#endif

typedef struct {
   int slen;
   short stype;
   char *s;
} STRING;

extern int wind_tm_open_channel_();        /* FORTRAN routines, pre Nov-94 */
extern int wind_tm_close_channel_();
extern int wind_tm_get_filename_();
extern int wind_tm_set_messages_off_();
extern int wind_tm_set_messages_on_();
extern int wind_tm_version_();
extern int wind_tm_eof_();

extern int wind_tm_get_event_();
extern int wind_tm_get_next_event_();
extern int wind_tm_get_previous_event_();
extern int wind_tm_get_item_();
extern int wind_tm_xlate_item_();

extern int wind_tm_set_wait_();
extern int wind_tm_set_nowait_();

extern int wind_tm_get_mfmf_();
extern int wind_tm_get_stream_mfmf_();
extern int wind_tm_get_next_mfmf_();
extern int wind_tm_get_earliest_mfmf_();
extern int wind_tm_get_latest_mfmf_();

extern int wind_tm_decrement_packet_();
extern int wind_tm_increment_packet_();
extern int wind_tm_decrement_mfmf_();
extern int wind_tm_increment_mfmf_();
extern int wind_tm_delta_mfmf_();

extern int wind_tm_bit_rate_();
extern int wind_tm_get_word_();
extern int wind_tm_get_minor_frame_();
extern int wind_tm_get_major_frame_();
extern int wind_tm_get_packet_();
extern int wind_tm_get_hk_();
extern int wind_tm_get_test_();
extern int wind_tm_get_step_();

extern int wind_tm_scet_to_mfmf_();
extern int wind_tm_mfmf_to_scet_();
extern int wind_tm_ert_to_mfmf_();
extern int wind_tm_mfmf_to_ert_();

extern int w_channel_open_();		/* newer routines, post Nov-94 */
extern int w_channel_select_();
extern int w_channel_position_();
extern int w_channel_filename_();
extern int w_channel_close_();

extern int w_messages_off_();
extern int w_messages_on_();
extern int w_version_();

#ifdef USE_MACOSX
extern int w_item_i4_();		/* C routine */
extern int w_item_r4_();		/* C routine */
extern int w_item_r8_();		/* C routine */
extern int w_item_char_();		/* C routine */
#else
extern int w_item_i4_();			/* C routine */
extern int w_item_r4_();			/* C routine */
extern int w_item_r8_();			/* C routine */
extern int w_item_char_();			/* C routine */
#endif

extern int w_item_xlate_();
extern int w_event_();
extern int w_status_();

extern int w_ur8_from_ydoy_();
extern int w_ur8_from_ymd_();
extern int w_ur8_to_string_();
extern int w_ur8_to_string_fr_();
extern int w_ur8_to_ydoy_();
extern int w_ur8_to_ymd_();
extern int w_ur8_to_ymd_i_();
extern int w_ur8_from_ymd_i_();
extern int w_ur8_to_epoch_();
extern int w_ur8_from_epoch_();

int w_channel_open(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   STRING *context;

/*
puts("...sun_idl_interface.c, w_channel_open...");
*/
   pch = (int *) argv[0];
   context = (STRING *) argv[1];
   len = (int) context->slen;

   return w_channel_open_(pch, context->s, len);
}

int w_channel_select(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   double *pd1, *pd2;
   STRING *context;

/*
puts("...sun_idl_interface.c, w_channel_select...");
*/
   pch = (int *) argv[0];
   context = (STRING *) argv[1];
   len = (int) context->slen;
   pd1 = (double *) argv[2];
   pd2 = (double *) argv[3];

   return w_channel_select_(pch, context->s, pd1, pd2, len);
}

int w_channel_position(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   double *pt; /* ur8 time */

   pch = (int *) argv[0];
   pt  = (double *) argv[1];

   return w_channel_position_(pch, pt);
}

int w_channel_filename(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   STRING *filename;
   int ok;
   int i;

   pch = (int *) argv[0];
   filename = (STRING *) argv[1];
   len = (int) filename->slen;

   ok = w_channel_filename_(pch, filename->s, len);
   for (i=0; i < len; ++i) if (filename->s[i] == '\0') filename->s[i] = ' ';

   return ok;
}

int w_channel_close(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return w_channel_close_(pch);
}

int w_messages_off(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return w_messages_off_(pch);
}

int w_messages_on(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return w_messages_on_(pch);
}

int w_version(argc, argv)
   int argc;
   void *argv[];
{
   int len;
   STRING *version;
   int i, ok;

   version = (STRING *) argv[0];
   len = (int) version->slen;

   ok = w_version_(version->s, len);
   for (i=0; i < len; ++i) if (version->s[i] == '\0') version->s[i] = ' ';

   return ok;
}

int w_event(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   STRING *event_name;

   pch  = (int *) argv[0];
   event_name = (STRING *) argv[1];
   len = (int) event_name->slen;

   return w_event_(pch, event_name->s, len);
}

int w_status(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return w_status_(pch);
}

int w_item_i4_idl(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   int *pbuf;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;

#ifdef DBG_W_ITEM_I4_IDL_0
/* diags */
printf ("\nw_item_i4_idl:  f= %s   l= %d\n", __FILE__, __LINE__) ;
fflush (stdout) ;
printf ("   argc= %d\n", argc) ;
fflush (stdout) ;
/* end diags */
#endif

   pch  = (int *) argv[0];

#ifdef DBG_W_ITEM_I4_IDL_0
/* diags */
printf ("   pch= %lx\n", pch) ;
fflush (stdout) ;
if (pch)
    {
    printf ("      *pch= %d\n", *pch) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

   item_name = (STRING *) argv[1];

#ifdef DBG_W_ITEM_I4_IDL_0
/* diags */
printf ("   item_name= %lx\n", item_name) ;
fflush (stdout) ;
if (item_name)
    {
    printf ("      item_name->slen= %d\n", item_name->slen) ;
    fflush (stdout) ;
    printf ("      item_name->s= %lx\n", item_name->s) ;
    fflush (stdout) ;
    if (item_name->s)
        {
        printf ("      item_name->s= %s\n", item_name->s) ;
        fflush (stdout) ;
        }
    }
/* end diags */
#endif

   len = (int) item_name->slen;

#ifdef DBG_W_ITEM_I4_IDL_0
/* diags */
printf ("   len= %d\n", len) ;
fflush (stdout) ;
/* end diags */
#endif

   pbuf = (int *) argv[2];
   pbuf_sz = (int *) argv[3];
   pret_sz = (int *) argv[4];

#ifdef DBG_W_ITEM_I4_IDL_0
/* diags */
if (pbuf)
    {
    printf ("      *pbuf= %.5f\n", *pbuf) ;
    fflush (stdout) ;
    }
if (pbuf_sz)
    {
    printf ("      *pbuf_sz= %d\n", *pbuf_sz) ;
    fflush (stdout) ;
    }
if (pret_sz)
    {
    printf ("      *pret_sz= %d\n", *pret_sz) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

#ifdef DBG_W_ITEM_I4_IDL_0
/* diags */
printf ("\n   l= %d   before:  w_item_i4():\\n", __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef USE_MACOSX
   return w_item_i4_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);
#else
   return w_item_i4_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);
#endif
}

int w_item_r4_idl(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   float *pbuf;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;

#ifdef DBG_W_ITEM_R4_IDL_0
/* diags */
printf ("\nw_item_r4_idl:  f= %s   l= %d\n", __FILE__, __LINE__) ;
fflush (stdout) ;
printf ("   argc= %d\n", argc) ;
fflush (stdout) ;
/* end diags */
#endif

   pch  = (int *) argv[0];

#ifdef DBG_W_ITEM_R4_IDL_0
/* diags */
printf ("   pch= %lx\n", pch) ;
fflush (stdout) ;
if (pch)
    {
    printf ("      *pch= %d\n", *pch) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

   item_name = (STRING *) argv[1];

#ifdef DBG_W_ITEM_R4_IDL_0
/* diags */
printf ("   item_name= %lx\n", item_name) ;
fflush (stdout) ;
if (item_name)
    {
    printf ("      item_name->slen= %d\n", item_name->slen) ;
    fflush (stdout) ;
    printf ("      item_name->s= %lx\n", item_name->s) ;
    fflush (stdout) ;
    if (item_name->s)
        {
        printf ("      item_name->s= %s\n", item_name->s) ;
        fflush (stdout) ;
        }
    }
/* end diags */
#endif

   len = (int) item_name->slen;

#ifdef DBG_W_ITEM_R4_IDL_0
/* diags */
printf ("   len= %d\n", len) ;
fflush (stdout) ;
/* end diags */
#endif

   pbuf = (float *) argv[2];
   pbuf_sz = (int *) argv[3];
   pret_sz = (int *) argv[4];

#ifdef DBG_W_ITEM_R4_IDL_0
/* diags */
if (pbuf)
    {
    printf ("      *pbuf= %.5f\n", *pbuf) ;
    fflush (stdout) ;
    }
if (pbuf_sz)
    {
    printf ("      *pbuf_sz= %d\n", *pbuf_sz) ;
    fflush (stdout) ;
    }
if (pret_sz)
    {
    printf ("      *pret_sz= %d\n", *pret_sz) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

#ifdef DBG_W_ITEM_R4_IDL_0
/* diags */
printf ("\n   l= %d   before:  w_item_r4():\\n", __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef USE_MACOSX
   return w_item_r4_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);
#else
   return w_item_r4_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);
#endif
}

int w_item_r8_idl(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   double *pbuf;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;

#ifdef DBG_W_ITEM_R8_IDL_0
/* diags */
printf ("\nw_item_r8_idl:  f= %s   l= %d\n", __FILE__, __LINE__) ;
fflush (stdout) ;
printf ("   argc= %d\n", argc) ;
fflush (stdout) ;
/* end diags */
#endif

   pch  = (int *) argv[0];

#ifdef DBG_W_ITEM_R8_IDL_0
/* diags */
printf ("   pch= %lx\n", pch) ;
fflush (stdout) ;
if (pch)
    {
    printf ("      *pch= %d\n", *pch) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

   item_name = (STRING *) argv[1];

#ifdef DBG_W_ITEM_R8_IDL_0
/* diags */
printf ("   item_name= %lx\n", item_name) ;
fflush (stdout) ;
if (item_name)
    {
    printf ("      item_name->slen= %d\n", item_name->slen) ;
    fflush (stdout) ;
    printf ("      item_name->s= %lx\n", item_name->s) ;
    fflush (stdout) ;
    if (item_name->s)
        {
        printf ("      item_name->s= %s\n", item_name->s) ;
        fflush (stdout) ;
        }
    }
/* end diags */
#endif

   len = (int) item_name->slen;

#ifdef DBG_W_ITEM_R8_IDL_0
/* diags */
printf ("   len= %d\n", len) ;
fflush (stdout) ;
/* end diags */
#endif

   pbuf = (double *) argv[2];
   pbuf_sz = (int *) argv[3];
   pret_sz = (int *) argv[4];

#ifdef DBG_W_ITEM_R8_IDL_0
/* diags */
if (pbuf)
    {
    printf ("      *pbuf= %.5f\n", *pbuf) ;
    fflush (stdout) ;
    }
if (pbuf_sz)
    {
    printf ("      *pbuf_sz= %d\n", *pbuf_sz) ;
    fflush (stdout) ;
    }
if (pret_sz)
    {
    printf ("      *pret_sz= %d\n", *pret_sz) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

#ifdef DBG_W_ITEM_R8_IDL_0
/* diags */
printf ("\n   l= %d   before:  w_item_r8():\\n", __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef USE_MACOSX
   return w_item_r8_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);
#else
   return w_item_r8_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);
#endif
}

int w_item_char_idl(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;
   STRING *pbuf;

#ifdef DBG_W_ITEM_CHAR_IDL_0
/* diags */
printf ("\nw_item_char_idl:  f= %s   l= %d\n", __FILE__, __LINE__) ;
fflush (stdout) ;
printf ("   argc= %d\n", argc) ;
fflush (stdout) ;
/* end diags */
#endif

   pch  = (int *) argv[0];
   item_name = (STRING *) argv[1];
   len = (int) item_name->slen;
   pbuf = (STRING *) argv[2];
   pbuf_sz = (int *) argv[3];
   pret_sz = (int *) argv[4];


#ifdef DBG_W_ITEM_CHAR_IDL_0
/* diags */
printf ("   pch= %lx\n", pch) ;
fflush (stdout) ;
if (pch)
    {
    printf ("      *pch= %d\n", *pch) ;
    fflush (stdout) ;
    }
printf ("   item_name= %lx\n", item_name) ;
fflush (stdout) ;
if (item_name)
    {
    printf ("      item_name->length:  len= %d\n", len) ;
    fflush (stdout) ;
    printf ("      item_name->s= %lx\n", item_name->s) ;
    fflush (stdout) ;
    if (item_name->s)
        {
        printf ("      item_name->s= %s\n", item_name->s) ;
        fflush (stdout) ;
        }
    }
printf ("   pbuf= %lx\n", pbuf) ;
fflush (stdout) ;
if (pbuf)
    {
    printf ("      pbuf->slen= %d\n", pbuf->slen) ;
    fflush (stdout) ;
    printf ("      pbuf->s= %lx\n", pbuf->s) ;
    fflush (stdout) ;
    if (pbuf->s)
        {
        printf ("      pbuf->s= %s\n", pbuf->s) ;
        fflush (stdout) ;
        }
    }
printf ("   pbuf_sz= %lx\n", pbuf_sz) ;
fflush (stdout) ;
if (pbuf_sz)
    {
    printf ("      *pbuf_sz= %d\n", *pbuf_sz) ;
    fflush (stdout) ;
    }
printf ("   pret_sz= %lx\n", pret_sz) ;
fflush (stdout) ;
if (pret_sz)
    {
    printf ("      *pret_sz= %d\n", *pret_sz) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

#ifdef DBG_W_ITEM_CHAR_IDL_0
/* diags */
printf ("\n   l= %d   before w_item_char():\n", __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

#ifdef USE_MACOSX
   return w_item_char_(pch, item_name->s, 
                       pbuf->s, pbuf_sz, pret_sz, 
                       len, (int) pbuf->slen);
#else
   return w_item_char_(pch, item_name->s, 
                       pbuf->s, pbuf_sz, pret_sz, 
                       len, (int) pbuf->slen);
#endif
}

int w_item_xlate(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int lenit, lenev, lenxl;
   int *pval;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;
   STRING *event_name;
   STRING *xlate;
   int ok;

   pch  = (int *) argv[0];
   event_name = (STRING *) argv[1];
   lenev = (int) event_name->slen;
   item_name = (STRING *) argv[2];
   lenit = (int) item_name->slen;
   pval = (int *) argv[3];
   xlate = (STRING *) argv[4];
   lenxl = (int) xlate->slen;

   ok = w_item_xlate_(pch, 
           event_name->s, 
           item_name->s, 
           pval,
           xlate->s,
           lenev,
           lenit,
           lenxl);

   return ok;
}

int w_ur8_from_ydoy(argc, argv)
   int argc;
   void *argv[];
{
   int *pyear;
   int *pdoy;
   int *pmsec;
   double *pur8;

   pur8  = (double *) argv[0];
   pyear = (int *) argv[1];
   pdoy  = (int *) argv[2];
   pmsec = (int *) argv[3];

   return w_ur8_from_ydoy_(pur8, pyear, pdoy, pmsec);
}

int w_ur8_from_ymd(argc, argv)
   int argc;
   void *argv[];
{
   int *pyear;
   int *pmonth;
   int *pday;
   int *phour;
   int *pminute;
   int *psecond;
   int *pmsec;
   double *pur8;

#ifdef DBG_W_UR8_FROM_YMD
/* diags */
printf ("\nw_ur8_from_ymd:  f= %s   l= %d\n", __FILE__, __LINE__) ;
fflush (stdout) ;
printf ("   argc= %d\n", argc) ;
fflush (stdout) ;
/* end diags */
#endif

   pur8    = (double *) argv[0];
   pyear   = (int *) argv[1];
   pmonth  = (int *) argv[2];
   pday    = (int *) argv[3];
   phour   = (int *) argv[4];
   pminute = (int *) argv[5];
   psecond = (int *) argv[6];
   pmsec   = (int *) argv[7];

#ifdef DBG_W_UR8_FROM_YMD
/* diags */
printf ("\n   l= %d\n", __LINE__) ;
fflush (stdout) ;
printf ("      pur8= %lx\n", pur8) ;
fflush (stdout) ;
if (pur8)
    {
    printf ("      *pur8= %14.6f\n", *pur8) ;
    fflush (stdout) ;
    }
printf ("      pyear= %lx\n", pyear) ;
fflush (stdout) ;
if (pyear)
    {
    printf ("      *pyear= %d\n", *pyear) ;
    fflush (stdout) ;
    }
printf ("      pmonth= %lx\n", pmonth) ;
fflush (stdout) ;
if (pmonth)
    {
    printf ("      *pmonth= %d\n", *pmonth) ;
    fflush (stdout) ;
    }
printf ("      pday= %lx\n", pday) ;
fflush (stdout) ;
if (pday)
    {
    printf ("      *pday= %d\n", *pday) ;
    fflush (stdout) ;
    }
printf ("      phour= %lx\n", phour) ;
fflush (stdout) ;
if (phour)
    {
    printf ("      *phour= %d\n", *phour) ;
    fflush (stdout) ;
    }
printf ("      pminute= %lx\n", pminute) ;
fflush (stdout) ;
if (pminute)
    {
    printf ("      *pminute= %d\n", *pminute) ;
    fflush (stdout) ;
    }
printf ("      psecond= %lx\n", psecond) ;
fflush (stdout) ;
if (psecond)
    {
    printf ("      *psecond= %d\n", *psecond) ;
    fflush (stdout) ;
    }
printf ("      pmsec= %lx\n", pmsec) ;
fflush (stdout) ;
if (pmsec)
    {
    printf ("      *pmsec= %d\n", *pmsec) ;
    fflush (stdout) ;
    }
/* end diags */
#endif

#ifdef DBG_W_UR8_FROM_YMD
/* diags */
printf ("\n   l= %d   before w_ur8_from_ymd_():\n", __LINE__) ;
fflush (stdout) ;
/* end diags */
#endif

   return w_ur8_from_ymd_(pur8, 
                          pyear, pmonth, pday, 
                          phour, pminute, psecond, pmsec);
}

int w_ur8_to_string(argc, argv)
   int argc;
   void *argv[];
{
   int len;
   double *pur8;
   STRING *ascii_time;
   int ok;

   pur8 = (double *) argv[0];
   ascii_time = (STRING *) argv[1];
   len = (int) ascii_time->slen;

   ok = w_ur8_to_string_(pur8, ascii_time->s, len);
   return ok;
}

int w_ur8_to_string_fr(argc, argv)
   int argc;
   void *argv[];
{
   int len;
   double *pur8;
   STRING *ascii_time;

   pur8 = (double *) argv[0];
   ascii_time = (STRING *) argv[1];
   len = (int) ascii_time->slen;

   return w_ur8_to_string_fr_(pur8, ascii_time->s, len);
}

int w_ur8_to_ydoy(argc, argv)
   int argc;
   void *argv[];
{
   int *pyear;
   int *pdoy;
   int *pmsec;
   double *pur8;

   pur8  = (double *) argv[0];
   pyear = (int *) argv[1];
   pdoy  = (int *) argv[2];
   pmsec = (int *) argv[3];

   return w_ur8_to_ydoy_(pur8, pyear, pdoy, pmsec);
}

int w_ur8_to_ymd(argc, argv)
   int argc;
   void *argv[];
{
   int *pyear;
   int *pmonth;
   int *pday;
   int *phour;
   int *pminute;
   int *psecond;
   int *pmsec;
   double *pur8;

   pur8    = (double *) argv[0];
   pyear   = (int *) argv[1];
   pmonth  = (int *) argv[2];
   pday    = (int *) argv[3];
   phour   = (int *) argv[4];
   pminute = (int *) argv[5];
   psecond = (int *) argv[6];
   pmsec   = (int *) argv[7];

   return w_ur8_to_ymd_(pur8, 
                          pyear, pmonth, pday, 
                          phour, pminute, psecond, pmsec);
}

int w_ur8_to_ymd_i(argc, argv)
   int argc;
   void *argv[];
{
   int *pymd;
   int *phms;
   double *pur8;

   pur8 = (double *) argv[0];
   pymd = (int *) argv[1];
   phms = (int *) argv[2];

   return w_ur8_to_ymd_i_(pur8, pymd, phms);
}

int w_ur8_from_ymd_i(argc, argv)
   int argc;
   void *argv[];
{
   int *pymd;
   int *phms;
   double *pur8;

   pur8 = (double *) argv[0];
   pymd = (int *) argv[1];
   phms = (int *) argv[2];

   return w_ur8_from_ymd_i_(pur8, pymd, phms);
}

int w_ur8_from_epoch(argc, argv)
   int argc;
   void *argv[];
{
   double *pur8;
   double *pepoch;

   pur8  = (double *) argv[0];
   pepoch = (double *) argv[1];

   return w_ur8_from_epoch_(pur8, pepoch);
}

int w_ur8_to_epoch(argc, argv)
   int argc;
   void *argv[];
{
   double *pur8;
   double *pepoch;

   pur8  = (double *) argv[0];
   pepoch = (double *) argv[1];

   return w_ur8_to_epoch_(pur8, pepoch);
}

/*
  Old-style wind_lib routines, pre Nov-1994
*/

int wind_tm_open_channel(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   STRING *context;
   int ok;

   pch = (int *) argv[0];
   context = (STRING *) argv[1];
   len = (int) context->slen;

   ok = wind_tm_open_channel_(pch, context->s, len);

   return ok;
}

int wind_tm_close_channel(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return wind_tm_close_channel_(pch);
}

int wind_tm_get_filename(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   STRING *filename;

   pch = (int *) argv[0];
   filename = (STRING *) argv[1];
   len = (int) filename->slen;

   return wind_tm_get_filename_(pch, filename->s, len);
}

int wind_tm_set_messages_off(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return wind_tm_set_messages_off_(pch);
}

int wind_tm_set_messages_on(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return wind_tm_set_messages_on_(pch);
}

int wind_tm_version(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   STRING *version;

   pch = (int *) argv[0];
   version = (STRING *) argv[1];
   len = (int) version->slen;

   return wind_tm_version_(pch, version->s, len);
}

int wind_tm_eof(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_eof_(pch, pmjr, pmnr);
}

/*
  old event/item routines
*/

int wind_tm_get_event(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   int *pmjr;
   int *pmnr;
   STRING *event_name;
   int ok;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   event_name = (STRING *) argv[3];
   len = (int) event_name->slen;

   ok = wind_tm_get_event_(pch, pmjr, pmnr, event_name->s, len);

   return ok;
}


int wind_tm_get_next_event(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   int *pmjr;
   int *pmnr;
   STRING *event_name;
   int ok;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   event_name = (STRING *) argv[3];
   len = (int) event_name->slen;

   ok = wind_tm_get_next_event_(pch, pmjr, pmnr, event_name->s, len);

   return ok;
}


int wind_tm_get_previous_event(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   int *pmjr;
   int *pmnr;
   STRING *event_name;
   int ok;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   event_name = (STRING *) argv[3];
   len = (int) event_name->slen;

   ok = wind_tm_get_previous_event_(pch, pmjr, pmnr, event_name->s, len);

   return ok;
}


int wind_tm_get_item(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int len;
   int *pbuf;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;
   int ok;

   pch  = (int *) argv[0];
   item_name = (STRING *) argv[1];
   len = (int) item_name->slen;
   pbuf = (int *) argv[2];
   pbuf_sz = (int *) argv[3];
   pret_sz = (int *) argv[4];

   ok = wind_tm_get_item_(pch, item_name->s, pbuf, pbuf_sz, pret_sz, len);

   return ok;
}


int wind_tm_xlate_item(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int lenit, lenev, lenxl;
   int *pval;
   int *pbuf_sz;
   int *pret_sz;
   STRING *item_name;
   STRING *event_name;
   STRING *xlate;
   int ok;

   pch  = (int *) argv[0];
   event_name = (STRING *) argv[1];
   lenev = (int) event_name->slen;
   item_name = (STRING *) argv[2];
   lenit = (int) item_name->slen;
   pval = (int *) argv[3];
   xlate = (STRING *) argv[4];
   lenxl = (int) xlate->slen;

   ok = wind_tm_xlate_item_(pch, 
           event_name->s, 
           item_name->s, 
           pval,
           xlate->s,
           lenev,
           lenit,
           lenxl);

   return ok;
}


/*
   "realtime" wait on/off - provided for compatability with old programs
*/
int wind_tm_set_wait(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return wind_tm_set_messages_off_(pch);
}

int wind_tm_set_nowait(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;

   pch = (int *) argv[0];

   return wind_tm_set_messages_on_(pch);
}

/*
  old stream positioning routines
*/
int wind_tm_get_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_get_mfmf_(pch, pmjr, pmnr);
}

int wind_tm_get_stream_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_get_stream_mfmf_(pch, pmjr, pmnr);
}

int wind_tm_get_next_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_get_next_mfmf_(pch, pmjr, pmnr);
}

int wind_tm_get_earliest_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_get_earliest_mfmf_(pch, pmjr, pmnr);
}

int wind_tm_get_latest_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_get_latest_mfmf_(pch, pmjr, pmnr);
}


/*
  frame number manipulation routines
*/
int wind_tm_increment_packet(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_increment_packet_(pch, pmjr, pmnr);
}

int wind_tm_decrement_packet(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_decrement_packet_(pch, pmjr, pmnr);
}

int wind_tm_increment_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_increment_mfmf_(pch, pmjr, pmnr);
}

int wind_tm_decrement_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];

   return wind_tm_decrement_mfmf_(pch, pmjr, pmnr);
}
int wind_tm_delta_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr, *pmjr2;
   int *pmnr, *pmnr2;
   int *diff;

   pch   = (int *) argv[0];
   pmjr  = (int *) argv[1];
   pmnr  = (int *) argv[2];
   pmjr2 = (int *) argv[3];
   pmnr2 = (int *) argv[4];
   diff  = (int *) argv[5];

   return wind_tm_delta_mfmf_(pch, pmjr, pmnr, pmjr2, pmnr2, diff);
}

/*
  old get TM data routines
*/
int wind_tm_bit_rate(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   float *prate;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   prate = (float *) argv[3];

   return wind_tm_bit_rate_(pch, pmjr, pmnr, prate);
}

int wind_tm_get_word(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   int *pword;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pword = (int *) argv[3];

   return wind_tm_get_word_(pch, pmjr, pmnr, pword);
}

int wind_tm_get_minor_frame(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   int *pmnrfr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pmnrfr = (int *) argv[3];

   return wind_tm_get_minor_frame_(pch, pmjr, pmnr, pmnrfr);
}

int wind_tm_get_major_frame(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   int *pmjrfr;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pmjrfr = (int *) argv[3];

   return wind_tm_get_major_frame_(pch, pmjr, pmnr, pmjrfr);
}

int wind_tm_get_packet(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   int *ppkt;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   ppkt = (int *) argv[3];

   return wind_tm_get_packet_(pch, pmjr, pmnr, ppkt);
}

int wind_tm_get_hk(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *phkindex;
   int *phkword;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   phkindex = (int *) argv[2];
   phkword = (int *) argv[3];

   return wind_tm_get_hk_(pch, pmjr, phkindex, phkword);
}

int wind_tm_get_test(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   int *ptest;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   ptest = (int *) argv[3];

   return wind_tm_get_test_(pch, pmjr, pmnr, ptest);
}

int wind_tm_get_step(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   int *pstep;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pstep = (int *) argv[3];

   return wind_tm_get_step_(pch, pmjr, pmnr, pstep);
}

/*
   mfmf/scet conversion
*/
int wind_tm_scet_to_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   double *pur8time;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pur8time = (double *) argv[3];

   return wind_tm_scet_to_mfmf_(pch, pmjr, pmnr, pur8time);
}

int wind_tm_mfmf_to_scet(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   double *pur8time;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pur8time = (double *) argv[3];

   return wind_tm_mfmf_to_scet_(pch, pmjr, pmnr, pur8time);
}

int wind_tm_ert_to_mfmf(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   double *pur8time;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pur8time = (double *) argv[3];

   return wind_tm_ert_to_mfmf_(pch, pmjr, pmnr, pur8time);
}

int wind_tm_mfmf_to_ert(argc, argv)
   int argc;
   void *argv[];
{
   int *pch;
   int *pmjr;
   int *pmnr;
   double *pur8time;

   pch  = (int *) argv[0];
   pmjr = (int *) argv[1];
   pmnr = (int *) argv[2];
   pur8time = (double *) argv[3];

   return wind_tm_mfmf_to_ert_(pch, pmjr, pmnr, pur8time);
}
