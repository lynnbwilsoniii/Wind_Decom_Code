/* qdb_args.c - query database Unix style interface
*/
#define QDB_VERSION "v1.0"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "qdb.h"

static enum { S_no_state,
       S_get_event,		/* -e rad1 rad2 ffth  */
       S_get_item,		/* -i wind_mfi*       */
       S_get_area,		/* -a 6               */
       S_get_subtype,		/* -su 4              */
       S_get_author_date,	/* -da yyyymmdd       */
       S_get_validity_date,	/* -dv yyyymmdd       */
       S_get_dpu_version,       /* -vdpu 5            */
       S_get_fft_version,       /* -vfft 2            */
       S_get_tds_version,       /* -vtds 4            */
       S_get_output_file,       /* -o filename        */
       S_get_kind,              /* -k telem cdf const */
       S_get_data_type,         /* -t int f d         */
       S_get_help,              /* -h                 */
       S_get_shows,             /* -sh xlate fmt all author val ... */
       S_error};                /* command line input broke a rule */


static void show_help2()
{
   printf("\n");
   printf(" Wind/Waves Query Item Database Utility, Version %s\n", QDB_VERSION);
   printf("\n");
   printf("   Example 1, list all the CDF items: \n");
   printf("              qdb -k cdf \n");
   printf("\n");
   printf("   Example 2, list all the MFI CDF items with definitions: \n");
   printf("              qdb -k cdf -sh cdf -i \\*MFI\\* \n");
   printf("\n");
   printf("   Example 3, list all items created on or after 01-JUN-1996:\n");
   printf("              qdb -da 19960601 \n");
   printf("\n");
   printf("   Example 4, list all TNR and RAD2 items valid on 01-FEB-1995:\n");
   printf("              qdb -e RAD2 TNR -dv 19950201 \n");
   printf("\n");
   printf("   Example 5, list constants from the global item database:\n");
   printf("              qdb -e global -k const \n");
   printf("\n");
   return;
}

static void show_help()
{
   printf("\n");
   printf(" Syntax:\n");
   printf("\n");
   printf("   qdb [-h [help]] \n");
   printf("       [-a area] \n");
   printf("       [-da yyyymmdd.author] \n");
   printf("       [-dv yyyymmdd.validity]  \n");
   printf("       [-e event [event...]] \n");
   printf("       [-i itemname] \n");
   printf("       [-kind [telem] [cdf] [lookup] [const] [file] [func] [validation]]\n");
   printf("       [-o outputfile] \n");
   printf("       [-show [xlate] [fmt] [author] [val] [cdf] [telem] [all]] \n");
   printf("       [-subtype int] \n");
   printf("       [-type [integer] [float] [double] [char]] \n");
   printf("       [-vdpu dpuver] [-vfft fftver] [-vtds tdsver] \n");
   printf("\n");
   printf(" Only the first one or two characters of any option are significant.\n");
   printf("\n");
   return;
}

static int set_state( pc )
   char *pc;
{
   switch(*++pc)
   {
   case 'a': return S_get_area;        break;
   case 'd': 
      switch(*++pc)
      {
      case 'a': return S_get_author_date;   break;
      case 'v': return S_get_validity_date; break;
      default:  return S_error;             break;
      }
      break;
   case 'e': return S_get_event;       break;
   case 'h': return S_get_help;        break;
   case 'i': return S_get_item;        break;
   case 'k': return S_get_kind;        break;
   case 'o': return S_get_output_file; break;
   case 's': 
      switch(*++pc)
      {
      case 'h': return S_get_shows;       break;
      case 'u': return S_get_subtype;     break;
      default:  return S_error;           break;
      }
      break;
   case 't': return S_get_data_type;   break;
   case 'v':
      switch(*++pc)
      {
      case 'd': return S_get_dpu_version; break;
      case 'f': return S_get_fft_version; break;
      case 't': return S_get_tds_version; break;
      default:  return S_error;           break;
      }
      break;
   default:
      break;
   }
   return S_error;
}

static int fill_event_name_list( psel )
   Select *psel;
{
   int ok;
   int n_el, sz_el, n, flags;
/*
   n     = 0;
   flags = 0;
   n_el  = MAX_N_EV;
   sz_el = MAX_EV_LEN;
   ok = w_get_file_list_from_dir("WIND_DATA","items_*.db",
   psel->ev, &n_el, &sz_el, &n, &flags);
*/
   strcpy(psel->ev[0],  "TDSS");
   strcpy(psel->ev[1],  "TDSF");
   strcpy(psel->ev[2],  "FFT");
   strcpy(psel->ev[3],  "FFTL");
   strcpy(psel->ev[4],  "FFTM");
   strcpy(psel->ev[5],  "FFTH");
   strcpy(psel->ev[6],  "RAD1");
   strcpy(psel->ev[7],  "RAD2");
   strcpy(psel->ev[8],  "TNR");
   strcpy(psel->ev[9],  "TNRO");
   strcpy(psel->ev[10], "GLOBAL");
   strcpy(psel->ev[11], "DUMP");
   strcpy(psel->ev[12], "HK");
   psel->n_ev = 13;

   return OK;
}

int qdb_args(argc, argv, psel)
   int argc;
   void *argv[];
   Select *psel;
{
   char *rn="qdb_args";
   int i,j,k;
   int state;
   char *pc, *ps;

   if (argc <= 1) {show_help(); return ERR;}

   state = S_no_state;

   for (j=1; j<argc; ++j)
   {
      pc = argv[j];
/*
      printf("Arg %d is %s.\n", j, pc);
*/

      if (*pc == '-') state = set_state(pc);
      else
      switch(state)
      {
      case S_get_output_file:
         strcpy(psel->f, pc);
         break;
      case S_get_author_date:
         strcpy(psel->v.auth_date, pc);
         psel->flags2 = psel->flags2 | V_AUTHOR_DATE;
         break;
      case S_get_validity_date:
         psel->v.ymdhms[0] = atoi(pc);
         psel->v.ymdhms[1] = 0;
         psel->flags2 = psel->flags2 | V_DATE;
         break;
      case S_get_dpu_version:
         psel->v.dpu_ver = atoi(pc);
         psel->flags2 = psel->flags2 | V_DPU_VERSION;
         break;
      case S_get_fft_version:
         psel->v.fft_ver = atoi(pc);
         psel->flags2 = psel->flags2 | V_FFT_VERSION;
         break;
      case S_get_tds_version:
         psel->v.tds_ver = atoi(pc);
         psel->flags2 = psel->flags2 | V_TDS_VERSION;
         break;
      case S_get_area:
         psel->v.area = *pc;
         psel->flags  = psel->flags | EXTRACT;
         psel->flags2 = psel->flags2 | V_AREA;
         break;
      case S_get_subtype:
         i = atoi(pc);
         k = 1;
         psel->v.subtype = k << (i-1);
         psel->flags2 = psel->flags2 | V_SUBTYPE;
         break;
      case S_get_item:
         for (ps=pc; *ps > ' '; ++ps) *ps = toupper(*ps);
         strcpy(psel->v.item_name, pc);
         psel->flags2 = psel->flags2 | V_ITEM_NAME;
         if (strchr(pc,'*') != NULL)
            psel->flags2 = psel->flags2 | V_WILD_NAME;
         break;
      case S_get_event:
         for (ps=pc; *ps > ' '; ++ps) *ps = toupper(*ps);
         i = psel->n_ev;
         strcpy(&(psel->ev[i][0]), pc);
         psel->n_ev++;
         break;
      case S_get_data_type:
         switch(*pc)
         {
            case 'i': psel->flags3 = psel->flags3 | INT_RETURN;    break;
            case 'd': psel->flags3 = psel->flags3 | DOUBLE_RETURN; break;
            case 'f': psel->flags3 = psel->flags3 | FLOAT_RETURN;  break;
            case 'c': psel->flags3 = psel->flags3 | CHAR_RETURN;   break;
            default: goto err05; break;
         }
         break;
      case S_get_kind:
         switch(*pc)
         {
            case 'c':
               switch(*++pc)
               {
               case 'o': psel->flags = psel->flags | FIXED_VALUE;  break;
               case 'd': psel->flags = psel->flags | CDF_ITEM;     break;
               default: goto err04; break;
               }
               break;
            case 'f':
               switch(*++pc)
               {
               case 'i': psel->flags = psel->flags | FILE_NAME;       break;
               case 'u': psel->flags = psel->flags | FUNCTION_NUMBER; break;
               default: goto err04; break;
               }
               break;
            case 't': psel->flags = psel->flags | EXTRACT;      break;
            case 'l': psel->flags = psel->flags | LOOKUP_TABLE; break;
            case 'v': psel->flags = psel->flags | VALIDATION;   break;
            default: goto err04; break;
         }
         break;
      case S_get_shows:
         switch(*pc)
         {
            case 'x': psel->flags4 = psel->flags4 | XLATE_PRESENT; break;
            case 'f': 
               switch(*++pc)
               {
               case 'u': psel->flags4 = psel->flags4 | FUNCTION_NUMBER; break;
               case 'o': psel->flags4 = psel->flags4 | FORMAT;          break;
               case 'i': psel->flags4 = psel->flags4 | FILE_NAME;       break;
                  break;
               default: goto err03; break;
               }
               break;
            case 'a': 
               switch(*++pc)
               {
               case 'u': psel->flags4 = psel->flags4 | AUTHOR_DATE;   break;
               case 'l': psel->flags4 = psel->flags4 | CDF_ITEM |
                         XLATE_PRESENT | FORMAT | AUTHOR_DATE |
                         EXTRACT | VALIDATION | FIXED_VALUE | FILE_NAME |
                         FUNCTION_NUMBER | TEXT_ITEM | COMPOSITE ;
                  break;
               default: goto err03; break;
               }
               break;
            case 'e': psel->flags4 = psel->flags4 | EXTRACT;       break;
            case 't': psel->flags4 = psel->flags4 | EXTRACT;       break;
            case 'v': psel->flags4 = psel->flags4 | VALIDATION;    break;
            case 'c': 
               switch(*++pc)
               {
               case 'o': psel->flags4 = psel->flags4 | FIXED_VALUE; break;
               case 'd': psel->flags4 = psel->flags4 | CDF_ITEM;    break;
               default: goto err03; break;
               }
               break;
            default: goto err02; break;
         }
         break;
      default: goto err01; break;
      }
      if (state == S_get_help) {show_help2(); return ERR;}
      if (state == S_error) goto err01;
   }

   if (psel->flags == 0) psel->flags = EXTRACT | FIXED_VALUE | FILE_NAME |
      FUNCTION_NUMBER | CDF_ITEM | VALIDATION | LOOKUP_TABLE ;

   if (psel->flags3 == 0) psel->flags3 = INT_RETURN | FLOAT_RETURN |
      DOUBLE_RETURN | CHAR_RETURN;

   if (psel->n_ev == 0)  return fill_event_name_list(psel);

/*
printf("flags  = %0X\n", psel->flags);
printf("flags2 = %0X\n", psel->flags2);
printf("flags3 = %0X\n", psel->flags3);
printf("flags4 = %0X\n", psel->flags4);
*/
   return OK;

err01:
printf("%s: illegal parameter or option: \"%s\"\n", rn, pc);
return ERR;

err02:
printf("%s: illegal data type specified: %s\n", rn, pc);
return ERR;

err03:
printf("%s: illegal item show specified: %s\n", rn, pc);
return ERR;

err04:
printf("%s: illegal item kind specified: %s\n", rn, pc);
return ERR;

err05:
printf("%s: illegal data type specified: %s\n", rn, pc);
return ERR;

}
