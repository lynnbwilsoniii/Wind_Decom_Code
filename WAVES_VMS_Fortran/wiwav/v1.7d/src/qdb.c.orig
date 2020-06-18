/* qdb.c - report writing module of the Query Database program
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>
#include <time.h>

#include "qdb.h"

extern int get_ptr_to_item_attribute();
extern void wid_inorder_traverse2();
extern int get_items_for_event();
extern struct event_node *find_event_item_list();
extern int w_do_validation();
extern int wc_match_spec();

#if SYSTEM == SUNOS
#define W_version w_version_c_
#endif

#if SYSTEM == VMS
#define W_version w_version_c
#endif

extern int W_version();

/*                 1234567890123456 */
static char *tab1="  ";
static char *tab2="    ";
static char *tab3="      ";
static char *tab4="        ";

static int grab_flg=0;
static int show_flg=0;
Select *psel=NULL;
static int label2_idx;
static int label2_printed;
static char* event_name;
static int n_items_printed=0;

static FILE *of;
struct output_type_list {
   int type;
   char *label;
} otl[] = { FIXED_VALUE,      "CONSTANT",
            EXTRACT,          "TELEMETRY",
            PROCEDURE_NUMBER, "PHYSICAL",
            FILE_NAME,        "FILE",
            LOOKUP_TABLE,     "LOOKUP TABLE",
            CDF_ITEM,         "CDF",
            FUNCTION_NUMBER,  "FUNCTION",
            TEXT_ITEM,        "TEXT",
            0,                "\0" };

int is_selected_item(psni, item_name)
   struct same_name_item *psni;
   char *item_name;
{
   int ok;
   struct item_attributes *pia;

   if ((grab_flg     & psni->flags) == 0) return NO;
   if ((psel->flags  & psni->flags) == 0) return NO;
   if ((psel->flags3 & psni->flags) == 0) return NO;

   if (psel->flags2 == 0) return YES;

   /* match author date if selecting on author date */
   if ((psel->flags2 & V_AUTHOR_DATE) != 0) 
   {
      if ((psni->flags  & AUTHOR_DATE)  == 0) return NO;
      ok = get_ptr_to_item_attribute(&pia, psni, AUTHOR_DATE);
      if (ok != OK) return NO;
      ok = strncmp(pia->u.author_date, psel->v.auth_date, 8);
      if (ok <= 0) return NO;
   }

   /* match extract area if selecting on extract area */
   if ((psel->flags2 & V_AREA) != 0)
   {
      if ((psni->flags & EXTRACT) == 0) return NO;
      ok = get_ptr_to_item_attribute(&pia, psni, EXTRACT);
      if (ok != OK) return NO;
      if (psel->v.area != pia->u.pe->area) return NO;
   }

   /* match dpu version number if selecting on this criteria */
   if (((psel->flags2 & V_DPU_VERSION) != 0) &&
       ((psni->flags & VALIDATION)     != 0) )
   {
      /* loop over all the validation structures */
      pia = psni->head;
      while (pia != NULL)
      {
         if (pia->kind_of == VALIDATION)
         {
            if (strcmp("DPU_VERSION", pia->u.pv->name) == 0)
            {
               ok = w_do_validation(&(psel->v.dpu_ver), pia->u.pv);
               if (ok != OK) return NO;
            }
         }
         pia = pia->next;
      }
   }

   /* match fft version number if selecting on this criteria */
   if (((psel->flags2 & V_FFT_VERSION) != 0) &&
       ((psni->flags & VALIDATION)     != 0) )
   {
      /* loop over all the validation structures */
      pia = psni->head;
      while (pia != NULL)
      {
         if (pia->kind_of == VALIDATION)
         {
            if (strcmp("FFT_VERSION", pia->u.pv->name) == 0)
            {
               ok = w_do_validation(&(psel->v.fft_ver), pia->u.pv);
               if (ok != OK) return NO;
            }
         }
         pia = pia->next;
      }
   }

   /* match tds version number if selecting on this criteria */
   if (((psel->flags2 & V_TDS_VERSION) != 0) &&
       ((psni->flags & VALIDATION)     != 0) )
   {
      /* loop over all the validation structures */
      pia = psni->head;
      while (pia != NULL)
      {
         if (pia->kind_of == VALIDATION)
         {
            if (strcmp("TDS_VERSION", pia->u.pv->name) == 0)
            {
               ok = w_do_validation(&(psel->v.tds_ver), pia->u.pv);
               if (ok != OK) return NO;
            }
         }
         pia = pia->next;
      }
   }

   /* match subtype value if selecting on this criteria */
   if (((psel->flags2 & V_SUBTYPE)     != 0) &&
       ((psni->flags & VALIDATION)     != 0) )
   {
      /* loop over all the validation structures */
      pia = psni->head;
      while (pia != NULL)
      {
         if (pia->kind_of == VALIDATION)
         {
            if (strcmp("PACKET_SUBTYPE", pia->u.pv->name) == 0)
            {
               ok = w_do_validation(&(psel->v.subtype), pia->u.pv);
               if (ok != OK) return NO;
            }
         }
         pia = pia->next;
      }
   }

   /* match validity date/time if selecting on this criteria */
   if (((psel->flags2 & V_DATE   )     != 0) &&
       ((psni->flags & VALIDATION)     != 0) )
   {
      /* loop over all the validation structures */
      pia = psni->head;
      while (pia != NULL)
      {
         if (pia->kind_of == VALIDATION)
         {
            if (strcmp("EVENT_SCET", pia->u.pv->name) == 0)
            {
               ok = w_do_validation(&(psel->v.ymdhms[0]), pia->u.pv);
               if (ok != OK) return NO;
            }
         }
         pia = pia->next;
      }
   }

   /* match the string item name if selecting on item name */
   if ((psel->flags2 & V_ITEM_NAME) != 0) 
   {
      if ((psel->flags2 & V_WILD_NAME) == 0)
      {
         ok = strcmp(item_name, psel->v.item_name);
         if (ok != 0) return NO;
      }
      else
      {
         ok = wc_match_spec(psel->v.item_name, item_name);
         if (ok == 0) return NO;
      }
   }

   return YES;
}

void qdb_show_item_attribute( pia, psni )
   struct item_attributes *pia;
   struct same_name_item *psni;
{
   xlate_item *xli;
   struct function_arg_list *pfal;
   int i,j,k,m,n;
   struct string_list *psl;

   if (pia == NULL) return;

   switch (pia->kind_of & psel->flags4)
   {
   case 0:
   case DESCRIPTION:
      break;
   case EXTRACT:
      fprintf(of, "%25.sEXTRACT=%d %d %d %d %d Area=%c  flags=%0X.\n",
         tab3,
         pia->u.pe->startbit,
         pia->u.pe->offset,
         pia->u.pe->rep,
         pia->u.pe->bitlen,
         pia->u.pe->count,
         pia->u.pe->area,
         pia->u.pe->flags);
      for (i=0; i<5; i++) 
      {
         if (pia->u.pe->expr[i] != NULL)
            fprintf(of, "%25.s[%d] %s\n", tab3, i+1, pia->u.pe->expr[i]);
      }
      break;
   case FIXED_VALUE:
      j = pia->u.pfv->value_count;
      fprintf(of, "%25.sFIXED_VALUE=%d: ", tab3, j);
      if (pia->u.pfv->expr != NULL)
      {
         fprintf(of, "%s\n", pia->u.pfv->expr);
         break;
      }
      i = psni->flags & TYPE_RETURN_MASK;
      switch (i)
      {
      case INT_RETURN:
         for (k=0; k<j; k++) fprintf(of, "%d ", pia->u.pfv->u.ivals[k]);
         break;
      case FLOAT_RETURN:
         for (k=0; k<j; k++) fprintf(of, "%g ", (double) pia->u.pfv->u.fvals[k]);
         break;
      case DOUBLE_RETURN:
         for (k=0; k<j; k++) fprintf(of, "%g ", pia->u.pfv->u.dvals[k]);
         break;
      case CHAR_RETURN:
         fprintf(of, "%s.", pia->u.pfv->u.cvals);
         break;
      default:
         fprintf(of, "UNKNOWN data type: 0x%x.", i);
         break;
      }
      fprintf(of, "\n");
      break;
   case VALIDATION:
      fprintf(of, "%25.sVALIDATION=%s, v1=%d, v2=%d, op=%d.\n", 
         tab3,
         pia->u.pv->name, 
         pia->u.pv->val1, 
         pia->u.pv->val2, 
         pia->u.pv->operation);
      break;
   case AUTHOR_DATE:
      fprintf(of, "%25.sAuthored %s.\n", tab3, pia->u.author_date);
      break;
   case COMPOSITE:
      fprintf(of, "%25.sCOMPOSITE=%s.\n", tab3, pia->u.chain);
      break;
   case FILE_NAME:
      fprintf(of, "%25.sFILE_NAME=%s.\n", tab3, pia->u.myfile);
      break;
   case TEXT_ITEM:
      fprintf(of, "%25.sTEXT: %s.\n", tab3, pia->u.text);
      break;
   case COUNT_ITEM:
      fprintf(of, "%25.sCOUNT_ITEM=%s.\n", tab3, pia->u.count);
      break;
   case FUNCTION_NUMBER:
      fprintf(of, "%25.sFUNCTION_NUMBER=%d.\n", tab3, pia->u.pfun->function_number);
      pfal = pia->u.pfun->head;
      while (pfal != NULL) 
      {
         fprintf(of, "%25.sArgument: %s.\n", tab3, pfal->pc);
         pfal = pfal->next;
      }
      break;
   case XLATE_PRESENT:
      xli = pia->u.xlate_ptr;
      while (xli != NULL)
      {
         fprintf(of, "%25.sXLATE=%s. %d.\n", tab3, xli->str, xli->val);
         xli = xli->next;
      }
      break;
   case CDF_ITEM:
      fprintf(of, "%25.siRv: %s", tab3, pia->u.pcdf->independent_rv);
      fprintf(of, "  %d,%d,%d,%d,%d,%d,%d\n",
          pia->u.pcdf->independent_rv_indices[0],
          pia->u.pcdf->independent_rv_indices[1],
          pia->u.pcdf->independent_rv_indices[2],
          pia->u.pcdf->independent_rv_indices[3],
          pia->u.pcdf->independent_rv_indices[4],
          pia->u.pcdf->independent_rv_indices[5],
          pia->u.pcdf->independent_rv_indices[6]);
      fprintf(of, "%25.swlv: %s\n", tab3, pia->u.pcdf->independent_rv_match);
      fprintf(of, "%25.sdRv: %s", tab3, pia->u.pcdf->dependent_rv);
      fprintf(of, "  %d,%d,%d,%d,%d,%d,%d\n",
          pia->u.pcdf->dependent_rv_indices[0],
          pia->u.pcdf->dependent_rv_indices[1],
          pia->u.pcdf->dependent_rv_indices[2],
          pia->u.pcdf->dependent_rv_indices[3],
          pia->u.pcdf->dependent_rv_indices[4],
          pia->u.pcdf->dependent_rv_indices[5],
          pia->u.pcdf->dependent_rv_indices[6]);
      psl = pia->u.pcdf->file_list;
      while (psl != NULL)
      {
         fprintf(of, "%25.s%s\n", tab3, psl->str);
         psl = psl->next;
      }
      break;
   default:
      printf("%25.s******UNKNOWN kind_of=%d\n", tab3, pia->kind_of);
      break;
   }

   return;
}

void qdb_show_item( psni, name, nth, name_printed )
   struct same_name_item *psni;
   char *name;
   int nth;
   int *name_printed;
{
   struct item_attributes *pia;
   int ok;
   int i;

   n_items_printed++;

   if (label2_printed == NO)
   {
      fprintf(of, "%s\n", event_name);
      label2_printed = YES;
      fprintf(of, "%s%s\n", tab1, otl[label2_idx].label);
   }

   if (*name_printed == NO)
   {
      *name_printed = YES;
      if (nth == 0)
         fprintf(of, "%s%-20.64s", tab2, name);
      else
         fprintf(of, "%s%-20.64s %d)", tab2, name, nth+1);
   }
   else
      fprintf(of, "%s%19d)", tab2, nth+1);

   /* first print the descriptions */
   ok = get_ptr_to_item_attribute(&pia, psni, DESCRIPTION);
   if (ok != OK) fprintf(of, " [no description available]\n");
   else 
   {
      fprintf(of, " %.80s\n", pia->u.desc);
      pia = pia->next;
      while (pia != NULL)
      {
         if (pia->kind_of == DESCRIPTION)
            fprintf(of, "%s%20.s %.80s\n", tab2, tab2, pia->u.desc);
         pia = pia->next;
      }
   }

   /* next print the selected attributes, if any... */
   pia = psni->head;
   while (pia != NULL)
   {
      qdb_show_item_attribute( pia, psni );
      pia = pia->next;
   }
   /* print data type if flagged to do so */
   i = psel->flags4 & TYPE_RETURN_MASK;
   if (i != 0)
   {
      switch(TYPE_RETURN_MASK & psni->flags)
      {
      case DOUBLE_RETURN:
         fprintf(of, "%25.sREAL*8\n", tab3);
         break;
      case FLOAT_RETURN:
         fprintf(of, "%25.sREAL*4\n", tab3);
         break;
      case INT_RETURN:
         fprintf(of, "%25.sINTEGER*4\n", tab3);
         break;
      case CHAR_RETURN:
         fprintf(of, "%25.sCHARACTER\n", tab3);
         break;
      default:
         fprintf(of, "%25.sUnknown data type\n", tab3);
         break;
      }
   }
   return;
}

void qdb_item_node_visit(pin)
   struct item_node *pin;
{
   struct same_name_item *psni;
   int i;
   int have_shown_item_name=NO;

   psni = pin->list;
   i = 0;
   while (psni != NULL)
   {
      if (is_selected_item(psni, pin->item_name) == YES) 
         qdb_show_item( psni, pin->item_name, i, &have_shown_item_name);
      ++i;
      psni = psni->next;
   }
}

static void write_titles()
{
   int i,k;
   char ver[32];
   time_t t;
   struct tm *ptm;
   char str[32];
   char *pc;

   t = time(NULL);
   ptm = localtime(&t);
   i = strftime(str, sizeof(str), "%A, %B %d, %Y", ptm);
   fprintf(of, "\nThis report generated on %s.\n", str);

   i = sizeof(ver);
   W_version(ver,&i);
   pc = strstr(ver, "  ");
   if (pc != NULL) *pc = '\0';
   fprintf(of, "Using WindLib version %s.\n\n", ver);

   if ((psel->flags2 & V_DATE) == 0)
      fprintf(of, "No constraint was put on the date of validity.\n");
   else
      fprintf(of, "Items selected are valid on date %d.\n", 
         psel->v.ymdhms[0]);

   if ((psel->flags2 & V_DPU_VERSION) == 0)
      fprintf(of, "No constraint was put on the DPU version number.\n");
   else
      fprintf(of, "Items selected are valid for DPU version number %d.\n", 
         psel->v.dpu_ver);

   if ((psel->flags2 & V_FFT_VERSION) == 0)
      fprintf(of, "No constraint was put on the FFT version number.\n");
   else
      fprintf(of, "Items selected are valid for FFT version number %d.\n", 
         psel->v.fft_ver);

   if ((psel->flags2 & V_TDS_VERSION) == 0)
      fprintf(of, "No constraint was put on the TDS version number.\n");
   else
      fprintf(of, "Items selected are valid for TDS version number %d.\n", 
         psel->v.tds_ver);

   if ((psel->flags2 & V_SUBTYPE) == 0)
      fprintf(of, "No constraint was put on subtype.\n");
   else
   {
      for (k=0, i = psel->v.subtype; i > 0; i = i >> 1, k++);
      fprintf(of, "Items selected are valid for subtype %d (%d, %xX).\n", 
         k, psel->v.subtype, psel->v.subtype);
   }

   if ((psel->flags2 & V_AUTHOR_DATE) == 0)
      fprintf(of, "No constraint was put on the author date.\n");
   else
      fprintf(of, "Items selected were authored on or after %s.\n", psel->v.auth_date);

   if ((psel->flags2 & V_AREA) == 0)
      fprintf(of, "No constraint was put on the telemetry area.\n");
   else
      fprintf(of, "Items selected are valid for telemetry area %c.\n",
         psel->v.area);

   if ((psel->flags2 & V_ITEM_NAME) != 0)
      fprintf(of, "Items selected were named %s.\n", psel->v.item_name);

   fprintf(of, "\n");
   return;
}

static void set_output_stream()
{
   if (psel->f[0] <= ' ') of = stdout;
   else
   {
      of = fopen(psel->f, "w");
      if (of == NULL)
      {
         perror("Output file open error");
         of = stdout;
      }
   }
   return;
}

int write_report(arg_psel)
   Select *arg_psel;
{
   char *rn="write_report";
   int i;
   int ok;
   struct event_node *pen;

   show_flg = 0;
   psel = arg_psel;

   set_output_stream();
   
   write_titles();

   for (i=0; i < MAX_N_EV && i < psel->n_ev; i++)
   {
      event_name = &(psel->ev[i][0]);
      if (*event_name == '\0') break;
      /* acquire the items associated with the named event */
      ok = get_items_for_event(event_name, YES);
      if (ok != OK)
      {
         printf("%s: cannot read items for event %s.\n", rn, event_name);
         continue;
      }
      pen = find_event_item_list(event_name);
      if (pen == NULL)
      {
         printf("%s: cannot find event item list for %s.\n", rn, event_name);
         continue;
      }
      for (label2_idx=0; otl[label2_idx].type != 0; label2_idx++)
      {
         grab_flg = otl[label2_idx].type;
         label2_printed = NO;
         wid_inorder_traverse2(pen->head, qdb_item_node_visit);
      }
   }

   printf("\n  %d items selected.\n", n_items_printed);

   return OK;
}
