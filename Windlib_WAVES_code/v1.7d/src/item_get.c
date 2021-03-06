/* item_get.c -- source for w_really_get_item, which is where the
   item-getting logic really is (to reap the benefits of a recursive C language)

   dbms4 Version
*/

#include "wind_os_def.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
/*#include "wind_tm_event_def.h"*/
#include "item.h"

#if SYSTEM == SUNOS
#include <memory.h>
#define cp_bits_to_lw copy_bits_to_longword_
#define wind_supprs_int_msg wind_suppress_internal_msgs_
#define w_get_usr_struc_addr w_get_addr_of_user_struct_
#define phys_units_mstr_cyl w_physical_units_r4_

#ifdef CDF_LIB_USEABLE
#define wind_get_cdf_thing w_cdf_get_
#else
#define wind_get_cdf_thing w_cdf_dummy_
#endif

#else

#ifdef USE_MACOSX

#include <memory.h>
#define cp_bits_to_lw copy_bits_to_longword_
#define wind_supprs_int_msg wind_suppress_internal_msgs_
#define w_get_usr_struc_addr w_get_addr_of_user_struct_
#define phys_units_mstr_cyl w_physical_units_r4_

#ifdef CDF_LIB_USEABLE
#define wind_get_cdf_thing w_cdf_get_
#else
#define wind_get_cdf_thing w_cdf_dummy_
#endif

#else

#define cp_bits_to_lw copy_bits_to_longword
#define wind_supprs_int_msg wind_suppress_internal_msgs
#define w_get_usr_struc_addr w_get_addr_of_user_struct
#define phys_units_mstr_cyl w_physical_units_r4
#define wind_get_cdf_thing w_cdf_get

#endif

#endif



extern int cpy_bits2();                   /* C function       */
extern int cp_bits_to_lw();               /* FORTRAN function */
extern int wind_supprs_int_msg();         /* FORTRAN function */
extern int w_get_usr_struc_addr();        /* FORTRAN function */
extern int phys_units_mstr_cyl();         /* FORTRAN function */
extern int wind_get_cdf_thing();          /* FORTRAN function */

extern struct event_node *find_event_item_list();
extern int w_get_item_list();
#ifdef USE_MACOSX
int w_really_get_item_();
#else
int w_really_get_item();
#endif
static int w_generate_aux_args();
extern int call_function_item();
extern int get_ptr_to_item_attribute();
extern int w_item_expr_eval();
extern int w_item_expr_var_resolve();
extern int wc_msg();
extern int w_optimize_extract();

static int  wind_get_table_name();
static int  w_read_tbl_file();
static void strcpy_nowhite();
static int  w_mk_cdf_arg_ls_and_get();
int  w_do_validation();

#define w_ungettable_vldtn_item  500		/* get_item return codes      */
#define w_item_validation_fail	 502
#define w_bad_validation_op	 504
#define w_no_1st_header_for_item 510
#define w_no_2nd_header_for_item 512
#define w_no_3rd_header_for_item 514
#define w_no_4th_header_for_item 516
#define w_no_data_area_for_item	 518
#define w_invalid_item_area	 520
#define w_bad_table_name	 530
#define w_bad_table_file   	 532
#define w_ungettable_cmpsit_item 540
#define w_ungettable_count_item  542
#define	w_ungettable_function_item 544
#define w_item_name_not_found    546
#define w_invalid_data_type      548

#define OK 1
#define ERR 0
#define YES 1
#define NO 0
#define MY_MAX_COUNT 0x7fffffff

static int show_validation_failures=NO;

static int bit_extract_method=2;

struct ieee_low_byte {
   union {
      unsigned int i4;
      unsigned char uc[4];
      struct my_bytes_ieee {
           unsigned char b3, b2, b1, b0;
      } b;
   } u;
};

struct vax_low_byte {
   union {
      unsigned int i4;
      int i;
      unsigned char uc[4];
      struct my_bytes_vax {
           unsigned char b0, b1, b2, b3;
      } b;
   } u;
};

int w_set_bit_extract_method( val )
   int *val;
{
   if (*val == 2) bit_extract_method = 2;
   else bit_extract_method = 1;
   return 1;
}

/* implement a simple hash table for event/item names */

#define HASH_TBL_SZ 103
#define hshfun(n) (n % HASH_TBL_SZ)
#define hshsum(i,j,a,f)  for (i=j=0, a=f; *a > ' '; a++) j = j + *a;
/*
#define hshsum(i,j,a,f)  for (i=j=0, a=f; *a > ' '; i++, a++) \
   j = i < 11 ? j + ((int) *a << i) : j + *a;
*/
#define IHT_NOT_FOUND 0
#define IHT_FOUND 1
#define IHT_NO_SLOT 2
#define IHT_ERROR 3

struct item_hash_table_element {
   char *event;
   char *item;
   struct item_node *pin;
};
static struct item_hash_table_element iht[HASH_TBL_SZ];
static int is_iht_initialized=NO;
static int iht_hits;
static int iht_misses;
static int iht_errors;
static int iht_count;

static int w_iht_initialize()
{
   int i;

   is_iht_initialized = YES;
   for (i=0; i < HASH_TBL_SZ; i++)
   {
      iht[i].event = NULL;
      iht[i].item = NULL;
      iht[i].pin = NULL;
   }
   iht_hits = 0;
   iht_misses = 0;
   iht_errors = 0;
   iht_count = 0;
   return 1;
}

int w_iht_show_hash_tbl_stats()
{
   printf("Hits: %d, Misses: %d, Errors: %d, Count: %d.\n",
      iht_hits, iht_misses, iht_errors, iht_count);
   return 1;
}

static int w_iht_get(event, item, ppin)
   char *event;
   char *item;
   struct item_node **ppin;
{
   int i,j,k,n;
   char *a;

   if (is_iht_initialized==NO) w_iht_initialize();
   hshsum(n,i,a,event);
   hshsum(n,j,a,item);
   k = i + j;
   i = hshfun(k);
   if (iht[i].pin != NULL)
      if (strcmp(event, iht[i].event) == 0)
         if (strcmp(item, iht[i].item) == 0)
         {
            *ppin = iht[i].pin;
            iht_hits++;
            return IHT_FOUND;
         }
         else {iht_misses++; return IHT_NO_SLOT;}
      else {iht_misses++; return IHT_NO_SLOT;}

   iht_misses++;
   return IHT_NOT_FOUND;
}

static int w_iht_put(event, pin)
   char *event;
   struct item_node *pin;
{
   int i,j,k,n;
   char *a;
   struct event_node *pen;

   if (is_iht_initialized==NO) w_iht_initialize();
   hshsum(n,i,a,event);
   hshsum(n,j,a,pin->item_name);
   k = i + j;
   i = hshfun(k);

   if (iht[i].pin == NULL && iht[i].event == NULL && iht[i].item == NULL)
   {
      pen = find_event_item_list(event);
      if (pen == NULL) {iht_errors++; return IHT_ERROR;}
      iht_count++;
      iht[i].event = pen->event_name;
      iht[i].pin = pin;
      iht[i].item = pin->item_name;
      return IHT_FOUND;
   }

   return IHT_NO_SLOT;
}

static int w_locate_item( ch, event_name, item_name, arg_psni )
   int ch;
   char *event_name;
   char *item_name;
   struct same_name_item **arg_psni;
{
   char *rn="W_LOCATE_ITEM";
   int ok;
   struct item_node *pin;
   struct same_name_item *psni;
   struct same_name_item *psni2;
   struct item_attributes *pia;
   int is_valid;
   int my_ret_size;
   int my_bit_start=0;
   int my_ret_type;
   int mya[2];
   int iht_status;

   *arg_psni = NULL;

   if (IHT_FOUND != (iht_status = w_iht_get(event_name, item_name, &pin)))
   {
      ok = w_get_item_list(event_name, item_name, &pin);
      if (ok != OK) goto unfindable_item_name;
      if (iht_status == IHT_NOT_FOUND)
      {
         ok = w_iht_put(event_name, pin);
      }
   }
   psni = pin->list;
   pia = psni->head;

   /* if this item has validations, do them now, trying successive
      same-named items from the list for failures
   */
   is_valid = YES;
   while ((psni->flags & VALIDATION) != 0)
   {
      is_valid = YES;
      while (pia != NULL)
      {
         if ((pia->kind_of & VALIDATION) != 0)
         {
            mya[0] = 0;
            mya[1] = 0;
            my_bit_start = 0;
            my_ret_size = 0;
            my_ret_type = INT_RETURN;
#ifdef USE_MACOSX
            ok = w_really_get_item_(ch,event_name,pia->u.pv->name, 
#else
            ok = w_really_get_item(ch,event_name,pia->u.pv->name, 
#endif
                 mya,2, &my_ret_size, &my_bit_start, &my_ret_type, &psni2);
            if (ok == w_item_name_not_found) goto unfindable_validation;
            if (ok != 1) goto other_get_item_err;
            if (my_ret_type != INT_RETURN) goto bad_data_type;
            if (ok != OK) {is_valid = NO; break;}
            ok = w_do_validation(mya,pia->u.pv);
            if (ok != OK) {is_valid = NO; break;}
         }
         pia = pia->next;
      }
      if (is_valid == YES) break;
      if (is_valid != YES) psni = psni->next;
      if (psni == NULL) break;
      pia = psni->head;
   }
   if (is_valid != YES) return w_item_validation_fail;
   *arg_psni = psni;
   return OK;

unfindable_item_name:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s\n",
            rn,
            "cannot get item entry for: ",
            item_name);
  return w_item_name_not_found;

other_get_item_err:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s, ok=%d.\n",
            rn,
            "cannot get item entry for: ",
            item_name);
  return w_item_name_not_found;

unfindable_validation:
   if (!wind_supprs_int_msg())
      printf("%s: %s\n	item=%s, validation=%s\n",
	       rn,
               "cannot get item validation entry, ",
	       item_name,
               pia->u.pv->name);
   return w_ungettable_vldtn_item;

bad_data_type:
   if (!wind_supprs_int_msg())
      printf("%s: %s\n	item=%s, validation=%s\n",
	       rn,
               "cannot get item validation entry (bad data type), ",
	       item_name,
               pia->u.pv->name);
   return w_ungettable_vldtn_item;

}

/*--------------------------------------------- w_really_get_item ------*/
#ifdef USE_MACOSX
int w_really_get_item_
#else
int w_really_get_item
#endif
  (ch, event_name, item_name, a, size, ret_size, bit_start, ret_type, ret_psni)
  int  ch             ;	/* user's channel number			*/
  char *event_name    ;	/* character string event type			*/
  char *item_name     ; /* item name as a string			*/
  int  a[]            ;	/* user's storage area (buffer)			*/
  int  size           ;	/* size of user's buffer in data type units	*/
  int  *ret_size      ;	/* # of type units written to user's area	*/
  int  *bit_start     ; /* starting bit position for composite items	*/
  int  *ret_type      ; /* data type of [buffer on input, item on ret]	*/
  struct same_name_item **ret_psni; /* for caller's item aux info call  */
{
  /*const*/ char *rn="W_REALLY_GET_ITEM";
  int  ok;
  int  i, j, k, m, n;
  unsigned int ui, uj, um;
  int  my_ret_size;
  int  my_bit_start=0;
  int  my_ret_type;
  int  sbit, obit, ebit, len;
  void *addr;
  char newf[256];	/* new file name after decoding */
  int  my_count;	/* for extracting array TM items in groupings     */
  int  switch_bits;     /* for sunos, some TM items need to be converted */
  int  is_little_endian;
  struct same_name_item *psni;
  struct same_name_item *psni2;
  struct item_attributes *pia;
  struct extract *pe;
  struct extract extract;
  int extract_strategy;
  struct pc_or_int pcoi;
  char *ps;
  char *pc;
  unsigned char *pu;
  float *pf;
  double *pd, *pd2;
  int item_type;
  int callers_type;
  int callers_byte_size;
  char **ppc;
  int *pi;
  int start_byte;
  int byte_size;
  int ilen;
  int ibit;
  int remaining;
  struct mdt xmdt;
  char i_name[48];
  int found_embedded_param;

  if (size <= 0) goto bad_buf_size_err;

  /* truncate the item name if it contains embedded parameters */
  found_embedded_param = NO;
  for (i=0, pc=i_name, ps=item_name; 
       *ps != '\0' && *ps != ITEM_NAME_PARAM_INTRO && i < 47;
       *pc = *ps, ps++, pc++, i++);
  *pc = *ps;
  if (*ps == ITEM_NAME_PARAM_INTRO) *(++pc) = '\0';
  
  /* find a valid item in the item database */
  ok = w_locate_item( ch, event_name, i_name, &psni );
  if (ok != OK) return ok;

  *ret_psni = psni;
  callers_type = *ret_type;
  item_type = psni->flags & TYPE_RETURN_MASK;

/*
  wid_show_item( psni, item_name );
*/

  /* process the non-EXTRACT items */                             
  if ((psni->flags & EXTRACT) == 0)
  {
     switch (callers_type)
     {
     case INT_RETURN:
        callers_byte_size = size * 4;
        break;
     case FLOAT_RETURN:
        callers_byte_size = size * 4;
        break;
     case DOUBLE_RETURN:
        callers_byte_size = size * 8;
        break;
     case CHAR_RETURN:
        callers_byte_size = size;
        break;
     case PCHAR_RETURN | DOUBLE_RETURN:	/* special for w_generate_aux_args */
        callers_byte_size = size * 8;
        break;
     default:
        goto sizeing_data_type_err;
        break;
     }

     pia = psni->head;
     while (pia != NULL)
     {
        switch(pia->kind_of)
        {
        case FIXED_VALUE:
           *ret_type = item_type;
/*
printf("...*ret_type=%Xx.\n", *ret_type);
printf("...value_count=%d.\n", pia->u.pfv->value_count);
*/
           if (pia->u.pfv->expr != NULL)
           {
/*
printf("%s: ...calling to evaluate expression for %s...\n", rn, item_name);
printf("%s: ...expr=%s\n", rn, pia->u.pfv->expr);
*/
              /* only type int or double is returned here */
              ok = w_item_expr_eval(ch, event_name, item_name, 
                   pia->u.pfv->expr,
                   &xmdt, &i,
                   w_item_expr_var_resolve, wc_msg);
              *ret_size = 1;
              switch (*ret_type)
              {
              case INT_RETURN:
                 if (i == INT_RETURN) {a[0] = xmdt.u.i4;}
                 if (i == DOUBLE_RETURN) {a[0] = xmdt.u.r8;}
                 break;
              case FLOAT_RETURN:
                 pf = (float *) a;
                 if (i == INT_RETURN) {*pf = xmdt.u.i4;}
                 if (i == DOUBLE_RETURN) {*pf = xmdt.u.r8;}
                 break;
              case DOUBLE_RETURN:
                 pd = (double *) a;
                 if (i == INT_RETURN) {*pd = xmdt.u.i4;}
                 if (i == DOUBLE_RETURN) {*pd = xmdt.u.r8;}
                 break;
              default:
                 goto fixed_value_data_type_err;
                 break;
              }
              return OK;
              break;
           }
           switch (*ret_type)
           {
           case INT_RETURN:
              j = callers_byte_size / 4;
              for (i=0; i < pia->u.pfv->value_count && i < j; ++i)
                 a[i] = pia->u.pfv->u.ivals[i];
              *ret_size = i;
              break;
           case FLOAT_RETURN:
              j = callers_byte_size / 4;
              pf = (float *) a;
              for (i=0; i < pia->u.pfv->value_count && i < j; ++i)
                 pf[i] = pia->u.pfv->u.fvals[i];
              *ret_size = i;
              break;
           case DOUBLE_RETURN:
              /* caller's buffer alignment must be 64-bit or get
                 a "bus error" on the sun
              */
              if ((callers_type & TYPE_RETURN_MASK) != DOUBLE_RETURN)
                 goto fixed_value_real8_err;
              j = callers_byte_size / 8;
              pd = (double *) a;
              for (i=0; i < pia->u.pfv->value_count && i < j; ++i)
                 pd[i] = pia->u.pfv->u.dvals[i];
              *ret_size = i;
              break;
           case CHAR_RETURN:
              j = callers_byte_size;
              ps = (char *) a;
              for (i=0; i < pia->u.pfv->value_count && i < j; ++i)
                 ps[i] = pia->u.pfv->u.cvals[i];
              *ret_size = i;
              break;
           default:
              goto fixed_value_data_type_err;
              break;
           }
           return OK;
           break;

        case FUNCTION_NUMBER:
           if (pia->u.pfun->head != NULL)
              /* generate the auxilliary argument list */
           {
              ok = w_generate_aux_args(ch, event_name,
                   pia->u.pfun->head );
              if (ok != OK) goto function_aux_arg_err;
           }
           ok = call_function_item(
                pia->u.pfun->function_number, 
                ch, event_name, 
                a,callers_byte_size,ret_size,ret_type,
                pia->u.pfun->head);
           if (ok != OK) goto bad_function_invocation;
           *ret_type = psni->flags & TYPE_RETURN_MASK;
	   return OK;
           break;

        /* this procedure_number code is not really used for other than
           reporting an error for physical_units items, but, there may be more
           proceudres (functions with no argument lists that are allowed to
           get items) in the future...
        */
        case PROCEDURE_NUMBER:
           switch(pia->u.pproc->procedure_number)
           {
           case PHYSICAL_UNITS_PROC:
                i = ch + 1;
                ok = phys_units_mstr_cyl(&i, item_name, a, &size, ret_size);
                *ret_type = FLOAT_RETURN;
                if (ok != 1) goto physical_units_err;
                return OK;
                break;
           default:
                goto procedure_number_err;
                break;
           }
	   return 0;
	   break;

        /* handle NSSDC Common Data Format data elements
        */
        case CDF_ITEM:
           ok = w_mk_cdf_arg_ls_and_get(ch, a, size, ret_size, ret_type, 
                pia->u.pcdf, item_name, psni->flags);
           if (ok != OK) goto cdf_item_err;
	   return 1;
	   break;

        case LOOKUP_TABLE:
           i = 0;
           my_ret_size = 0;
           my_bit_start = 0;
           my_ret_type = INT_RETURN;
#ifdef USE_MACOSX
           ok = w_really_get_item_(
#else
           ok = w_really_get_item(
#endif
		ch,
		event_name,
	        pia->u.plook->name,
		&i,
		1,
		&my_ret_size,
		&my_bit_start,
                &my_ret_type,
		&psni2);
           if (ok != OK) goto bad_lookup_table_name;
           if (my_ret_type != INT_RETURN) goto bad_lookup_type;
           if (!(i >= 0 && i < MAX_LOOKUP_TBL_ENTRIES))
              goto bad_lookup_table_index;
           *ret_type = psni->flags & TYPE_RETURN_MASK;
	   *ret_size = 1;
           switch (*ret_type)
           {
           case INT_RETURN:
              a[0] = pia->u.plook->u.itbl[i];
              break;
           case FLOAT_RETURN:
              pf = (float *) a;
	      *pf = pia->u.plook->u.ftbl[i];
              break;
           case DOUBLE_RETURN:
              pd = (double *) a;
	      *pd = pia->u.plook->u.dtbl[i];
              break;
           case CHAR_RETURN:
              ps = (char *) a;
              strncpy(ps, pia->u.plook->u.ctbl[i], callers_byte_size);
              *ret_size = strlen(ps);
              break;
           default:
              goto fixed_value_data_type_err;
              break;
           }
           return OK;
           break;

        case FILE_NAME:
           ok = wind_get_table_name( pia->u.myfile, newf, ch, event_name);
           if (ok != OK) goto bad_table_name;
           j = psni->flags & TYPE_RETURN_MASK;
           if (j == DOUBLE_RETURN && callers_type != DOUBLE_RETURN)
              goto bad_table_data_type;
           if (!w_read_tbl_file(newf,a,size,ret_size,j))
                 goto bad_table_file;
           *ret_type = psni->flags & TYPE_RETURN_MASK;
	   return OK;
           break;

        case TEXT_ITEM:
/*
printf("...processing TEXT item...\n");
printf("......value is %s.\n", pia->u.text);
*/
           /* return a copy of the text, or a pointer to the text */
           if ((callers_type & PCHAR_RETURN) != 0)
           {
              ppc = (char **) a;
              *ppc = pia->u.text;
/*
#if SYSTEM == VMS
              ppc = (char *) &(a[0]);
              *ppc = pia->u.text;
#endif
#if SYSTEM == SUNOS
              ppc = (char **) a;
              *ppc = pia->u.text;
#endif
#ifdef USE_MACOSX
              ppc = (char **) a;
              *ppc = pia->u.text;
#endif
              pcoi.u.ival[0] = 0;
              pcoi.u.ival[1] = 0;
              pcoi.u.pc = pia->u.text;
	      a[0] = pcoi.u.ival[0];
              a[1] = pcoi.u.ival[1];
*/
              *ret_size = 1;
              *ret_type = PCHAR_RETURN;
           }
           else
           {
              ps = (char *) a;
              strncpy(ps, pia->u.text, size);
              *ret_size = strlen(ps);
              *ret_type = psni->flags & TYPE_RETURN_MASK;
           }
/*
printf("......END processing TEXT item...\n");
*/
           return OK;
           break;

        default: break;

        } /* end of switch */
	pia = pia->next;
     } /* end of while */
  } /* end if, non-extract items */

#define JONK
#ifdef JONK
#include "extract.c"
#else
#include "extract_old.c"
#endif

  /* use recursion to build composite item if required */
  if ((psni->flags & COMPOSITE) != 0) 
  {
     ok = get_ptr_to_item_attribute(&pia, psni, COMPOSITE);
     if (ok != OK) goto missing_composite;
     *bit_start = *bit_start + len;
     *ret_size  = 0;			/* composite item is max 32 bits */
     my_ret_type = INT_RETURN;
/*
printf("%s: getting composite named %s.\n", rn, pia->u.chain); 
*/
#ifdef USE_MACOSX
    ok = w_really_get_item_(
#else
    ok = w_really_get_item(
#endif
		ch,
		event_name,
	        pia->u.chain,
		a,
		size,
		ret_size,
		bit_start,
                &my_ret_type,
		&psni2);
    if (ok != OK) goto bad_composite;
    if (my_ret_type != INT_RETURN) goto bad_type_composite;
  }

  return OK;

  /*********************** error messages **************************/

  bad_buf_size_err:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s buffer size must be greater than zero\n", rn,
         event_name, item_name);
  return ERR;

  bad_table_name:
  if (!wind_supprs_int_msg())
     printf("%s: cannot parse DBMS file name: %s\n", rn, pia->u.myfile);
  return w_bad_table_name;

  bad_table_data_type:
  if (!wind_supprs_int_msg())
     printf("%s: incompatable caller/item (file) data types: %s\n",
         rn, item_name);
  return w_bad_table_name;

  bad_table_file:
  if (!wind_supprs_int_msg())
     printf("%s: cannot open or read table file: %s\n", rn, newf);
  return w_bad_table_file;

  invalid_area:
  if (!wind_supprs_int_msg())
     printf("%s: invalid EXTRACT/AREA in %s %s, area=%d,'%c'\n", rn, event_name,
	item_name, pe->area, pe->area);
  return w_invalid_item_area;

  bad_conversion:
  if (!wind_supprs_int_msg())
     printf("%s: '%s %s' invalid offset and/or length for bit extraction\n",
         rn, event_name, item_name);
  return ok;
/*
  bad_count:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' count item not gettable\n", rn,
          event_name, pia->u.count);
  return w_ungettable_count_item;
*/

  fixed_value_data_type_err:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' unknown type for fixed value item\n", rn, item_name);
     printf("%s: data type is %Xx.\n", rn, *ret_type);
  return w_invalid_data_type;

  sizeing_data_type_err:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' unknown type for sizeing item\n", rn, item_name);
  return w_invalid_data_type;

  fixed_value_real8_err:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' real*8 buffer required for item\n", rn, item_name);
  return w_invalid_data_type;

  bad_function_invocation:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' function item not gettable\n", rn, item_name);
  return w_ungettable_function_item;

  function_aux_arg_err:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' function item argument list not gettable\n",
     rn, item_name);
  return w_ungettable_function_item;

  physical_units_err:
  if (!wind_supprs_int_msg())
  printf("%s: error getting physical units for '%s'.\n",
     rn, item_name);
  return ERR;

  procedure_number_err:
  if (!wind_supprs_int_msg())
     printf("%s: '%s' procedure to call is not specified\n",
     rn, item_name);
  return ERR;

  cdf_item_err:
  if (!wind_supprs_int_msg())
     printf("%s: cannot get CDF item '%s'.\n",
     rn, item_name);
  return ERR;

  missing_extract_struct:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT flagged but not found\n", rn, 
         event_name, item_name);
  return ERR;

  extract_eval_err:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT embedded evaluation failed\n", rn, 
         event_name, item_name);
  return ERR;

  extract_optimize_err:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT optimization failed\n", rn, 
         event_name, item_name);
  return ERR;

  extract_data_type_err1:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT data type incompatability (sizes)\n", rn, 
         event_name, item_name);
  return ERR;

  extract_data_type_err2:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT data type incompatability (code)\n", rn, 
         event_name, item_name);
  return ERR;

  extract_data_type_err3:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT data type incompatability (composite)\n", rn, 
         event_name, item_name);
  return ERR;

  extract_data_type_err4:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s EXTRACT, 8 bits or less per paste\n", rn, 
         event_name, item_name);
  return ERR;

  bad_composite:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s composite item not gettable\n", rn, event_name, 
          pia->u.chain);
  return w_ungettable_cmpsit_item;

  missing_composite:
  if (!wind_supprs_int_msg())
     printf("%s: %s %s COMPOSITE flagged but not found\n", rn, 
         event_name, item_name);
  return ERR;

  bad_type_composite:
  printf("%s: %s %s COMPOSITE not integer.\n", rn, 
         event_name, item_name);
  return w_invalid_data_type;

  bad_lookup_type:
  printf("%s: %s %s, LOOKUP=%s not integer.\n", rn, 
         event_name, item_name, pia->u.plook->name);
  return w_invalid_data_type;

  bad_lookup_table_name:
  if (!wind_supprs_int_msg())
  printf("%s: %s %s, LOOKUP=%s table (item name) not found.\n", rn, 
         event_name, item_name, pia->u.plook->name);
  return ERR;

  bad_lookup_table_index:
  if (!wind_supprs_int_msg())
  printf("%s: %s %s, LOOKUP=%s index %d not in [0..%d].\n", rn, 
         event_name, item_name, pia->u.plook->name, i, 
         MAX_LOOKUP_TBL_ENTRIES-1);
  return ERR;

} /* end of w_really_get_item */

/*
   Generate the argument list passed to function-item functions.
   Note that constant values are converted from the original string
   specification each time because it is anticipated that user written/defined
   functions will be available some day and user programs can not be
   trusted to maintain the integrety of the constant values.
*/
static int w_generate_aux_args( ch, event, head )
   int ch;
   char *event;
   struct function_arg_list *head;
{
   struct function_arg_list *p;
   int size;
   int ret_size;
   int ok;
   int bit_start;
   int ret_type;
   int data_type;
   int i, n;
   char *dot;
   struct same_name_item *psni2;

   if (head == NULL) return ERR;

   i = 1;
   p = head;
   while (p != NULL)
   {
      p->u.ival[0] = 0;
      p->u.ival[1] = 0;
      size = 1; /* size of 1 double, 8 bytes */
      bit_start = 0;
      ret_size = 0;
      data_type = p->data_type;
      if (p->pc == NULL) goto item_str_err;
      if ( (*p->pc >= '0' && *p->pc <= '9') || 
           (*p->pc == '.') ||
           (*p->pc == '-') ||
           (*p->pc == '+') )			/* numeric constant */
      {
         dot = strstr(p->pc, ".");
         if (dot == NULL) 			/* integer constant */
         {
            p->data_type = INT_RETURN;
            n = sscanf(p->pc,"%d", p->u.ival);
            if (n != 1) goto scan_err;
         }
         else
         {
            /* for now, assume exponential notation means double precesion */
            if ( NULL != (dot = strstr(p->pc, "E")))	/* double constant */
            {
               p->data_type = DOUBLE_RETURN;
               n = sscanf(p->pc,"%lf", &p->u.dval);
               if (n != 1) goto scan_err;
            }
            else					/* float constant */
            {
               p->data_type = FLOAT_RETURN;
               n = sscanf(p->pc,"%f", p->u.fval);
               if (n != 1) goto scan_err;
            }
         }
      }
      else					/* item name */
      {
         data_type = DOUBLE_RETURN | PCHAR_RETURN;
#ifdef USE_MACOSX
         ok = w_really_get_item_(ch, event, p->pc, &p->u.dval, size, 
#else
         ok = w_really_get_item(ch, event, p->pc, &p->u.dval, size, 
#endif
           &ret_size, &bit_start, &data_type, &psni2);
         if (ok != OK) goto item_get_err;
         p->data_type = data_type;
      }
      p = p->next;
      i++;
   }

   return OK;

   item_get_err:
   printf( "W_GENERATE_AUX_ARGS: cannot get argument item %s.\n", p->pc);
   return ERR;

   item_str_err:
   printf( "W_GENERATE_AUX_ARGS: null string pointer for item #%d.\n", i);
   return ERR;

   scan_err:
   printf( "W_GENERATE_AUX_ARGS: cannot scan const funct arg: %s.\n", p->pc);
   return ERR;
}

/*--------------------------------------------------- wind_get_table_name ----*/
/*xxxxxxxxxxxxxxxx
Make this routine get the item after getting the format, then call
it with an int buffer for ints, and a char buffer for characters.
Then users can say FILE=item_name[,%s].  Add a "case('%')" to catch
the format, then get the item.  May have to append a default ",%s"
or something, to get desired default behavior.
*/
static int wind_get_table_name(dbase_file_spec, fname, ch, event_name)
  char *dbase_file_spec;	/* coded file spec from data base	*/
  char fname[]         ;	/* new and complete file specification	*/
  int  ch              ;	/* user's channel number		*/
  char *event_name     ;	/* character string event type		*/
{
/* Parses a string of the form:
	'wind_dbms:', item_name1, fmt1, '_', item_name2, fmt2, '.dat'
   such as:
       'wind_dbms:',	 item_name1, %03.3x, '_', item_name2,%d, '.dat'
   and builds a complete file specification by calling wind_tm_really_get_item
   to return values for items named in the file specification string.
*/
  /*const*/ char *rn="WIND_GET_TABLE_NAME";
  /*const*/ char *tokens = ",";
  char fmt[16], *pfmtend;
  char item_name[64];
  char *p1, *p2, *c, *sp;
  int got_item_name = NO;
  int got_format = NO;
  int got_literal = NO;
  int done = NO;
  int j=0;
  int ok;
  int  ret_size;
  int  bit_start;
  int  ret_type;
  int  i,n;
  double r8;
  char cbuf[256];
  struct same_name_item sni2;
  struct same_name_item *psni2;
  struct mybuf {
     union {
        int i4;
        int ai4[2];
        unsigned int ui4;
        float r4;
        double r8;
        char ac[256];
     } u;
  };
  struct mybuf buffer;
  int buffer_size;
  int desired_type;

  p1 = dbase_file_spec;
  c  = cbuf;
  n  = 0;
  psni2 = &(sni2);
  while(done == NO)
  {
     switch(*p1)
     {
     case('%'):				/* a format */
        for (i = 0, p2 = fmt;
             ((*p1 > ' ' && *p1 < 'a') || (*p1 > 'z')) && (i < 15);
             *p2 = *p1, ++p1, ++p2, ++i);
        if (*p1 < 'a' || *p1 > 'z') goto parse_error;
        *p2 = *p1;
        pfmtend = p2;
        *(++p2) = '\0';
        ++p1;
        got_format = YES;
        got_literal = NO;
        break;
     case(','):				/* comma or white space */
     case(' '):
     case('\t'):
        ++p1;
        break;
     case('\0'):			/* end of string */
         done = YES;
         break;
     case('\''):			/* quoted literal, copy string as is */
     case('\"'):			/* omitting the quote marks */
        p2 = p1;
        ++p1;
        for (n = 0; 
             *p1 > ' ' && *p1 != *p2 && n < 255;
             *c = *p1, ++n, ++c, ++p1);
        if (n == 0) goto parse_error;
        if (*p1 == *p2) ++p1;
        *c = '\0';
        break;
     default:				/* an item name (unquoted string) */
        /* copy the item name to a local buffer */
        for (i = 0, p2 = item_name;
             *p1 > ' ' && *p1 != ',' && i < 63;
             *p2 = *p1, ++p1, ++p2, ++i);
        if (i == 0 || i >= 63) goto parse_error;
        *p2 = '\0';
        got_item_name = YES;
        got_literal = NO;
        break;
     }
     /* get the item when appropriate */
     if (*p1 == '\0') done = YES;
     if ( (got_item_name == YES) &&
        ( (got_literal == YES) || (got_format == YES) || (done == YES) ) )
     {
        /* supply a default format as needed */
        if (got_format != YES)
        {
           strcpy(fmt, "%s");
           pfmtend = &(fmt[1]);
        }
        /* reset flags for parsing remainder of coded input string */
        got_item_name = NO;
        got_format = NO;
        got_literal = NO;
        /* adjust w_really_get_item arguments according to data type */
        switch (*pfmtend)
        {
        case ('s'):
           ret_type = CHAR_RETURN;
           buffer_size = sizeof(buffer.u.ac) - 1;
           break;
        case ('d'):
        case ('o'):
        case ('u'):
        case ('x'):
        case ('X'):
           ret_type  = INT_RETURN;
           buffer_size = 2;
           break;
        case ('f'):
        case ('e'):
        case ('E'):
        case ('g'):
        case ('G'):
           ret_type  = DOUBLE_RETURN;
           buffer_size = 1;
           break;
        default:
           goto data_type_err;
           break;
        }
        /* get value of item */
        bit_start = 0;
        ret_size  = 0;
	desired_type = ret_type;
/*
printf("...calling really_get_item with %s.\n", item_name);
printf("......ch=%d, event=%s, buff=%XX, buffer_size=%d\n", ch, event_name,
buffer.u.ai4, buffer_size);
printf("......ret_size=%d, bitstart=%d, ret_type=%XX, psni2=%XX\n",
   ret_size, bit_start, ret_type, psni2);
*/
#ifdef USE_MACOSX
        ok = w_really_get_item_(
#else
        ok = w_really_get_item(
#endif
                ch,
                event_name,
                item_name,
                buffer.u.ai4,
                buffer_size,
                &ret_size,
                &bit_start,
                &ret_type,
                &psni2);
        if (ok != OK) goto bad_item;
        if (ret_size < 1 || ret_size > buffer_size) goto bad_ret_size;
        /* write value to file name */
        /* on VAX VMS sprintf returns int, which is K&R standard,
           on SunOS sprintf returns (char *)
           sp or j = sprintf(c,p2, j);
        */
        switch (ret_type & desired_type)
        {
        case (INT_RETURN):
           sprintf(c, fmt, buffer.u.i4);
           break;
        case (FLOAT_RETURN):
           sprintf(c, fmt, buffer.u.r4);
           break;
        case (DOUBLE_RETURN):
           sprintf(c, fmt, buffer.u.r8);
           break;
        case (CHAR_RETURN):
           /* should keep track of how many characters written to buf */
           i = ret_size;
           strncpy(c,buffer.u.ac,ret_size);
           c[i] = '\0';
           break;
        default:
           goto data_type_err2;
           break;
        }
        while(*c > ' ') ++c;
     }
  }
  /* copy result to caller's buffer */
  strncpy(fname, cbuf, 255);
  return OK;

  bad_item:
  if (wind_supprs_int_msg()) return ERR;
  printf("%s: %s item '%s' is ungettable.\n", rn, event_name, item_name);
  return ERR;

  bad_ret_size:
  if (wind_supprs_int_msg()) return ERR;
  printf("%s: %s item '%s' has a bad return_size: %d.\n", 
     rn, event_name, item_name, ret_size);
  return ERR;

  data_type_err:
  if (wind_supprs_int_msg()) return ERR;
  printf("%s: this format not allowed in FILE=%s.\n", rn, dbase_file_spec);
  return ERR;

  data_type_err2:
  if (wind_supprs_int_msg()) return ERR;
  printf("%s: item '%s' returns an odd or mismatched type.\n", rn, item_name);
  return ERR;

  parse_error:
  if (wind_supprs_int_msg()) return ERR;
  printf("%s: event %s, cannot parse: %s.\n", rn, event_name, dbase_file_spec);
  return ERR;

}

/*-------------------------------------------------------- strcpy_nowhite ----*/
static void strcpy_nowhite( s, t )
  char *s ;
  char *t ;
/* copy string t to string s removing white space */
{
  while( *t != '\0')
    if (*t > ' ') *s++ = *t++;
    else t++;
  *s = '\0';
}


/*------------------------------------------------------- w_read_tbl_file ----*/
static int w_read_tbl_file( fname, a, size, ret_size, data_type )
char fname[128] ;   /* name of file to open to read integers from  */
int  a[]        ;   /* caller's buffer to contain read-in integers */
int  size       ;   /* size of caller's buffer in int's            */
int  *ret_size  ;   /* number of ints written into caller's buffer */
int  data_type  ;   /* data type of caller's buffer                */
{
/*  This routine opens and reads an integer array of numbers from
    the file whose name is passed via the argument list
*/
  FILE *fp;
  char line[256];
  char *c;
  char **e = &c;  /* strtol hangs unless e is true, also advances pointer c */
  /*const*/ char *intchars="!+-0123456789";
  int  *ap;
  char buf[256];
  char *p;
  char *getenv();
  char *p_c;
  float *p_f;
  double *p_d;
  double d;

  p_f = (float *) a;
  p_d = (double *) a;

#if SYSTEM == VMS
  if ( (fp = fopen(fname, "r")) == NULL) goto bad_file_name;
#endif

#if SYSTEM == SUNOS
/* item database names are in vax/vms format, i.e., logical_name:file_name.ext.
   In SunOS we take the logical_name to be an environment variable which
   we use getenv to decode, replace the ":" with a "/", and keep the
   file_name.ext as the file name to generate a complete path/filename
   specification
*/
   c = strchr(fname,':');
   if (c != NULL)
   {
      *c = '\0';
      strcpy(buf,fname);
      *c = ':';
      p = getenv(buf);
      if (p == NULL)
      {
         printf("Environment variable %s is undefined.\n", buf);
         return 0;
      }
      strcpy(buf,p);
      strcat(buf,"/");
      c++;
      strcat(buf,c);
      if ( (fp = fopen(buf, "r")) == NULL) goto bad_file_name;
   }
   else
     if ( (fp = fopen(fname, "r")) == NULL) goto bad_file_name;
#endif

#ifdef USE_MACOSX
/* item database names are in vax/vms format, i.e., logical_name:file_name.ext.
   In MacOS we take the logical_name to be an environment variable which
   we use getenv to decode, replace the ":" with a "/", and keep the
   file_name.ext as the file name to generate a complete path/filename
   specification
*/
   c = strchr(fname,':');
   if (c != NULL)
   {
      *c = '\0';
      strcpy(buf,fname);
      *c = ':';
      p = getenv(buf);
      if (p == NULL)
      {
         printf("Environment variable %s is undefined.\n", buf);
         return 0;
      }
      strcpy(buf,p);
      strcat(buf,"/");
      c++;
      strcat(buf,c);
      if ( (fp = fopen(buf, "r")) == NULL) goto bad_file_name;
   }
   else
   {
     if ( (fp = fopen(fname, "r")) == NULL) goto bad_file_name;
   }
#endif

  ap = a;

  while(fgets(line, 256, fp) && (*ret_size < size))
  {
     c = line;
     while( (c = strpbrk(c,intchars)) && (*ret_size < size) )
     {
        if (*c == '!') break;				/* got comment, quit */
        switch(data_type)
        {
        case INT_RETURN:
           *(ap++) = (int) strtol(c, e, 10);		/* c is updated also */
           break;
        case FLOAT_RETURN:
           d = strtod(c, e);
           *(p_f++) = (float) d;
           break;
        case DOUBLE_RETURN:
           *(p_d++) = strtod(c, e);
           break;
        default:
           break;
        }
        ++*ret_size;
     }
  }

  fclose(fp);
  return 1;

  bad_file_name:
  if (wind_supprs_int_msg()) return 0;
  printf("cannot open file: %s\n", fname);
#if SYSTEM == SUNOS
  printf("            Unix: %s\n", buf);
#endif
#ifdef USE_MACOSX
  printf("            Unix: %s\n", buf);
#endif
  return 0;
}

int w_do_validation( a, p )
   int *a;
   struct validation *p;
{
   char *rn="W_DO_VALIDATION";
   int i;

   /* compare the validation values */
   switch(p->operation)
   {
   case IS_BIT_SET_OP:
      if (*a > 31) goto no_pass;
      i = (i=1) << *a;
      if ( (i & p->val1) == 0 ) goto no_pass;
      break;
   case BETWEEN_OP_INC:
      if ( !(p->val1 <= *a && p->val2 >= *a) ) goto no_pass;
      break;
   case BETWEEN_OP_EXC:
      if ( !(p->val1 < *a && p->val2 > *a) ) goto no_pass;
      break;
   case DATE_GE_OP:
      if (a[0] < p->val1) goto no_pass; 
      if (a[0] == p->val1 && a[1] < p->val2) goto no_pass;
      break;
   case DATE_GT_OP:
      if (a[0] < p->val1) goto no_pass; 
      if (a[0] == p->val1 && a[1] <= p->val2) goto no_pass;
      break;
   case DATE_LE_OP:
      if (a[0] > p->val1) goto no_pass; 
      if (a[0] == p->val1 && a[1] > p->val2) goto no_pass;
      break;
   case DATE_LT_OP:
      if (a[0] > p->val1) goto no_pass; 
      if (a[0] == p->val1 && a[1] >= p->val2) goto no_pass;
      break;
   case EQ_OP:
      if (p->val1 != *a) goto no_pass;
      break;
   case GE_OP:
      if (*a < p->val1) goto no_pass;
      break;
   case GT_OP:
      if (*a <= p->val1) goto no_pass;
      break;
   case LE_OP:
      if (*a > p->val1) goto no_pass;
      break;
   case LT_OP:
      if (*a >= p->val1) goto no_pass;
      break;
   case NE_OP:
      if (p->val1 == *a) goto no_pass;
      break;
   case OR_OP:
      if ( (p->val1 | *a) == 0 ) goto no_pass;
      break;
   case AND_OP:
      if ( (p->val1 & *a) == 0 ) goto no_pass;
      break;
   case NOT_BETWEEN_OP:
      if ( !(p->val1 > *a && p->val2 < *a) ) goto no_pass;
      break;
   default:
      goto invalid_operation;
      break;
   }

   return OK;

no_pass:
   if (show_validation_failures == YES)
      printf("%s: %s=%d failed, op=%d v1=%d v2=%d\n", rn,
         p->name, *a, p->operation, p->val1, p->val2);
   return w_item_validation_fail;

invalid_operation:
  if (!wind_supprs_int_msg())
     printf("%s: invalid 'operation'=%d %s\n", rn, p->operation, p->name);
  return w_bad_validation_op;

}

int w_set_val_fail_msg( on_off )
   int *on_off;
{
   show_validation_failures = *on_off;
   return 1;
}

#ifdef USE_MACOSX
int wid_do_xlate_cpy_( psni, tm_val, s, len_s, flag)
#else
int wid_do_xlate_cpy( psni, tm_val, s, len_s, flag)
#endif
   struct same_name_item *psni;		/* ptr to same named item	*/
   int *tm_val          ;               /* a value from the TM stream   */
   char *s              ;               /* caller's buffer for string   */
                                        /* representation of arg tm_val */
   int len_s		;		/* size of caller's buffer	*/
   int flag		;		/* do char-int, or int-char	*/
{
   int ok;
   xlate_item *xp	;		/* pointer to xlate structure	*/
   struct item_attributes *pia;

   /* find the xlate structure in the item's attribute list */
   ok = get_ptr_to_item_attribute( &pia, psni, XLATE_PRESENT);
   if (ok != OK) return ok;

   xp = pia->u.xlate_ptr;
   while (xp != NULL)
   {
      switch (flag)
      {
      case 0:				/* int to char translation */
         if (xp->val == *tm_val)
         {
            strncpy(s, xp->str, len_s);
            return OK;
         }
         break;
      case 1:				/* char to int translation */
         if (strcmp(s,xp->str) == 0)
         {
            *tm_val = xp->val;
            return OK;
         }
         break;
      default:
         printf("WID_DO_XLATE_CPY: invalid xlate flag./n");
         return ERR;
         break;
      }
      xp = xp->next;
   }

   return ERR;
}

/*----------------------------------------------------------------------------*/
/*
  Gets the string representation for named event/item with value tm_val.
  XLATE is used in wind_lib to mean an integer to character translation.
*/
#ifdef USE_MACOSX
int wid_get_item_xlate_( ch, event_name, item_name, tm_val, s, len_s, flag )
#else
int wid_get_item_xlate( ch, event_name, item_name, tm_val, s, len_s, flag )
#endif
   int ch		;		/* caller's channel number	*/
   char *event_name	;		/* caller's event name		*/
   char *item_name	;		/* caller's item name		*/
   int *tm_val          ;               /* a value from the TM stream   */
   char *s              ;               /* caller's buffer for string   */
                                        /* representation of arg tm_val */
   int len_s		;		/* size of caller's buffer	*/
   int flag		;		/* do char-int, or int-char	*/
{
   int ok;
   struct same_name_item *psni;

   /* search the linked list of xlate item values to match caller's value */
   ok = w_locate_item( ch, event_name, item_name, &psni );
   if (ok != OK) return ok;

#ifdef USE_MACOSX
   ok = wid_do_xlate_cpy_( psni, tm_val, s, len_s, flag);
#else
   ok = wid_do_xlate_cpy( psni, tm_val, s, len_s, flag);
#endif
   if (ok != OK) return ok;

   return OK;
}

#ifdef USE_MACOSX
int w_get_item_substruct_( ch, event_name, item_name,  pv, flag)
#else
int w_get_item_substruct( ch, event_name, item_name,  pv, flag)
#endif
   int ch;
   char *event_name;
   char *item_name;
   void *pv;
   int flag;
{
   char *rn="W_GET_ITEM_SUBSTRUCT";
   struct same_name_item *psni;
   struct item_attributes *pia;
   int ok;

   ok = w_locate_item( ch, event_name, item_name, &psni );
   if (ok != OK) return ERR;
   if ((flag & psni->flags) == 0) return ERR;

   /* find the specified structure in the item's attribute list */
   ok = get_ptr_to_item_attribute( &pia, psni, flag);
   if (ok != OK) return ERR;

   switch(flag)
   {
   case EXTRACT:
      memcpy(pv, pia->u.pe, sizeof(struct extract)); 
      break;
   case FILE_NAME:
   case TEXT_ITEM:
      strcpy((char *) pv, pia->u.myfile);
      break;
   default:
      printf("%s: nonreturnable substructure, %s %s flag=%d.\n",
          rn, event_name, item_name, flag);
      return ERR;
      break;
   }

   return OK;
}

#ifdef USE_MACOSX
int w_get_item_info_ ( psni, atom, buf, size, ret_size)
#else
int w_get_item_info ( psni, atom, buf, size, ret_size)
#endif
   struct same_name_item *psni;
   int *atom;
   void *buf;
   int *size;
   int *ret_size;
{
   char *rn="W_GET_ITEM_INFO";
   struct item_attributes *pia;
   int ok;
   int mysize;
   struct fortran_xlate_item *pfxi;
   struct fortran_validation *pfv;
   struct fortran_fixed_value *pffv;
   struct fortran_function_arg_list *pffal;
   struct fortran_lookup_table *pflook;
   struct extract *pe;
   xlate_item *pxi;
   struct function_arg_list *pfal;
   struct validation *pv;
   struct procedure *pproc;
   struct fortran_cdf_item *pcdf;
   int i,j;
   int *pi;
   struct string_list *psl;

   if (psni == NULL) return ERR;
   mysize = *size;
   *ret_size = 0;

   if (*atom == FLAGS)
   {
      pi = (int *) buf;
      *pi = psni->flags;
      *ret_size = 1;
      return OK;
   }

   ok = get_ptr_to_item_attribute( &pia, psni, *atom);
   if (ok != OK) goto get_pia_err;

   switch (*atom)
   {
   case EXTRACT:
      pe = (struct extract *) buf;
      *pe = *(pia->u.pe);
      *ret_size = 1;
      break;
   case XLATE_PRESENT:
      pfxi = (struct fortran_xlate_item *) buf;
      pxi = pia->u.xlate_ptr;
      for ( ; pxi != NULL && *ret_size < *size; 
              pxi = pxi->next, ++*ret_size, ++pfxi)
      {
         strncpy(pfxi->str, pxi->str, FORTRAN_ITEM_STR_LEN);
         pfxi->val = pxi->val;
      }
      break;
   case FUNCTION_NUMBER:
      pffal = (struct fortran_function_arg_list *) buf;
      pfal = pia->u.pfun->head;
i=0;
      for ( ; pfal != NULL && *ret_size < *size;
              pfal = pfal->next,  ++*ret_size, ++pffal)
      {
         strncpy( pffal->pc, pfal->pc, FORTRAN_ITEM_STR_LEN);
         pffal->data_type = pfal->data_type;
         pffal->u.dval = pfal->u.dval;
         pffal->function_number = pia->u.pfun->function_number;
      }
      break;
   case VALIDATION:
      pfv = (struct fortran_validation *) buf;
      do
      {
         if (pia->kind_of == VALIDATION)
         {
            pv = pia->u.pv;
            strncpy( pfv->name, pv->name, FORTRAN_ITEM_STR_LEN);
            pfv->val1 = pv->val1;
            pfv->val2 = pv->val2;
            pfv->operation = pv->operation;
            ++*ret_size;
            ++pfv;
         }
         pia = pia->next;
      } while (pia != NULL && *ret_size < *size);
      break;
   case FIXED_VALUE:
      pffv = (struct fortran_fixed_value *) buf;
      pffv->data_type   = psni->flags & TYPE_RETURN_MASK;
      pffv->value_count = pia->u.pfv->value_count;
      pffv->pchar       = pia->u.pfv->expr;
      if (pia->u.pfv->u.ivals == NULL) /* no values, is an expression */
      {
         pffv->u.ivals[0] = 0;
         pffv->u.ivals[1] = 0;
         break;
      }
      switch (pffv->data_type)
      {
      case INT_RETURN:
         i = FORTRAN_MAX_BYTES/4;
         for (j=0; j < i && j < pia->u.pfv->value_count; j++) 
            pffv->u.ivals[j] = pia->u.pfv->u.ivals[j];
         break;
      case FLOAT_RETURN:
         i = FORTRAN_MAX_BYTES/4;
         for (j=0; j < i && j < pia->u.pfv->value_count; j++) 
            pffv->u.fvals[j] = pia->u.pfv->u.fvals[j];
         break;
      case DOUBLE_RETURN:
         i = FORTRAN_MAX_BYTES/8;
         for (j=0; j < i && j < pia->u.pfv->value_count; j++) 
            pffv->u.dvals[j] = pia->u.pfv->u.dvals[j];
         break;
      case CHAR_RETURN:
         i = FORTRAN_MAX_BYTES;
	 if (i > pia->u.pfv->value_count) i = pia->u.pfv->value_count;
         for (j=0; j < i; j++) 
            pffv->u.cvals[j] = pia->u.pfv->u.cvals[j];
         break;
      default:
         goto unknown_data_type_err;
         break;
      }
      break;
   case LOOKUP_TABLE:
      pflook = (struct fortran_lookup_table *) buf;
      pflook->data_type = psni->flags & TYPE_RETURN_MASK;
      for (j=0; j<MAX_LOOKUP_TBL_ENTRIES; j++)
         pflook->u.itbl[j] = pia->u.plook->u.itbl[j];
      break;
   case PROCEDURE_NUMBER:
      pproc = (struct procedure *) buf;
      pproc->procedure_number = pia->u.pproc->procedure_number;
      break;
   case CDF_ITEM:
      pcdf = (struct fortran_cdf_item *) buf;
      strncpy(pcdf->independent_rv, pia->u.pcdf->independent_rv, 32);
      strncpy(pcdf->independent_rv_match, pia->u.pcdf->independent_rv_match,32);
      strncpy(pcdf->dependent_rv, pia->u.pcdf->dependent_rv, 32);
      pcdf->search_mode = pia->u.pcdf->search_mode;
      pcdf->search_mode_2nd = pia->u.pcdf->search_mode_2nd;
      psl = pia->u.pcdf->file_list;
      i = j = 0;
      while(psl->str != NULL && j < MAX_CDF_FILES)
      {
         strncpy(&(pcdf->files[j][0]), psl->str, FORTRAN_MAX_BYTES);
         j++;
         psl = psl->next;
         i = i + FORTRAN_MAX_BYTES;
      }
      pcdf->n_files = j;
      break;
   case DESCRIPTION:
   case FILE_NAME:
   case COUNT_ITEM:
   case TEXT_ITEM:
   case COMPOSITE:
   case AUTHOR_DATE:
   case FORMAT:
      strncpy(buf, pia->u.myfile, *size);
      *ret_size = strlen(buf);
      break;
   default:
      goto unknown_atom_err;
      break;
   }

   return OK;

   unknown_atom_err:
   printf("%s: unknown item attribute code: %d.\n", rn, *atom);
   return ERR;

   get_pia_err:
/* Particular attribute doesn't exist for this item.
   printf("%s: cannot get attribute list.\n", rn);
*/
   return ERR;

   unknown_data_type_err:
   printf("%s: unknown fixed_value data type: %d.\n", rn, pffv->data_type);
   return ERR;
}

static int w_cp_c_cdf_to_fort_cdf(pcdf, fcdf)
   struct cdf_item *pcdf;
   struct fortran_cdf_item *fcdf;
{
   int fnmax=10;
   int fsz=256;
   int ok;
   int i,j,k,n;
   struct string_list *psl;

   memset( (char *) fcdf, 0, sizeof(struct fortran_cdf_item) );

   /* copy the linked list of file names into a contiguous buffer
      suitable for FORTRAN comsumption
   */
   psl = pcdf->file_list;
   if (psl == NULL) return ERR;
   for (i=0,n=0; n<=fnmax && psl != NULL; n++, i = i + fsz, psl = psl->next)
   {
      strncpy(fcdf->files[n], psl->str, fsz);
   }
   fcdf->n_files = n;

/*
printf("...inside w_cp_c_cdf_to_fort_cdf...\n");
printf("...dependent rv=%s.\n", pcdf->dependent_rv);
printf("...independent rv=%s.\n", pcdf->independent_rv);
printf("...independent rv type=%s.\n", pcdf->independent_rv_type);
printf("...independent rv match=%s.\n", pcdf->independent_rv_match);
printf("...file 1...%s.\n", &f[0]);
printf("...file 2...%s.\n", &f[256]);
*/
   if (pcdf->independent_rv != NULL)
      strncpy(fcdf->independent_rv, pcdf->independent_rv, 32);

   if (pcdf->independent_rv_match != NULL)
      strncpy(fcdf->independent_rv_match, pcdf->independent_rv_match, 32);

   strncpy(fcdf->dependent_rv, pcdf->dependent_rv, 32);
   fcdf->search_mode     = pcdf->search_mode;
   fcdf->search_mode_2nd = pcdf->search_mode_2nd;
   fcdf->use_dependent_rv_ranges = pcdf->use_dependent_rv_ranges;

   for (i=0; i<MAX_CDF_INDICES+1; i++)
   {
      fcdf->independent_rv_indices[i] = pcdf->independent_rv_indices[i];
      fcdf->dependent_rv_indices[i] = pcdf->dependent_rv_indices[i];
      fcdf->dependent_rv_range_order[i] = pcdf->dependent_rv_range_order[i];
      for (j=0; j < 4; j++)
         fcdf->dependent_rv_ranges[i][j] = pcdf->dependent_rv_ranges[i][j];

   }

   return OK;
}

static int w_mk_cdf_arg_ls_and_get ( ch, buf, size, ret_size, ret_type, pcdf,
                                     item, flags )
   int ch;
   void *buf;
   int size;
   int *ret_size;
   int *ret_type;
   struct cdf_item *pcdf;
   char *item;
   int  flags;
{
   int i,j;
   int ok;
   struct fortran_cdf_item fcdf;

   ok = w_cp_c_cdf_to_fort_cdf(pcdf, &fcdf);
   if (ok != OK) return ERR;

   /* w_cdf_get is a fortran routine */
   i = ch+1;
   j = strlen(item);
   return wind_get_cdf_thing(&i, buf, &size, ret_size, ret_type, &fcdf, 
                             item, &flags, &j);
}
