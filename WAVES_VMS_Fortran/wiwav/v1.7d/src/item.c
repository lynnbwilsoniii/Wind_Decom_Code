/* item.c - module to handle the basic item manipulation idioms like reading
from disk, internal storate, etc.

*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>

#include "wind_os_def.h"
#include "item.h"

#define OK 1
#define ERR 0
#define YES 1
#define NO 0
#define MAX_INPUT_LINE_SIZE 1024
#define MAX_SEARCH_LIST_DIRS 16
#define MY_MAX_COUNT 0x7fffffff
#define W_N_CDF_INDICES 11
#define W_USE_RANGE_LIMIT        -99

static int read_the_alls=NO;
static struct event_node *head_event_list=NULL;
/*
static char *wid_sl[MAX_SEARCH_LIST_DIRS];
*/
static char **wid_sl;
static int n_search_list_dirs=0;

extern struct item_node *tree_search();

extern void nth_tree_element();
extern void wid_inorder_traverse();
extern void rb_insert();

extern int w_item_expr_eval();

extern int wc_msg();

#define IS_WHITE(c) (c <= ' ')
#define IS_DIGIT(c) (c >= '0' && c <= '9')
#define IS_ALPHA(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
#define IS_EXPONENT_INTRO(c) (c == 'e' || c == 'E' || c == 'd' || c == 'D')

/* this test fails for expressions containing only items named E or D */
#define IS_EXPRESSION(px,py,end_char,it_is,has_var) \
for (py=px, it_is=NO, has_var=NO; \
    *py != '\0' && *py != end_char;\
    ++py)\
{\
  if (IS_DIGIT(*py)) continue; \
  if (IS_ALPHA(*py)) \
  {\
      if (IS_EXPONENT_INTRO(*py)) continue; \
      else {it_is = YES; has_var = YES; break;} \
  }\
  if (*py == '$' || *py == '_') {it_is = YES; has_var = YES; break;} \
  if (*py == '.') continue; \
  if (IS_WHITE(*py)) continue; \
  it_is = YES; \
}

/*
   optimize the extraction by checking for 8, 32, or 64 bit aligned
   entities
	startbit, bitlen, offset, rep, count

*/
int w_optimize_extract(pe)
   struct extract *pe;
{
   char *rn="w_optimize_extract";

   pe->flags = pe->flags & ~EXTRACT_TYPE_MASK;
   switch(pe->bitlen)
   {
   case 8:
      if ((pe->startbit % 8) == 0)
      {
         if (pe->offset <= 0)
         {
            pe->startbit = pe->startbit / 8;
            if (pe->rep <= 1)
               pe->flags = pe->flags | EXTRACT_8_BIT_SCALER;
            else
               pe->flags = pe->flags | EXTRACT_8_BIT_ARRAY;
         }
         else
         {
            if ((pe->offset % 8) == 0)
            {
               pe->startbit = pe->startbit / 8;
               pe->offset = pe->offset / 8;
               if (pe->rep <= 1)
                  pe->flags = pe->flags | EXTRACT_8_BIT_OFFSETS;
               else
                  pe->flags = pe->flags | EXTRACT_8_BIT_GROUPS;
            }
         }
      }
      break;
   case 32:
      if ((pe->startbit % 32) == 0)
      {
         if (pe->offset <= 0)
         {
            pe->startbit = pe->startbit / 32;
            if (pe->rep <= 1)
               pe->flags = pe->flags | EXTRACT_32_BIT_SCALER;
            else
               pe->flags = pe->flags | EXTRACT_32_BIT_ARRAY;
         }
         else
         {
            if ((pe->offset % 32) == 0)
            {
               pe->startbit = pe->startbit / 32;
               pe->offset = pe->offset / 32;
               if (pe->rep <= 1)
                  pe->flags = pe->flags | EXTRACT_32_BIT_OFFSETS;
               else
                  pe->flags = pe->flags | EXTRACT_32_BIT_GROUPS;
            }
         }
      }
      break;
   case 64:
      if ((pe->startbit % 64) == 0)
      {
         if (pe->offset <= 0)
         {
            pe->startbit = pe->startbit / 64;
            if (pe->rep <= 1)
               pe->flags = pe->flags | EXTRACT_64_BIT_SCALER;
            else
               pe->flags = pe->flags | EXTRACT_64_BIT_ARRAY;
         }
         else
         {
            if ((pe->offset % 64) == 0)
            {
               pe->startbit = pe->startbit / 64;
               pe->offset = pe->offset / 64;
               if (pe->rep <= 1)
                  pe->flags = pe->flags | EXTRACT_64_BIT_OFFSETS;
               else
                  pe->flags = pe->flags | EXTRACT_64_BIT_GROUPS;
            }
         }
      }
      break;
   default:
      pe->flags = pe->flags | EXTRACT_BITS;
      break;
   }

   if ((pe->flags & EXTRACT_TYPE_MASK) == 0) 
   {
      pe->flags = pe->flags | EXTRACT_BITS;
/*
      printf("%s: uncategorizeable EXTRACT type.\n");
      return ERR;
*/
      return OK;
   }
   return OK;
}


#ifdef USE_MACOSX
int w_cnvrt_c_fn_by_os_(cstr, bstr, sz_bstr)
#else
int w_cnvrt_c_fn_by_os(cstr, bstr, sz_bstr)
#endif
   char *cstr;
   char *bstr;
   int  *sz_bstr;
{
   char *getenv();
   char *s1;
   char f[256];
   char lgnm[256];	/* logical name or environment symbol */
   int i,j;
   char colon=':';
   FILE *myfi;
   char *pc, *ps, *pz, *px;

#if SYSTEM == VMS
   strncpy(bstr,cstr, *sz_bstr);
   return OK;
#endif

#if SYSTEM == SUNOS
   /* find colon in cstr, copy prefix to lgnm */
   for (i=0; cstr[i] != colon && cstr[i] > ' '; lgnm[i] = cstr[i], i++);
   if (cstr[i] != colon)
   {
      strncpy(bstr,cstr, *sz_bstr);
      return OK;
   }
   lgnm[i] = '\0';
   px = &cstr[i+1];

   s1 = getenv(lgnm);
   if (s1 == NULL)
   {
      printf("Environment variable '%s' is undefined.\n", lgnm);
      return ERR;
   }
   /* go through white-space-delimited search path, dir by dir, trying to
      find the proper file.  Successful when fopen returns non-NULL.
   */
   pc = s1;
   myfi = NULL;
   while (myfi == NULL && *pc != '\0')
   {
      for (pz=f; *pc > ' '; *pz = *pc, pc++, pz++); /* copy dir to buf */
      *pz++ = '/';
      *pz = '\0';
      strcat(f,px);
      if ( (myfi = fopen(f,"r")) == NULL)   /* skip inter-spec white space */
         for ( ; *pc != '\0' && *pc <= ' '; pc++);
   }
   if (myfi != NULL)
   {
      fclose(myfi);
      strncpy(bstr,f, *sz_bstr);
      return OK;
   }
   return ERR;
#endif

#ifdef USE_MACOSX
   /* find colon in cstr, copy prefix to lgnm */
   for (i=0; cstr[i] != colon && cstr[i] > ' '; lgnm[i] = cstr[i], i++);
   if (cstr[i] != colon)
   {
      strncpy(bstr,cstr, *sz_bstr);
      return OK;
   }
   lgnm[i] = '\0';
   px = &cstr[i+1];

   s1 = getenv(lgnm);
   if (s1 == NULL)
   {
      printf("Environment variable '%s' is undefined.\n", lgnm);
      return ERR;
   }
   /* go through white-space-delimited search path, dir by dir, trying to
      find the proper file.  Successful when fopen returns non-NULL.
   */
   pc = s1;
   myfi = NULL;
   while (myfi == NULL && *pc != '\0')
   {
      for (pz=f; *pc > ' '; *pz = *pc, pc++, pz++); /* copy dir to buf */
      *pz++ = '/';
      *pz = '\0';
      strcat(f,px);
      if ( (myfi = fopen(f,"r")) == NULL)   /* skip inter-spec white space */
         for ( ; *pc != '\0' && *pc <= ' '; pc++);
   }
   if (myfi != NULL)
   {
      fclose(myfi);
      strncpy(bstr,f, *sz_bstr);
      return OK;
   }
   return ERR;
#endif
}

int read_wid_int( s, val, val2 )
   char *s;
   int *val;
   int *val2;
{
   int i,j,n;

   *val = 0;

   if (s[0] == 'h' || s[0] == 'H')
   {
      n = sscanf(&s[1],"%x", val);
      return (n==1) ? OK : ERR;
   }

   /* converts a string representation of a binary number of the form
      b10011101 to int
   */
   if (s[0] == 'b' || s[0] == 'B')
   {
      n = strlen(s);
      if (n > 33 || n < 2) return ERR;
      for (j=0, n--; n > 0; j++, n--)
      {
         switch (s[n])
         {
         case '1': *val = *val | ( (i=1) << j );
         case '0': break;
         default: return ERR; break;
         }
      }
      return OK;
   }

   /* read a date-time string */
   if (*s == 'D')
   {
      n = sscanf(s,"DATE:%d-%d", val, val2);
      if (n != 2) return ERR;
      return OK;
   }

   n = sscanf(s,"%d", val);
   return (n==1) ? OK : ERR;
}


struct event_node *find_event_item_list(event_name)
   char *event_name;
{
   struct event_node *p;

   p = head_event_list;
   while(p != NULL)
   {
      if (0 == strcmp(p->event_name, event_name)) return p;
      p = p->next;
   }

   return NULL;
}



static struct event_node *get_new_event_item_list(event_name)
   char *event_name;
{
   char *rn="get_new_event_item_list";
   struct event_node *p, *t;
   int code;

   /* Make sure event name is not already a member of the list */
   if (NULL == (p = find_event_item_list(event_name)))
   {
      p = (struct event_node *) malloc(sizeof(struct event_node));
      if (p == NULL) goto allocate_error;
      p->event_name = (char *) malloc(strlen(event_name)+1);
      if (p->event_name == NULL) goto allocate_error;
      strcpy(p->event_name, event_name);
      p->n = 0;
      p->head = NULL;
      p->next = NULL;
      p->source_filename = NULL;
   }

   if (head_event_list==NULL) head_event_list = p;
   else
   {
      t = head_event_list;
      while (t->next != NULL) t = t->next;
      t->next = p;
   }

   return p;

   allocate_error:
   printf("%s: error allocating memory.\n", rn);
   return ERR;
}

static int new_item_attribute_node(pia, head)
   struct item_attributes **pia;
   struct item_attributes **head;
{
   char *rn="new_item_attribute_node";
   struct item_attributes *p;

   *pia = (struct item_attributes *) malloc(sizeof(struct item_attributes));
   if (*pia == NULL) goto allocate_error;
   (*pia)->next = NULL;
   (*pia)->prev = NULL;
   (*pia)->kind_of = 0;
   (*pia)->u.pe = NULL;
   if (*head==NULL) *head = *pia;
   else
   {
      p = *head;
      while (p->next != NULL) p = p->next;
      p->next = *pia;
      (*pia)->prev = p;
   }
   return OK;

   allocate_error:
   printf("%s: error allocating memory.\n", rn);
   return ERR;
}

/*
*/
int get_ptr_to_item_attribute(pia, psni, attribute)
   struct item_attributes **pia;
   struct same_name_item *psni;
   int attribute;
{
   struct item_attributes *p;

   if (psni == NULL) return ERR;
   if (psni->head == NULL) return ERR;
   if ((psni->flags & attribute) == 0) return ERR;

   p = psni->head;
   while (p != NULL)
   {
      if (p->kind_of == attribute)
      {
         *pia = p;
         return OK;
      }
      p = p->next;
   }

   return ERR;
}

/*
  An xlate (translate) instance is the structure containing an integer value
  and an associated string equivalent.  Xlate instances are maintained
  dynamically in a singly linked list.
*/
static int append_xlate_instance(val, str, xi)
   int val;
   char *str;
   xlate_item **xi;
{
   char *rn="append_xlate_instance";
   int i;
   xlate_item *p, *t;

   p = (xlate_item *) malloc(sizeof *p);
   if (p == NULL) goto allocate_error;
   p->val = val;
   p->next = NULL;
   i = strlen(str) + 1;
   p->str = malloc(i);
   if (p->str == NULL) goto allocate_error;
   strcpy(p->str, str);

   if (*xi == NULL)
      *xi = p;
   else
   {
      t = *xi;
      while (t->next != NULL) t = t->next;
      t->next = p;
   }

   return OK;

   allocate_error:
   printf("%s: error allocating memory.\n", rn);
   return ERR;
}

static int parse_extract_clause(ps, pe, flags)
   char *ps;
   struct extract *pe ;
   int flags;
{
   char *rn="parse_extract_clause";
   int i,j,k,n;
   int *pi;
   float *pf, r4;
   double *pd, r8;
   char *a, *b;
   char *pc;
   int is_exp, has_vars;
   int status;
   int ret_type;
   int declared_type;
   struct mdt x;
   int ok;
   char c4[4];
   char *apc[6];
   int sit;
   char buf[256];

   /* attempt to scan in the integer sequence, expressions will cause errors */
   n = sscanf(ps,"EXTRACT=%d,%d,%d,%d,%d,AREA:%c",
       &pe->startbit,
       &pe->bitlen,
       &pe->offset,
       &pe->rep,
       &pe->count,
       &c4[0]);
       pe->area = c4[0];
   if (n == 6) return OK;
   /* if ( !(n==6) ) goto bad_extract_clause; */

   /* assign pointers to beginning of each comma delimited sub-field */
   apc[0] = &ps[8];
   for (i=1, apc[i] = apc[i-1];
        i < 6 && *apc[i] >= ' ';
        ++apc[i])
      if (*apc[i] == ',') {++apc[i]; ++i; if (i < 6) apc[i] = apc[i-1];}
   if (i != 6) {sit=1; goto err_return;}

   /* read in the area field */
   a = apc[5];
   pe->area = (apc[5])[5];
   if (*a != 'A' || a[1] != 'R' || a[2] != 'E' || a[3] != 'A' || a[4] != ':')
      {sit=2; goto err_return;}

   for (j=0, b = apc[0]; j<5; ++j, b = apc[j])
   {
      switch(j)
      {
      case 0: pi = &(pe->startbit);   break;
      case 1: pi = &(pe->bitlen);     break;
      case 2: pi = &(pe->offset);     break;
      case 3: pi = &(pe->rep);        break;
      case 4: pi = &(pe->count);      break;
      default: sit = 3; goto err_return; break;
      }
      *pi = -99;
      IS_EXPRESSION(b,a,',',is_exp,has_vars)
      if (is_exp == YES)
      {
         if (has_vars == NO)
         {
            /* is a constant expression, evaluate now */
            for (k=0, a=b; *a != ',' && k < 255; buf[k] = *a, a++, k++);
            buf[k] = '\0';
/*
printf("...EX evaluating const expression: %s\n", buf);
*/
            ok = w_item_expr_eval(0,"NONE","NADA", buf,
                 &x, &ret_type, NULL, wc_msg);
            if (ok != OK) goto err_return;
            switch(ret_type)
            {
            case INT_RETURN:    *pi = x.u.i4; break;
            case DOUBLE_RETURN: *pi = x.u.r8; break;
            default: sit=4; goto err_return; break;
            }
/*
printf("...EX const expression evaluated to %d\n", x.u.i4);
*/
         }
         else
         {
/*
printf("...EX saving variable expression: %s\n", ps);
*/
            /* is a run-time expression, save and evaluate later */
            pe->flags = EXTRACT_HAS_EXPR;
            for (i=1, a=b; *a != '\0' && *a != ','; i++, a++);
            pe->expr[j] = (char *) malloc(sizeof(char) * i);
            if (pe->expr[j] == NULL) {sit=5; goto err_return;}
            strncpy(pe->expr[j], b, i);
            (pe->expr[j])[i-1] = '\0';
         }
      }
      else
      {
         /* this particular subfield is a single integer value */
         n = sscanf(b,"%d%n", pi, &k);
         if (n != 1) {sit=6; goto err_return;}
      }
   } /* end for, looping over the number of extract= subfield values */

/*
printf("@@@...got %d,%d,%d,%d,%d,area:%c\n", pe->startbit, pe->bitlen,
       pe->offset, pe->rep, pe->count, pe->area);
*/
   return OK;
err_return:
   printf("%s: error, situation %d, string=%.40s.\n", rn, sit, ps);
   if (sit == 5) printf("%s: error allocating memory.\n", rn);
   return ERR;
}

static int parse_fixed_value_clause(ps, pfv, flags)
   char *ps;
   struct fixed_value *pfv;
   int flags;
{
   char *rn="parse_fixed_value_clause";
   int i,j,k,n;
   int *pi;
   float *pf, r4;
   double *pd, r8;
   char *a;
   char *pc;
   int is_exp, has_vars;
   int status;
   int ret_type;
   int declared_type;
   struct mdt x;
   int ok;

   declared_type = TYPE_RETURN_MASK & flags;
   status = OK;
   IS_EXPRESSION(ps,a,',',is_exp,has_vars)
   if (is_exp == YES)
   {
      if (has_vars == NO)
      {
/*
printf("...evaluating const expression: %s\n", ps);
*/
         /* is a constant expression, evaluate now */
         ok = w_item_expr_eval(0,"NONE","NADA", ps,&x, &ret_type, NULL, wc_msg);
         if (ok != OK) goto err_return;
         switch(declared_type)
         {
         case INT_RETURN:
            if (ret_type == DOUBLE_RETURN) x.u.i4 = i = x.u.r8;
            pfv->u.ivals = (int *) malloc(sizeof(int));
            if (pfv->u.ivals == NULL) goto allocate_error;
            *pfv->u.ivals = x.u.i4;
/*
printf("...const expression evaluated to %d\n", x.u.i4);
*/
            break;
         case FLOAT_RETURN:
            if (ret_type == INT_RETURN) x.u.r4 = r4 = x.u.i4;
            if (ret_type == DOUBLE_RETURN) x.u.r4 = r4 = x.u.r8;
            pfv->u.fvals = (float *) malloc(sizeof(float));
            if (pfv->u.fvals == NULL) goto allocate_error;
            *pfv->u.fvals = x.u.r4;
            break;
         case DOUBLE_RETURN:
            if (ret_type ==  INT_RETURN) x.u.r8 = r8 = x.u.i4;
            pfv->u.dvals = (double *) malloc(sizeof(double));
            if (pfv->u.dvals == NULL) goto allocate_error;
            *pfv->u.dvals = x.u.r8;
            break;
         default:
            goto err_return;
            break;
         }
      }
      else
      {
/*
printf("...saving variable expression: %s\n", ps);
*/
         /* is a run-time expression, save and evaluate later */
         for (i=1, a=ps; *a != '\0' && *a != ','; i++, a++);
         pfv->expr = (char *) malloc(sizeof(char) * i);
         if (pfv->expr == NULL) goto allocate_error;
         strncpy(pfv->expr, ps, i);
         pfv->expr[i-1] = '\0';
      }
      if (pfv->value_count != 1) goto err_return;
   }
   else
   {
      i = pfv->value_count;
      switch (declared_type)
      {
      case INT_RETURN:
         pfv->u.ivals = (int *) malloc(sizeof(int) * i);
         if (pfv->u.ivals == NULL) goto err_return;
         for (j=0, pi = pfv->u.ivals; j<i; j++, pi++)
         {
            n = sscanf(ps,"%d%n", pi, &k);
            if (n != 1) goto err_return;
            ps = &ps[k+1];
         }
         break;
      case FLOAT_RETURN:
         pfv->u.fvals = (float *) malloc(sizeof(float) * i);
         if (pfv->u.fvals == NULL) goto err_return;
         for (j=0, pf = pfv->u.fvals; j<i; j++, pf++)
         {
            n = sscanf(ps,"%f%n", pf, &k);
            if (n != 1) goto err_return;
            ps = &ps[k+1];
         }
         break;
      case DOUBLE_RETURN:
         pfv->u.dvals = (double *) malloc(sizeof(double) * i);
         if (pfv->u.dvals == NULL) goto allocate_error;
         for (j=0, pd = pfv->u.dvals; j<i; j++, pd++)
         {
            n = sscanf(ps,"%lf%n", pd, &k);
            if (n != 1) goto err_return;
            ps = &ps[k+1];
         }
         break;
      case CHAR_RETURN:
         pfv->u.cvals = (char *) malloc(sizeof(char) * i);
         if (pfv->u.cvals == NULL) goto allocate_error;
         for (j=0, pc = pfv->u.cvals; j<i; j++, pc++, ps++)
             *pc = *ps;
         break;
      default:
         printf("%s: current data type is invalid.\n", rn);
         goto err_return;
         break;
      }
   }
   return status;

err_return:
  return ERR;
allocate_error:
   printf("%s: error allocating memory.\n", rn);
   return ERR;
}

void determine_validation_op( s, t, v )
   char *s;		/* string version of operator, eg.: GT,GE,EQ,LE,LT... */
   char *t;		/* string version of operand, possibly a date-time */
   int *v;		/* buffer to receive integer operator code */
{
   char *rn="DETERMINE_VALIDATION_OP";

   *v = NO_OP;
   switch(*s)
   {
   case 'G':
      if (s[1] == 'T' && s[2] == '\0') *v = GT_OP;
      else if (s[1] == 'E' && s[2] == '\0') *v = GE_OP;
      break;
   case 'E': 
      if (s[1] == 'Q' && s[2] == '\0') *v = EQ_OP;
      break;
   case 'L':
      if (s[1] == 'T' && s[2] == '\0') *v = LT_OP;
      else if (s[1] == 'E' && s[2] == '\0') *v = LE_OP;
      break;
   case 'N':
      if (strcmp(s,"NE") == 0) *v = NE_OP;
      else if (strcmp(s,"NBT")==0) *v = NOT_BETWEEN_OP;
      break;
   case 'A':
      if (strcmp(s,"AND") == 0) *v = AND_OP;
      break;
   case 'O':
      if (s[1] == 'R' && s[2] == '\0') *v = OR_OP;
      break;
   case 'M':
      if (strcmp(s,"MO") == 0) *v = IS_BIT_SET_OP;
      break;
   default:
      break;
   }

   if (*v == NO_OP) printf("%s: unknown operator specified: %s.\n",rn,s);

   if (*v != NO_OP && *t == 'D')
   {
      switch (*v)
      {
      case GE_OP: *v = DATE_GE_OP; break;
      case GT_OP: *v = DATE_GT_OP; break;
      case LE_OP: *v = DATE_LE_OP; break;
      case LT_OP: *v = DATE_LT_OP; break;
      default:
         printf("%s: operation not permitted with DATE type.\n",rn);
         *v = NO_OP;
         break;
      }
   }

   return;
}

static int parse_validation_clause( ps, psni )
   char *ps;			/* points to VALIDATION=... string */
   struct same_name_item *psni;
{
   char *rn="PARSE_VALIDATION_CLAUSE";
   struct item_attributes *pia;
   char s[5][64];
   char *a=s[0], *b=s[1], *c=s[2], *d=s[3], *e=s[4], *f;
   int i,j,k,m,n;
   int second_attribute;
   int dummy;
   int ok;

   ok = new_item_attribute_node(&pia, &(psni->head));
   if (ok != OK) goto new_item_node_error;
   pia->kind_of = VALIDATION;
   psni->flags = psni->flags | VALIDATION;
   pia->u.pv = (struct validation *) malloc(sizeof(struct validation));
   if (pia->u.pv == NULL) goto allocate_error;

   n = sscanf(ps,"VALIDATION=%[^,],%[^,],%[^,],%[^,],%[^,]", a,b,c,d,e);
   if (!(n==3 || n==5)) {printf("%s: wrong argument count.\n",rn); return ERR;}
   if (n==3)
   {
      d = e = NULL;
      f = c;
   }
   else
      f = e;
   while (*f > ' ') f++;
   *f = '\0';

   /* store the validation item name */
   i = strlen(a) + 1;
   if (i==1) {printf("%s: missing item name.\n",rn); return ERR;}
   pia->u.pv->name = (char *) malloc(i);
   if (pia->u.pv->name == NULL) goto allocate_error;
   strcpy(pia->u.pv->name, a);

   /* determine the validation operator : {GT,GE,EQ,LE,LT,...} */
   pia->u.pv->operation = NO_OP;
   determine_validation_op( b, c, &pia->u.pv->operation );
   if (pia->u.pv->operation == NO_OP) return ERR;

   ok = read_wid_int( c, &pia->u.pv->val1, &pia->u.pv->val2 );
   if (ok != OK) {printf("%s: cannot read %s.\n", rn,c); return ERR;}

   if (d == NULL) return OK;

   /* two part validation */

   second_attribute = 1;
   if (*e == 'D') {printf("%s: no compound date test.\n",rn); return ERR;}
   if (*c != 'D') /* date validation uses both operands */
   {
      ok = read_wid_int( e, &(pia->u.pv->val2), &dummy );
      if (ok != OK) {printf("%s: cannot read %s.\n", rn,e); return ERR;}
      determine_validation_op( d, e, &j );
      if (j == NO_OP) return ERR;
      i = pia->u.pv->operation;
      if ( i == LE_OP && j == GE_OP )
      {
         second_attribute = 0;
         pia->u.pv->operation = BETWEEN_OP_INC;
      }
      else
         if ( i == LT_OP && j == GT_OP )
         {
            second_attribute = 0;
            pia->u.pv->operation = BETWEEN_OP_EXC;
         }
   }
   else
      if (second_attribute == 1)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = VALIDATION;
         psni->flags = psni->flags | VALIDATION;
         pia->u.pv = (struct validation *) malloc(sizeof(struct validation));
         if (pia->u.pv == NULL) goto allocate_error;
         i = strlen(a) + 1;
         if (i==1) {printf("%s: missing item name (2).\n",rn); return ERR;}
         pia->u.pv->name = (char *) malloc(i);
         if (pia->u.pv->name == NULL) goto allocate_error;
         strcpy(pia->u.pv->name, a);
         determine_validation_op( d, e, & pia->u.pv->operation );
         if (pia->u.pv->operation == NO_OP) return ERR;
         ok = read_wid_int( e, &(pia->u.pv->val1), &(pia->u.pv->val2) );
         if (ok != OK) {printf("%s: cannot read %s.\n", rn,e); return ERR;}
      }

   return OK;

   allocate_error:
   printf("%s: error allocating memory.\n", rn);
   return ERR;
   new_item_node_error:
   printf("%s: cannot get new item node.\n", rn);
   return ERR;
}

/* parses ITEM=some_item_name clauses and initializes structures
*/
static int parse_item_clause( s, pen, ppin, ppsni )
   char *s;
   struct event_node *pen;
   struct item_node **ppin;
   struct same_name_item **ppsni;
{
   char *ps, *pz;
   char *a;
   char z[256];
   int i;
   struct item_node *pin;
   struct same_name_item *psni;
   struct same_name_item *psni2;
   int depth;

   ps = s;

   /* copy the item name to buffer, null terminate, and possibly
      truncate item name at beginning of embedded parameter list, if
      this item name contains one
   */
   for (a = &ps[5], pz = z, i=0;
       *a > ' ' && *a != ITEM_NAME_PARAM_INTRO && i < 254;
       *pz = *a, pz++, a++, ++i);
   if (i == 0) return ERR;
   if (*a == ITEM_NAME_PARAM_INTRO) {*pz = *a; pz++; i++;}
   *pz = '\0';
   i++;
   depth = 0;
   pin = tree_search(pen->head, z, &depth);
   if (pin == NULL)			/* new item name */
   {
      pin = (struct item_node *) malloc(sizeof(struct item_node));
      if (pin == NULL) goto item_allocate_error;
      pin->list = NULL;
      pin->item_name = (char *) malloc(i);
      if (pin->item_name == NULL) goto item_allocate_error;
      strcpy(pin->item_name, z);

      rb_insert(&(pen->head), pin); /* put item name in search tree */
   }
   *ppin = pin;

   /* create a same-named-item node for the same-named-item list */
   psni = (struct same_name_item *) 
         malloc(sizeof(struct same_name_item));
   if (psni == NULL) goto item_allocate_error;
   *ppsni = psni;
   psni->prev  = NULL;
   psni->next  = NULL;
   psni->flags = 0;
   psni->flags = INT_RETURN;
   psni->head  = NULL;

   /* append the same-named-item to the list */
   if (pin->list == NULL) pin->list = psni;
   else
   {
      psni2 = pin->list;
      while (psni2->next != NULL) psni2 = psni2->next;
      psni2->next = psni;
      psni->prev = psni2;
   }

   return OK;

item_allocate_error:
   printf("  Cannot allocate memory for new item.\n");
   return ERR;
}

/* cpdsoi_to_ai - comma/colon and paren delimited string of ints to int array
  This function translates a string, ps, of the form:

	"-1,2,(*,5,-3:2),0,(2,* ,55: 1),0,0,0,0,0,0   !"

into an two dimensional array of integers, ai.  Commas and colons are
delimiters while parens are subdelimiters.  In this example d1==11, 
d2=4, and ai becomes:

	ai[0][0]=-1,  ai[0][1]=0,  ai[0][2]=0,  ai[0][3]=0
	ai[1][0]= 2,  ai[1][1]=0,  ai[1][2]=0,  ai[1][3]=0
	ai[2][0]= 4,  ai[2][1]=15, ai[2][2]=61, ai[2][3]=7
	...

   Returns OK for success and ERR for any error.  
*/
static int cpdsoi_to_ai(ps, ai, d1, d2)
   char *ps;
   int *ai;
   int d1;
   int d2;
{
   int i,j,k,n;
   int in_paren;

   for (i=0; i < (d1 * d2); i++) ai[i] = 0;

   in_paren = NO;
   for (i=j=0; *ps != '\0'; ++ps)
   {
      k = (i * d2) + j;
      switch (*ps)
      {
      case ' ':
      case '\t':
         break;
      case '!':
      case '#':
         /* remaining portion of line is a comment */
         *(ps--) = '\0';
         break;
      case ':':
      case ',':
         if (in_paren == NO) { i++; j=0; }
         else                {      j++; }
         break;
      case '(':
         if (in_paren == YES) return ERR;
         in_paren = YES;
         break;
      case ')':
         if (in_paren == NO) return ERR;
         in_paren = NO;
         break;
      case '*':
         ai[k] = W_USE_RANGE_LIMIT;
         break;
      default:
         n = sscanf(ps, "%d", &(ai[k]) );
         if (n != 1) { return ERR; break; }
/*
         if (n != 1) { printf("Error reading int at %s.\n", ps); break; }
*/
         if (*ps == '-') ps++;
         while ( *ps >= '0' && *ps <= '9') ps++; ps--;
         break;
      }
      if (i >= d1 || j >= d2) return ERR;
   }

   return OK;
}

static int parse_cdf_clause( ps, psni )
   char *ps;
   struct same_name_item *psni;
{
   char *rn="parse_cdf_clause";
   struct cdf_item *pcdf;
   struct item_attributes *pia;
   int ok;
   char *a, *b, *c, *d;
   int i,j,k,n;
   struct string_list *psl, **ppsl;

   if ( (psni->flags & CDF_ITEM) == 0)
   {
      ok = new_item_attribute_node(&pia, &(psni->head));
      if (ok != OK) goto item_allocate_error;
      pia->kind_of = CDF_ITEM;
      psni->flags = psni->flags | CDF_ITEM;
      pia->u.pcdf = (struct cdf_item *) malloc(sizeof(struct cdf_item));
      if (pia->u.pcdf == NULL) goto item_allocate_error;
      pia->u.pcdf->dependent_rv         = NULL;
      pia->u.pcdf->independent_rv       = NULL;
      pia->u.pcdf->independent_rv_match = NULL;
      pia->u.pcdf->file_list            = NULL;
      pia->u.pcdf->use_dependent_rv_ranges = NO;
      /* default search mode strategy */
      pia->u.pcdf->search_mode          = W_CDF_SEARCH_INTERPOLATE;
      pia->u.pcdf->search_mode_2nd      = W_CDF_SEARCH_NEAREST;
      for (i=0; i<MAX_CDF_INDICES+1; i++)
      {
         pia->u.pcdf->dependent_rv_indices[i] = 0;
         pia->u.pcdf->dependent_rv_range_order[i] = -1;
         pia->u.pcdf->independent_rv_indices[i] = 0;
      }
   }
   else
   {
      ok = get_ptr_to_item_attribute(&pia, psni, CDF_ITEM);
      if (ok != OK) goto bad_cdf_item_clause;
   }

   ps = &ps[4];
   if ( (b = strstr(ps,"FILE=")) != NULL)
   {
      ps = &b[5];
      while (*ps <= ' ' && *ps != '\0') ++ps;
      for (a=ps, i=1; *a > ' '; ++a, ++i);
      if (i == 1) goto bad_cdf_item_clause;
      *a = '\0';
      psl = (struct string_list *) malloc(sizeof(struct string_list));
      if (psl == NULL) goto item_allocate_error;
      psl->next = NULL;
      psl->str  = NULL;
      psl->str = (char *) malloc(i);
      if (psl->str == NULL) goto item_allocate_error;
      strcpy(psl->str, ps);
      ppsl = &(pia->u.pcdf->file_list);
      if (*ppsl != NULL)
      {
         while ((*ppsl)->next != NULL) ppsl = &(*ppsl)->next;
         (*ppsl)->next = psl;
      }
      else
         *ppsl = psl;
      return OK;
   }

   if ((b = strstr(ps,"INDEPENDENT_RV_INDICES=")) != NULL)
   {
      ps = a = &b[23]; 
      n = sscanf(ps,"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
          &(pia->u.pcdf->independent_rv_indices[0]),
          &(pia->u.pcdf->independent_rv_indices[1]),
          &(pia->u.pcdf->independent_rv_indices[2]),
          &(pia->u.pcdf->independent_rv_indices[3]),
          &(pia->u.pcdf->independent_rv_indices[4]),
          &(pia->u.pcdf->independent_rv_indices[5]),
          &(pia->u.pcdf->independent_rv_indices[6]),
          &(pia->u.pcdf->independent_rv_indices[7]),
          &(pia->u.pcdf->independent_rv_indices[8]),
          &(pia->u.pcdf->independent_rv_indices[9]),
          &(pia->u.pcdf->independent_rv_indices[10]) );
      if (n < 1) goto bad_cdf_item_clause;
      return OK;
   }

   if ((b = strstr(ps,"DEPENDENT_RV_INDICES=")) != NULL)
   {
      ps = a = &b[21];
      for (;
           (*a >= ' ' || *a == '\t') && *a != '(' && *a != '!' && *a != '#';
           ++a);
      if (*a == '(')
      {
         /* if a paren is found then this is an array item and we
            must get the multi-dimensional loop extract instructions
            for the range
         */
         ok = cpdsoi_to_ai(ps, &(pia->u.pcdf->dependent_rv_ranges[0]),
              W_N_CDF_INDICES, 4);
         pia->u.pcdf->use_dependent_rv_ranges = YES;
         /* layout the order in which the ranges are to be processed */
         for (i=0; i < W_N_CDF_INDICES; i++)
             if ( (k = pia->u.pcdf->dependent_rv_ranges[i][3]) > 0 &&
                  k <= W_N_CDF_INDICES)
                 pia->u.pcdf->dependent_rv_range_order[--k] = i;
         /* verify the ordering here someday... */
         return ok;
      }
      n = sscanf(ps,"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
          &(pia->u.pcdf->dependent_rv_indices[0]),
          &(pia->u.pcdf->dependent_rv_indices[1]),
          &(pia->u.pcdf->dependent_rv_indices[2]),
          &(pia->u.pcdf->dependent_rv_indices[3]),
          &(pia->u.pcdf->dependent_rv_indices[4]),
          &(pia->u.pcdf->dependent_rv_indices[5]),
          &(pia->u.pcdf->dependent_rv_indices[6]),
          &(pia->u.pcdf->dependent_rv_indices[7]),
          &(pia->u.pcdf->dependent_rv_indices[8]),
          &(pia->u.pcdf->dependent_rv_indices[9]),
          &(pia->u.pcdf->dependent_rv_indices[10]) );
      if (n < 1) goto bad_cdf_item_clause;
      return OK;
   }

   if ((b = strstr(ps,"INDEPENDENT_RV_MATCH=")) != NULL)
   {
      ps = &b[21];
      while (*ps <= ' ' && *ps != '\0') ++ps;
      for (a=ps, i=1; *a > ' '; ++a, ++i);
      if (i == 1) goto bad_cdf_item_clause;
      *a = '\0';
      pia->u.pcdf->independent_rv_match = (char *) malloc(i);
      if (pia->u.pcdf->independent_rv_match == NULL)
          goto item_allocate_error;
      strcpy(pia->u.pcdf->independent_rv_match, ps);
      return OK;
   }

   if ((b = strstr(ps,"INDEPENDENT_RV=")) != NULL)
   {
      ps = &b[15];
      while (*ps <= ' ' && *ps != '\0') ++ps;
      for (a=ps, i=1; *a > ' '; ++a, ++i);
      if (i == 1) goto bad_cdf_item_clause;
      *a = '\0';
      pia->u.pcdf->independent_rv = (char *) malloc(i);
      if (pia->u.pcdf->independent_rv == NULL) goto item_allocate_error;
      strcpy(pia->u.pcdf->independent_rv, ps);
      return OK;
   }

   if ((b = strstr(ps,"DEPENDENT_RV=")) != NULL)
   {
      ps = &b[13];
      while (*ps <= ' ' && *ps != '\0') ++ps;
      for (a=ps, i=1; *a > ' '; ++a, ++i);
      if (i == 1) goto bad_cdf_item_clause;
      *a = '\0';
      pia->u.pcdf->dependent_rv = (char *) malloc(i);
      if (pia->u.pcdf->dependent_rv == NULL) goto item_allocate_error;
      strcpy(pia->u.pcdf->dependent_rv, ps);
      return OK;
   }

   if ((b = strstr(ps,"SEARCH_MODE=")) != NULL)
   {
      a = c = &b[12];
      if ( a == strstr(c,"LINEAR") )
      {
         pia->u.pcdf->search_mode = W_CDF_SEARCH_INTERPOLATE;
         if (a[6] == ',')
         {
            switch ( a[7] )
            {
            case 'N':
               pia->u.pcdf->search_mode_2nd = W_CDF_SEARCH_NEAREST;
               break;
            case 'E':
               pia->u.pcdf->search_mode_2nd = W_CDF_SEARCH_EARLIER;
               break;
            case 'L':
               pia->u.pcdf->search_mode_2nd = W_CDF_SEARCH_LATER;
               break;
            default:
               goto bad_cdf_item_clause;
               break;
            }
         }
      }
      else if ( a == strstr(c,"NEAREST") )
         pia->u.pcdf->search_mode = W_CDF_SEARCH_NEAREST;
      else if ( a == strstr(c,"EARLIER") )
         pia->u.pcdf->search_mode = W_CDF_SEARCH_EARLIER;
      else if ( a == strstr(c,"LATER") )
         pia->u.pcdf->search_mode = W_CDF_SEARCH_LATER;
      else
         goto bad_cdf_item_clause;
      return OK;
   }

   return ERR;

item_allocate_error:
   printf("%s: cannot allocate memory for item structure.\n", rn);
   return ERR;

bad_cdf_item_clause:
   return ERR;
}

static int read_items_from_file(filename, widf, everything, pen)
   char *filename;
   FILE *widf;
   int everything;
   struct event_node *pen;
{
   char *rn="READ_ITEMS_FROM_FILE";
   char s[MAX_INPUT_LINE_SIZE];
   FILE *widf2;
   char z[256];
   char c4[4];
   char *ps;
   char *pz;
   char *pc;
   int line_number;
   int i,j,k,m,n;
   int *pi;
   float *pf;
   float fval;
   double *pd;
   double dval;
   char *a, *b, *c, *d;
   struct item_node *pin;
   struct same_name_item *psni;
   struct item_attributes *pia;
   struct function_arg_list *pfal, *pfal2;
   struct function *pfun;
   struct lookup *plook;
   int n_items_found;
   int ok;
   char f[256];
   int more;

   if (widf == NULL) /* file has not been preopened */
   {
      i = 256;
#ifdef USE_MACOSX
      ok = w_cnvrt_c_fn_by_os_(filename, f, &i);
#else
      ok = w_cnvrt_c_fn_by_os(filename, f, &i);
#endif
      if (ok == OK)
      {
         if ( (widf = fopen(f,"r")) == NULL)
         {
            perror("FOPEN");
            printf("%s: Cannot open %s.\n", rn, f);
            printf("Make sure %s points to a valid directory or search path.\n",
               WID_SEARCH_LIST);
            exit(2);
         }
      }
      else
      {
         printf("%s: filename unuseable: %s.\n", rn, filename);
         return ERR;
      }

   }
   else /* file is already open */
      strncpy(f,filename,256);

   line_number = 0;
   n_items_found = 0;

   while(ps=fgets(s,MAX_INPUT_LINE_SIZE,widf))
   {
      ++line_number;
      if (s[0] == '!') continue;		/* skip over comments */

/*
printf("%d. %s.   ", n_items_found, z);
printf("...search depth=%d...", depth);
printf("...new item name...");
printf("\n");
*/

      /* replace trailing newline char with a null */
      for(a=s, i=1; *a >= ' ' || *a == '\t'; ++a, ++i);
      if (i == 1) continue;
      *a = '\0';
      /* find first non-whitespace character */
      for (; *ps <= ' '; ps++);

/* put goto general_clause_error; after outer switch statement */
/* mark 2 */
      switch(*ps)
      {
      case 'I':
         /* replacing strstr with strncmp should be more effecient */
/*         if ((ps=strstr(s,"ITEM=")) != NULL) */
         if (0 == strncmp(ps,"ITEM=",5))
         {
            ok = parse_item_clause(ps, pen, &pin, &psni);
            if (ok != OK) goto item_clause_error;
            ++n_items_found;
            continue;
         }
         break;
      case 'V':
         if ((ps = strstr(s,"VALIDATION=")) != NULL)
         {
            strcpy(z,ps);
            ok = parse_validation_clause( z, psni );
            if (ok != OK) goto general_clause_error;
            continue;
         }
         break;
      case 'C':
         if ((ps = strstr(s,"CDF_")) != NULL)
         {
            ok = parse_cdf_clause(ps, psni);
	    if (ok != OK) goto general_clause_error;
            continue;
         }
         break;
      default: break;
      }


      if ((ps = strstr(s,"DATA_TYPE=")) != NULL)
      {
         i = ~TYPE_RETURN_MASK;
         psni->flags = psni->flags & i;
         ps = &ps[10];
         if (0 == strncmp(ps,"INTEGER*4", 9)) 
            psni->flags = psni->flags | INT_RETURN;
         else if (0 == strncmp(ps,"REAL*4", 6)) 
            psni->flags = psni->flags | FLOAT_RETURN;
         else if (0 == strncmp(ps,"REAL*8", 6)) 
            psni->flags = psni->flags | DOUBLE_RETURN;
         else if (0 == strncmp(ps,"CHARACTER", 9)) 
            psni->flags = psni->flags | CHAR_RETURN;
         else if (0 == strncmp(ps,"UR8", 3))
         {
            psni->flags = psni->flags | DOUBLE_RETURN;
            psni->flags = psni->flags | UR8_CONVERT;
         }
         else
            goto data_type_err;
         continue;
      }

      if ((ps = strstr(s,"EXTRACT=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = EXTRACT;
         psni->flags = psni->flags | EXTRACT;
         pia->u.pe = (struct extract *) malloc(sizeof(struct extract));
         if (pia->u.pe == NULL) goto item_allocate_error;
         pia->u.pe->flags = 0;
         pia->u.pe->expr[0] = NULL;
         pia->u.pe->expr[1] = NULL;
         pia->u.pe->expr[2] = NULL;
         pia->u.pe->expr[3] = NULL;
         pia->u.pe->expr[4] = NULL;
         ok = parse_extract_clause(ps, pia->u.pe, psni->flags);
         if (ok != OK) goto bad_extract_clause;
         /* optimize extract= clause for whole bytes, longwords, etc. */
         if ((pia->u.pe->flags & EXTRACT_HAS_EXPR) == 0)
         {
            ok = w_optimize_extract(pia->u.pe);
            if (ok != OK) printf("%s: extract optimize err.\n", rn);
         }
         continue;
      }

      if ((ps = strstr(s,"FIXED_VALUE=")) != NULL)
      {
/*
printf("...evaluating const expression: %s\n", ps);
*/
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = FIXED_VALUE;
         psni->flags = psni->flags | FIXED_VALUE;
         pia->u.pfv = (struct fixed_value *) malloc(sizeof(struct fixed_value));
         if (pia->u.pfv == NULL) goto item_allocate_error;
         n = sscanf(ps,"FIXED_VALUE=%d:%n", &i, &k);
         if (n != 1) goto bad_fixed_value_clause;
         if (k > 18) goto bad_fixed_value_clause; /* probably a missing ":" */
         ps = &ps[k];
         pia->u.pfv->value_count = i;
         pia->u.pfv->expr = NULL;
         pia->u.pfv->u.ivals = NULL;
         ok = parse_fixed_value_clause(ps, pia->u.pfv, psni->flags);
         if (ok != OK) goto bad_fixed_value_clause;
         continue;
      }

      if ((ps = strstr(s,"XLATE=")) != NULL)
      {
         if ((psni->flags & XLATE_PRESENT) == 0)
         {
            ok = new_item_attribute_node(&pia, &(psni->head));
            if (ok != OK) goto new_item_node_error;
            pia->kind_of = XLATE_PRESENT;
            psni->flags = psni->flags | XLATE_PRESENT;
         }
         else
         {
            ok = get_ptr_to_item_attribute(&pia, psni, XLATE_PRESENT);
            if (ok != OK) goto bad_xlate_clause_a;
         }

         n = sscanf(ps,"XLATE=%d,\"%[^\"]", &i, z);
         if (n != 2) goto bad_xlate_clause_b;
         ok = append_xlate_instance(i, z, &pia->u.xlate_ptr);
         if (ok != OK) goto bad_xlate_clause_c;
         continue;
      }

      if ((ps = strstr(s,"ARG=")) != NULL)
      {
         if ( (psni->flags & FUNCTION_NUMBER) == 0) 
            goto missing_function_clause;
         ps = a = &ps[4];
         pfal = (struct function_arg_list *) 
                     malloc(sizeof(struct function_arg_list));
         if (pfal == NULL) goto item_allocate_error;
         pfal->next = NULL;
         pfal->data_type = 0;
         pfal->u.ival[0] = 0;
         pfal->u.ival[1] = 0;
         for (i=1; *a > ' '; a++, i++);
         *a = '\0';
         pfal->pc = (char *) malloc(i);
         if (pfal->pc == NULL) goto item_allocate_error;
         strcpy(pfal->pc, ps);         
         if (pfun->head == NULL)
            pfun->head = pfal;
         else
         {
           for (pfal2 = pfun->head; pfal2->next != NULL; pfal2 = pfal2->next);
           pfal2->next = pfal;
         }
         continue;
      }

      m = 0;
      if ((ps = strstr(s,"FUNCTION=")) != NULL) m = 1;
      if (m==0)
         if ((ps = strstr(s,"POLY=")) != NULL) m = 2;
      if (m != 0)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = FUNCTION_NUMBER;
         psni->flags = psni->flags | FUNCTION_NUMBER;
         pia->u.pfun = (struct function *) malloc(sizeof(struct function));
         if (pia->u.pfun == NULL) goto item_allocate_error;
         pia->u.pfun->head = NULL;
         pfun = pia->u.pfun;
         if (m == 1)
            n = sscanf(ps,"FUNCTION=%d,%s", &pia->u.pfun->function_number, z);
         else 
            n = sscanf(ps,"POLY=%d,%s", &pia->u.pfun->function_number, z);
         if (n == 0) goto bad_function_clause;
         if (n == 1) continue;
         /* Auxilliary argument list of item names or numerical constants
            is present.
            pre 15-jun-96: This comma-delimited list contains no whitespace.
            post 15-jun-96: This comma-delimited list may contain whitespace.
            19-may-97, jk, whitespace previously NOT ok
         */
         pc = ps;
         while (*pc != ',') ++pc;
         ++pc;
         j = strlen(pc) + 1;
         c = (char *) malloc(j);
         if (c == NULL) goto item_allocate_error;
         strcpy(c,pc);
         a = c;
         pfal = (struct function_arg_list *) 
                     malloc(sizeof(struct function_arg_list));
         if (pfal == NULL) goto item_allocate_error;
         pia->u.pfun->head = pfal;
         while (*a != '\0')
         {
            /* skip over whitespace */
            while (*a == ' ' || *a == '\t') a++;
            /* save pointer to beginning of string */
            pfal->pc = a;
            pfal->next = NULL;
            pfal->data_type = 0;
            pfal->u.ival[0] = 0;
            pfal->u.ival[1] = 0;
            /* find end of function arg field denoted by comma or whitespace */
            while (*a != ',' && *a != '\0' && *a > ' ') a++;
            if (*a != '\0') more = YES; else more = NO;
            *a = '\0';
            if (more == YES)
            { 
               a++;
               while (*a == ',' && *a != '\0' && *a <= ' ') a++;
               if (*a == '\0') more = NO;
            }
            if (more == YES)
            { 
               pfal->next = (struct function_arg_list *) 
                     malloc(sizeof(struct function_arg_list));
               if (pfal->next == NULL) goto item_allocate_error;
               pfal = pfal->next;
            }
         }
         continue;
      }

      if ((ps = strstr(s,"COMPOSITE=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = COMPOSITE;
         psni->flags = psni->flags | COMPOSITE;
         ps = a = &ps[10];
         i = 1;
         while (*a > ' ') {++a; ++i;}
         if (i == 1) goto bad_composite_clause;
         *a = '\0';
         pia->u.chain = (char *) malloc(i);
         if (pia->u.chain == NULL) goto item_allocate_error;
         strcpy(pia->u.chain, ps);
         continue;
      }

      if ((ps = strstr(s,"LOOKUP=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         psni->flags = psni->flags | LOOKUP_TABLE;
         pia->kind_of = LOOKUP_TABLE;
         pia->u.plook = (struct lookup *) malloc(sizeof(struct lookup));
         if (pia->u.plook == NULL) goto item_allocate_error;
         plook = pia->u.plook;
         ps = a = &ps[7];
         i = 1;
         while (*a > ' ') {++a; ++i;}
         if (i == 1) goto bad_lookup_clause;
         *a = '\0';
         pia->u.plook->name = (char *) malloc(i);
         if (pia->u.plook->name == NULL) goto item_allocate_error;
         strcpy(pia->u.plook->name, ps);
         for (i=0; i < MAX_LOOKUP_TBL_ENTRIES; i++) plook->u.itbl[i] = 0;
         continue;
      }

      if ((ps = strstr(s,"TABLE=")) != NULL)
      {
         if ( (psni->flags & LOOKUP_TABLE) == 0) goto missing_lookup_clause;
         ps = a = &ps[6];
         switch (TYPE_RETURN_MASK & psni->flags)
         {
         case INT_RETURN:
            n = sscanf(ps,"%d,%d", &i, &k);
            if (n != 2) goto bad_lookup_table_clause;
            if (i < 0 || i >= MAX_LOOKUP_TBL_ENTRIES) 
               goto bad_lookup_table_clause;
            plook->u.itbl[i] = k;
            break;
         case FLOAT_RETURN:
            n = sscanf(ps,"%d,%f", &i, &fval);
            if (n != 2) goto bad_lookup_table_clause;
            if (i < 0 || i >= MAX_LOOKUP_TBL_ENTRIES) 
               goto bad_lookup_table_clause;
            plook->u.ftbl[i] = fval;
            break;
         case DOUBLE_RETURN:
            n = sscanf(ps,"%d,%lf", &i, &dval);
            if (n != 2) goto bad_lookup_table_clause;
            if (i < 0 || i >= MAX_LOOKUP_TBL_ENTRIES) 
               goto bad_lookup_table_clause;
            plook->u.dtbl[i] = dval;
            break;
         case CHAR_RETURN:
            n = sscanf(ps,"%d,", &i);
            if (n != 1) goto bad_lookup_table_clause;
            if (i < 0 || i >= MAX_LOOKUP_TBL_ENTRIES) 
               goto bad_lookup_table_clause;
	    if (plook->u.ctbl[i] != NULL) goto bad_lookup_table_clause;
            while(*ps != ',') ps++; /* find the comma */
            ps++;
            pc = ps++; /* this is the delimiter */
            for (j=1; *ps != *pc && *ps >= ' '; ps++, j++);
	    if (*ps == *pc)
            {
               *ps = '\0';
               plook->u.ctbl[i] = (char *) malloc(j);
               if (plook->u.ctbl[i] == NULL) goto item_allocate_error;
	       strcpy(plook->u.ctbl[i], pc);
            }
	    else
            {
	       printf("  No trailing delimiter found in TABLE=n,'str'.\n");
	       goto bad_lookup_table_clause;
            }
            break;
         default:
            printf("%s: unknown LOOKUP table data type.\n", rn);
            goto bad_lookup_table_clause;
            break;
         }
         continue;
      }


      if ((ps = strstr(s,"FILE=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = FILE_NAME;
         psni->flags = psni->flags | FILE_NAME;
         ps = a = &ps[5];
         i = 1;
         while (*a > ' ') {++a; ++i;}
         if (i == 1) goto bad_file_clause;
         *a = '\0';
         pia->u.myfile = (char *) malloc(i);
         if (pia->u.myfile == NULL) goto item_allocate_error;
         strcpy(pia->u.myfile, ps);
         continue;
      }

      if ((ps = strstr(s,"COUNT=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = COUNT_ITEM;
         psni->flags = psni->flags | COUNT_ITEM;
         ps = a = &ps[6];
         i = 1;
         while (*a > ' ') {++a; ++i;}
         if (i == 1) goto bad_count_clause;
         *a = '\0';
         pia->u.count = (char *) malloc(i);
         if (pia->u.count == NULL) goto item_allocate_error;
         strcpy(pia->u.count, ps);
         continue;
      }

      if ((ps = strstr(s,"TEXT=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = TEXT_ITEM;
         psni->flags = psni->flags | TEXT_ITEM;
         i = ~TYPE_RETURN_MASK;
         psni->flags = psni->flags & i;
         psni->flags = psni->flags | CHAR_RETURN;
         ps = a = &ps[5];
         i = 1;
         while (*a >= ' ') {++a; ++i;}
         if (i == 1) goto bad_text_clause;
         *a = '\0';
         pia->u.text = (char *) malloc(i);
         if (pia->u.text == NULL) goto item_allocate_error;
         strcpy(pia->u.text, ps);
         continue;
      }

      if ((ps=strstr(s,"#include")) != NULL)
      {
         widf2 = NULL;
         pz = &ps[9];
         while(*pz <= ' ') pz++;
         for (pc=pz; *pc > ' '; pc++);
         *pc = '\0';
         ok = read_items_from_file(pz, widf2, everything,  pen);
         if (ok != OK)
         {
           printf("%s: error opening/reading include file: %s.\n", rn, pz);
           printf("%s: will continue without included items.\n", rn);
         }
         continue;
      }

      if ((ps = strstr(s,"FORMAT=")) != NULL)
      {
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = FORMAT;
         psni->flags = psni->flags | FORMAT;
         ps = a = &ps[7];
         i = 2;
         if (*ps != '(') goto bad_format_clause;
         while (*a >= ' ' && *a != ')') {++a; ++i;}
         if (*a != ')') goto bad_format_clause;
         a++;
         *a = '\0';
         pia->u.format = (char *) malloc(i);
         if (pia->u.format == NULL) goto item_allocate_error;
         strcpy(pia->u.format, ps);
         continue;
      }

      /* these next clauses are for special routines that query
         ancilliary item information
      */

      if ( (ps = strstr(s,"DESC=")) != NULL )
      {
         if (everything != YES) continue;
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = DESCRIPTION;
         psni->flags = psni->flags | DESCRIPTION;
         ps = a = &ps[5];
         i = 1;
         while (*a >= ' ') {++a; ++i;}
         if (i == 1) goto bad_desc_clause;
         *a = '\0';
         pia->u.desc = (char *) malloc(i);
         if (pia->u.desc == NULL) goto item_allocate_error;
         strcpy(pia->u.desc, ps);
         continue;
      }

      if ( (ps = strstr(s,"AUTHOR_DATE=")) != NULL )
      {
         if (everything != YES) continue;
         ok = new_item_attribute_node(&pia, &(psni->head));
         if (ok != OK) goto new_item_node_error;
         pia->kind_of = AUTHOR_DATE;
         psni->flags = psni->flags | AUTHOR_DATE;
         ps = a = &ps[12];
         i = 1;
         while (*a >= ' ') {++a; ++i;}
         if (i == 1) goto bad_desc_clause;
         *a = '\0';
         pia->u.author_date = (char *) malloc(i);
         if (pia->u.author_date == NULL) goto item_allocate_error;
         strcpy(pia->u.author_date, ps);
         continue;
      }

      if ((ps = strstr(s,"PHYSICAL")) != NULL)
      {
         i = 1;
         pz = strstr(s,"!");
         if (pz != NULL && pz < ps) i = 0;
         if (ps > s && ps[-1] > ' ') i = 0;
         if (ps[8] > ' ' && ps[8] != '!') i = 0;
         if (i == 1)
         {
            ok = new_item_attribute_node(&pia, &(psni->head));
            if (ok != OK) goto new_item_node_error;
            psni->flags = psni->flags | PROCEDURE_NUMBER;
            pia->kind_of = PROCEDURE_NUMBER;
            pia->u.pproc = (struct procedure *) 
                           malloc(sizeof(struct procedure));
            if (pia->u.pproc == NULL) goto item_allocate_error;
            pia->u.pproc->procedure_number = PHYSICAL_UNITS_PROC;
            continue;
         }
         else
            printf("%s: Unexpected keyword 'PHYSICAL' at line %d.\n", 
               rn, line_number);
      }

      /* if we get here then we may have a typo, so try to catch it
         while allowing freedom for comments
      */
      ps = strstr(s,"=");
      if (ps != NULL)
      {
         pz = strstr(s,"!");
         if (pz != NULL && pz < ps) continue;
         printf("%s: Unknown clause at line %d.\n", rn, line_number);
         for (pz = s; *pz <= ' '; pz++);
         printf("  Line is: %s\n", pz);
      }

   }

   fclose(widf);

   if (n_items_found == 0)
   {
      printf("%s: cannot find any items in database %s.\n", rn, f);
   }

   return (n_items_found > 0) ? OK : ERR;

/*
   Error Messages
*/
/* mark 3 */
new_item_node_error:
   printf("%s: cannot get new item node at line#%d.\n", rn, line_number);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

item_allocate_error:
   printf("%s: cannot allocate memory for item, line #%d.\n", rn, line_number);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

item_clause_error:
   printf("  Invalid ITEM= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   printf("  File is: %s\n", f);
   fclose(widf);
   return ERR;

general_clause_error:
   printf("%s: Invalid clause at line# %d.\n", rn, line_number);
   if (pin != NULL)
      if (pin->item_name != NULL)
         printf("  Item is: %s\n", pin->item_name);
   printf("  Line is: %s\n", s);
   printf("  File is: %s\n", f);
   fclose(widf);
   return ERR;

bad_extract_clause:
   printf("  Invalid EXTRACT= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_fixed_value_clause:
   printf("  Invalid FIXED_VALUE= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_fixed_value_allo:
   printf("  Cannot allocate mem for FIXED_VALUE= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_count_clause:
   printf("  Invalid COUNT= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is %s.\n", s);
   fclose(widf);
   return ERR;

bad_function_clause:
   printf("  Invalid FUNCTION= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_composite_clause:
   printf("  Invalid COMPOSITE= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_file_clause:
   printf("  Invalid FILE= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_desc_clause:
   printf("  Invalid DESC= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_text_clause:
   printf("  Invalid TEXT= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;
 
bad_format_clause:
   printf("  Invalid FORMAT= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Make sure leading and trailing parens are supplied.\n");
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;
 
data_type_err:
   printf("  Invalid DATA_TYPE= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;
 
bad_xlate_clause_a:
   printf("  Cannot get pointer to XLATE item list.\n");
   goto bad_xlate_clause;
bad_xlate_clause_b:
   printf("  Wrong number of fields in XLATE= clause.\n");
   goto bad_xlate_clause;
bad_xlate_clause_c:
   printf("  Cannot append XLATE instance to list.\n");
   goto bad_xlate_clause;
bad_xlate_clause:
   printf("  Invalid XTRACT= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_validation_clause:
   printf("  Invalid VALIDATION= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_lookup_clause:
   printf("  Invalid LOOKUP= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

missing_lookup_clause:
   printf("  LOOKUP= clause not found before TABLE= clause at line#%d in %s.\n",
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

missing_function_clause:
   printf("  FUNCTION= clause not found before ARG= clause at line#%d in %s.\n",
      line_number, f);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

bad_lookup_table_clause:
   printf("  Invalid TABLE= clause at line#%d in %s.\n", 
      line_number, f);
   printf("  Values unreadable or out of range [0..%d].\n", 
      MAX_LOOKUP_TBL_ENTRIES-1);
   printf("  Line is: %s\n", s);
   fclose(widf);
   return ERR;

} /* read_items_from_file */

int get_items_for_event(event_name, everything)
   char *event_name;
   int everything;
{
   char *rn="GET_ITEMS_FOR_EVENT";
   char z[256];
   char *ps;
   char *pz;
   char *pc;
   int i;
   int ok;
   FILE *widf;
   struct event_node *pen;
   char *s1;
   char f[256];

   /* build up the datafile name from the WIND_DBMS environment variable*/

#if SYSTEM == SUNOS
   for(pz=z, pc=event_name; *pc != '\0'; pz++, pc++)
      *pz = isupper(i = (int) *pc) ? tolower(i) : *pc;
   *pz = '\0';
   strcpy(f,WID_SEARCH_LIST);
   strcat(f,":items_");
   strcat(f,z);
   strcat(f,".db");
   i = 256;
   ok = w_cnvrt_c_fn_by_os_(f,z,&i);
   if (ok != OK)
   {
      printf("Cannot open/find items_%s.db along search path.\n", z);
      printf("Make sure %s points to a valid directory or search path.\n",
         WID_SEARCH_LIST);
      s1 = getenv(WID_SEARCH_LIST);
      if (s1 != NULL)
         printf("Current search path is %s\n", s1);
      else
         printf("Current search path is not defined.\n");
      exit(2);
   }
   strcpy(f,z);
#endif

#ifdef USE_MACOSX
   for(pz=z, pc=event_name; *pc != '\0'; pz++, pc++)
      *pz = isupper(i = (int) *pc) ? tolower(i) : *pc;
   *pz = '\0';
   strcpy(f,WID_SEARCH_LIST);
   strcat(f,":items_");
   strcat(f,z);
   strcat(f,".db");
   i = 256;
   ok = w_cnvrt_c_fn_by_os_(f,z,&i);
   if (ok != OK)
   {
      printf("Cannot open/find items_%s.db along search path.\n", z);
      printf("Make sure %s points to a valid directory or search path.\n",
         WID_SEARCH_LIST);
      s1 = getenv(WID_SEARCH_LIST);
      if (s1 != NULL)
         printf("Current search path is %s\n", s1);
      else
         printf("Current search path is not defined.\n");
      exit(2);
   }
   strcpy(f,z);
#endif

#if SYSTEM == VMS
   strcpy(f,WID_SEARCH_LIST);
   strcat(f,":items_");
   strcat(f,event_name);
   strcat(f,".db");
#endif

   /* open the existing item datafile */
   if ( (widf = fopen(f,"r")) == NULL)
   {
      perror("FOPEN");
      printf("%s: Cannot open %s.\n", rn, f);
      printf("Make sure %s points to a valid directory or search path.\n",
         WID_SEARCH_LIST);
      exit(2);
   }

#if SYSTEM == VMS
   ps = fgetname(widf, z);
   if (ps == 0) 
   {
      printf("Cannot get full VMS file spec for %s.\n", f);
      exit(2);
   }
   strcpy(f,z);
#endif

   /* allocate a new item list for caller's event */
   strcpy(z,event_name);
   pen = find_event_item_list(z);
   if (pen == NULL)
   {
      if (NULL == (pen = get_new_event_item_list(z)))
      {
         printf("%s cannot get new event list for %s.\n", rn, z);
         fclose(widf);
         return ERR;
      }
      pen->source_filename = (char *) malloc(strlen(f)+1);
      if (pen->source_filename == NULL) goto allocate_error;
      strcpy(pen->source_filename, f);
   }
   else
   {
      printf("%s: ERROR, asked to read items already read.\n", rn);
      fclose(widf);
      return ERR;
   }

   return read_items_from_file(f, widf, everything, pen);

   allocate_error:
   printf("%s: error allocating memory.\n", rn);
   return ERR;
}

int w_get_item_list(event_name, item_name, pin)
   char *event_name;
   char *item_name;
   struct item_node **pin;
{
   char *rn="W_GET_ITEM_LIST";
   char *global_ename="GLOBAL";
   int ok;
   struct event_node *p=NULL;
   int depth;

   if (NULL == (p = find_event_item_list(event_name)))
   {
      if (OK != (ok = get_items_for_event(event_name, NO)))
      {
         printf("Cannot read items for event \"%s\".\n", event_name);
         return ERR;
      }
      p = find_event_item_list(event_name);
      if (p == NULL) {printf("ERR! where's the list just made?\n"); return ERR;}
      if (read_the_alls == NO)
      {
         if (strcmp(global_ename, event_name) != 0)
         {         
            if (OK != (ok = get_items_for_event(global_ename, NO)))
            {
               printf("%s: Cannot read items for event \"%s\".\n", 
                  rn, global_ename);
               return ERR;
            }
         }
         read_the_alls = YES;
      }
   }

   depth = 0;
   *pin = tree_search(p->head, item_name, &depth);

   /* try the "GLOBAL" event list if not already a "GLOBAL" item */
   if (*pin == NULL && strcmp(global_ename, event_name) != 0)
   {
      if (NULL == (p = find_event_item_list(global_ename)))
      {
         printf("Cannot find %s event item list, item=%s.\n", 
            global_ename, item_name);
         return ERR;
      }
      depth = 0;
      *pin = tree_search(p->head, item_name, &depth);
   }

   return (*pin == NULL) ? ERR : OK;
} /* end w_get_item_list */

void wid_show_item_attribute( pia, psni )
   struct item_attributes *pia;
   struct same_name_item *psni;
{
   xlate_item *xli;
   struct function_arg_list *pfal;
   int i,j,k,m,n;

   if (pia == NULL) return;

   switch (pia->kind_of)
   {
   case EXTRACT:
      printf("      EXTRACT=%d %d %d %d %d Area=%c.\n",
         pia->u.pe->startbit,
         pia->u.pe->offset,
         pia->u.pe->rep,
         pia->u.pe->bitlen,
         pia->u.pe->count,
         pia->u.pe->area);
      break;
   case FIXED_VALUE:
      j = pia->u.pfv->value_count;
      printf("      FIXED_VALUE=%d: ", j);
      i = psni->flags & TYPE_RETURN_MASK;
      switch (i)
      {
      case INT_RETURN:
         for (k=0; k<j; k++) printf("%d ", pia->u.pfv->u.ivals[k]);
         break;
      case FLOAT_RETURN:
         for (k=0; k<j; k++) printf("%g ", (double) pia->u.pfv->u.fvals[k]);
         break;
      case DOUBLE_RETURN:
         for (k=0; k<j; k++) printf("%g ", pia->u.pfv->u.dvals[k]);
         break;
      case CHAR_RETURN:
         printf("%s.", pia->u.pfv->u.cvals);
         break;
      default:
         printf("UNKNOWN data type: 0x%x.", i);
         break;
      }
      printf("\n");
      break;
   case VALIDATION:
      printf("      VALIDATION=%s, v1=%d, v2=%d, op=%d.\n", 
         pia->u.pv->name, 
         pia->u.pv->val1, 
         pia->u.pv->val2, 
         pia->u.pv->operation);
      break;
   case COMPOSITE:
      printf("      COMPOSITE=%s.\n", pia->u.chain);
      break;
   case FILE_NAME:
      printf("      FILE_NAME=%s.\n", pia->u.myfile);
      break;
   case TEXT_ITEM:
      printf("      TEXT_ITEM=%s.\n", pia->u.text);
      break;
   case COUNT_ITEM:
      printf("      COUNT_ITEM=%s.\n", pia->u.count);
      break;
   case FUNCTION_NUMBER:
      printf("      FUNCTION_NUMBER=%D.\n", pia->u.pfun->function_number);
      pfal = pia->u.pfun->head;
      while (pfal != NULL) 
      {
         printf("\t\tArgument: %s.\n", pfal->pc);
         pfal = pfal->next;
      }
      break;
   case XLATE_PRESENT:
      xli = pia->u.xlate_ptr;
      printf("      ...xlates...\n");
      while (xli != NULL)
      {
         printf("      XLATE=%s. %d.\n", xli->str, xli->val);
         xli = xli->next;
      }
      break;
   default:
      printf("******UNKNOWN kind_of=%d\n", pia->kind_of);
      break;
   }

   return;
}

void wid_show_item( psni, name )
   struct same_name_item *psni;
   char *name;
{
   struct item_attributes *pia;

   pia = psni->head;
   printf("    Item %s flags=x%X:\n", name, psni->flags);
   while (pia != NULL)
   {
      wid_show_item_attribute( pia, psni );
      pia = pia->next;
   }
   return;
}

void wid_show_item_node( x )
   struct item_node *x;
{
   struct same_name_item *psni;
   int i;

   printf(" x=%08.8x LF=%08.8x RG=%08.8x PA=%08.8x key: %s\n",
          x, x->left, x->right, x->parent, x->item_name);

   psni = x->list;
   i = 1;
   while (psni != NULL)
   {
      printf("%d. ", i);
      wid_show_item( psni, x->item_name );
      ++i;
      psni = psni->next;
   }
}

void wid_show_all_items()
{
   struct event_node *e;

   e = head_event_list;

   while (e != NULL)
   {
      printf("\n  LISTING OF ITEMS FOR EVENT %s:\n\n", e->event_name);
      wid_inorder_traverse(e->head);
      e = e->next;
   }
   return;
}

#ifdef USE_MACOSX
int w_goto_nth_sni_ ( ppin, ppsni, nth )
#else
int w_goto_nth_sni ( ppin, ppsni, nth )
#endif
   struct item_node **ppin;
   struct same_name_item **ppsni;
   int *nth;
{
   int i;
   int j;

   if (*nth < 1) return ERR;
   if (*ppin == NULL) return ERR;
   *ppsni = (*ppin)->list;
   if (*ppsni == NULL) return ERR;
   for(i=0, j= *nth - 1; i < j && *ppsni != NULL; i++, *ppsni = (*ppsni)->next);
   if (*ppsni == NULL) return ERR;
   return OK;
}

#ifdef USE_MACOSX
int w_goto_nth_item_( event, item, ppin, ppsni,  nth )
#else
int w_goto_nth_item( event, item, ppin, ppsni,  nth )
#endif
   char *event;
   char *item;
   struct item_node **ppin;
   struct same_name_item **ppsni;
   int *nth;
{
   char *rn="W_GOTO_NTH_ITEM";
   struct event_node *pen;
   struct item_node *pin;
   int ok;
   int i,j;

   pen = find_event_item_list(event);
   if (pen == NULL)
   {
      ok = get_items_for_event(event, YES);
      if (ok != OK)
      {
         printf("%s: cannot read items for event %s.\n", rn, event);
         return ERR;
      }
      pen = find_event_item_list(event);
      if (pen == NULL)
      {
         printf("%s: event doesn't exist in database: %s.\n", rn, event);
         return ERR;
      }
   }

   pin = NULL;
   i = *nth;
   j = 0;
   nth_tree_element(pen->head, &pin,  i, &j);
   if (pin == NULL) return ERR;

   *ppin = pin;
   *ppsni = pin->list;
   strncpy(item, pin->item_name, FORTRAN_ITEM_STR_LEN);
   return OK;
}

#ifdef USE_MACOSX
int w_get_event_name_list_(buf, dim, sz_e)
#else
int w_get_event_name_list(buf, dim, sz_e)
#endif
   char *buf;
   int dim;
   int sz_e;
{
   char *rn="W_GET_EVENT_NAME_LIST";
   struct event_node *pen;
   char *event="dummy";
   int i, j;
   int ok;

   if (head_event_list == NULL)
   {
      ok = get_items_for_event(event, YES);
      if (ok != OK)
      {
         printf("%s: cannot read items for event %s.\n", rn, event);
         return ERR;
      }
   }

   pen = head_event_list;
   for (i=0, j=0 ; pen != NULL && j < dim; pen = pen->next, i=i+sz_e, j++)
   {
      strncpy(&buf[i], pen->event_name, sz_e);
   }

   return OK;
}

#ifdef USE_MACOSX
int w_get_item_source_file_(event, filename)
#else
int w_get_item_source_file(event, filename)
#endif
   char *event;
   char *filename;
{
   struct event_node *p;

   if (NULL == (p = find_event_item_list(event))) return ERR;

   strcpy(filename, p->source_filename);
   return OK;
}
