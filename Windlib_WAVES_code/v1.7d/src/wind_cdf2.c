/* wind_cdf2.c - more cdf stuff
*/

#include <stdio.h>
#include <string.h>

#define MAX_PRIMARY 7
#define FIRST_OVERFLOW MAX_PRIMARY+1
#define MAX_OVERFLOW 7
#define TBL_SZ MAX_PRIMARY+MAX_OVERFLOW
#define MAX_FN_SZ 256			       /* maximum file name size */
#define my_w_cdf_null_val -1

struct w_cdf_file_hash_tbl {
   int val;
   int next;
   char f[MAX_FN_SZ]; /* file name with date resolved but wild card version */
   char g[MAX_FN_SZ]; /* resolved, useable filename */
};
static struct w_cdf_file_hash_tbl tbl[TBL_SZ];

static int n_in_q;
static int head;
static int tail;
struct w_cdf_file_hash_tbl_q {
   int primary;
   int idx;
};
static struct w_cdf_file_hash_tbl_q q[TBL_SZ];

#define hshfun(n) (n % MAX_PRIMARY)
#define hshsum(j,a,f)  for (j=0, a=f; *a > ' '; a++) j = j + *a;
#define qptradv(n) ((n+1) % TBL_SZ)

static void w_cdf_init_fn_hash_entry(thing)
   struct w_cdf_file_hash_tbl *thing;
{
   thing->val  = my_w_cdf_null_val;
   thing->next = my_w_cdf_null_val;
   thing->f[0] = '\0';
   thing->g[0] = '\0';
   return;
}

/* This queue implementation uses n_in_q to flag empty/full queue conditions.
   Variable "tail" points to the currently available slot.  Entries are
   added to the tail and removed from the head.
*/
static int w_cdf_fn_hash_enq(primary, idx)
   int *primary;
   int *idx;
{
   if (tail < 0) tail = 0;
   if (n_in_q==TBL_SZ) return 0;
   if (n_in_q==0) head = tail;
   q[tail].primary = *primary;
   q[tail].idx = *idx;
   tail = qptradv(tail);
   n_in_q++;
   return 1;
}

static int w_cdf_fn_hash_deq(primary, idx)
   int *primary;
   int *idx;
{
   if (n_in_q==0) return 0;
   *primary = q[head].primary;
   *idx = q[head].idx;
   head = qptradv(head);
   n_in_q--;
   return 1;
}

int w_cdf_fn_hash_deq_oldest(val)
   int *val;
{
   int i,j;
   int primary, overflow;
   int ok;

   ok = w_cdf_fn_hash_deq(&primary, &overflow);
   if (ok != 1) return 0;

   if (primary == overflow)
   {
      *val = tbl[primary].val;
      if (tbl[primary].next < 0)
      {
         i = primary;
      }
      else
      {
         i = tbl[primary].next;
         tbl[primary] = tbl[i];
      }
   }
   else
   {
      for (j=i=primary; i != overflow; j=i, i = tbl[i].next);
      *val = tbl[j].val;
      tbl[j] = tbl[i];
   }
   w_cdf_init_fn_hash_entry(&tbl[i]);

   return 1;
}

static int w_cdf_next_free_hash_slot(idx)
   int *idx;
{
   int i;

   for (i=FIRST_OVERFLOW-1; i < TBL_SZ; ++i)
      if (tbl[i].val == my_w_cdf_null_val) {*idx = i; return 1;}

   return 0;
}

int w_cdf_fn_hash_put(f, val, g)
   char *f;
   int *val;
   char *g;
{
   int i,j,k;
   int ok;
   int primary;
   char *a;

   hshsum(j,a,f);
   k = hshfun(j);
   primary = k;
   if (tbl[k].val != my_w_cdf_null_val)
   {
      for (i=primary; k > my_w_cdf_null_val; i=k, k=tbl[k].next);
      ok = w_cdf_next_free_hash_slot(&k);
      if (ok != 1) return 0;
      tbl[i].next = k;
   }

   ok = w_cdf_fn_hash_enq(&primary, &k);
   tbl[k].val = *val;
   strncpy(tbl[k].f, f, MAX_FN_SZ);
   strncpy(tbl[k].g, g, MAX_FN_SZ);

   return 1;
}

int w_cdf_fn_hash_get(f, val)
   char *f;
   int *val;
{
   int i,j,k;
   char *a;

   *val = my_w_cdf_null_val;
   hshsum(j,a,f);
   for (k=hshfun(j); k > my_w_cdf_null_val; k = tbl[k].next)
      if ( (strcmp(f,tbl[k].f) == 0))
      {
         *val = tbl[k].val;
         return 1;
      }
   return 0;
}

int w_cdf_fn_hash_clr()
{
   int i;

   n_in_q = 0;
   head = -1;
   tail = 0;
   for(i=0; i < TBL_SZ; i++)
   {
      q[i].primary = -1;
      q[i].idx     = -2;
      w_cdf_init_fn_hash_entry(&tbl[i]);
   }
   return 1;
}

static int show_table()
{
   int i;

   for (i=0; i<TBL_SZ; i++)
      printf("pos=%d. f=%s, val=%d, next=%d.\n", 
         i, tbl[i].f, tbl[i].val, tbl[i].next);

   return 1;
}

int w_cdf_get_fn_of_id(f, id)
   char *f;
   int *id;
{
   int i;

   for (i=0; i<TBL_SZ; i++)
   {
      if (*id == tbl[i].val)
      {
         strncpy(f, tbl[i].g, 80);
         return 1;
      }
   }
   return 0;
}

/*
main()
{
   char buf[] = "ABCDEFGHI";
   int i,j,k;
   int ok;

   ok = w_cdf_fn_hash_clr();
   for (i=0; i<16; i++)
   {
      printf("...putting #%d, %s.\n", i, buf);
      ok = w_cdf_fn_hash_put(buf, &i);
      if (ok != 1)
      {
         ok = w_cdf_fn_hash_deq_oldest(&j);
         if (ok != 1) printf("*** ERROR dequeueing!!!\n");
         else 
         {
         printf("*** dequeued, got val %d.\n", j);
         ok = w_cdf_fn_hash_put(buf, &i);
         printf("+++ 2nd hash put ok=%d.\n", ok);
         }
      }
      *buf = *buf + 1;
   }

   show_table();

   return 1;
}
*/
